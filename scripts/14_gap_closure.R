# scripts/14_gap_closure.R
# ============================================================================
# Difference two time points: what gaps closed, where, and by what mechanism
# ============================================================================
# Purpose:
#   Precompute every number the "Gaps filled" tab shows, so the app differences
#   nothing at runtime. Reads across time-point directories written by
#   run_timepoint.R and the cubes written by 04b.
#
#   The arithmetic lives in R/closure.R and is unit-tested by
#   tools/test_closure.R. This script is I/O, configuration and assertions.
#
# RUN IT WITH GAP_FINDER_TIMEPOINT UNSET. 14 spans time points; it is not a
#   per-time-point script, and a set env var would send its output into one
#   time point's directory. Asserted below rather than assumed.
#
# TWO KEY SPACES (see R/closure.R for the full argument):
#   dyntaxa - backbone_taxonID. The headline: a national checklist does not move
#             when GBIF changes its backbone, so it is the only space in which a
#             legacy-Backbone snapshot and a COL cube can be differenced.
#   gbif    - specieskey. Only between time points with the same key_backbone
#             (legacy nub keys and COL ids share no vocabulary), but it is what
#             the regression fixture measured, so it is the acceptance test that proves this
#             code correct - and it is the honest "all of GBIF" figure, since
#             ~18% of species carry no Dyntaxa taxonID.
#
# WHAT IS DELIBERATELY NOT COMPUTED:
#   A single continuous series across the snapshot -> cube boundary. That
#   boundary is a method change, not a measurement (different filters,
#   different coordinate precision). `regime_boundary` is emitted in
#   closure_summary so the app can mark it; the line should be broken, not drawn.
#   A snapshot and a cube of (nearly) the same date are the BRIDGE, not a
#   closure pair: script 15 measures the method gap from them.
#
# TIME-POINT METADATA (reference_version.yml, written by run_timepoint.R):
#   regime        snapshot | cube
#   key_backbone  legacy | col
#   The default pairs are within-regime (closure_default_pairs()); a
#   cross-regime pair runs only when named in GAP_CLOSURE_PAIR.
#
# Inputs (per time point tp):
#   - data/{CC}/proc/timepoints/{tp}/cubes/cube_{10,50}km.parquet   (04b)
#   - data/{CC}/proc/timepoints/{tp}/gaps/taxonomic_match_table.csv (09a)
#   - data/{CC}/proc/timepoints/{tp}/reference_version.yml          (run_timepoint)
#   - data/{CC}/proc/cellcodes_{10,50}km.txt                        (02, shared)
# Outputs (data/{CC}/proc/closure/):
#   - closure_summary_{from}_{to}.csv
#   - closure_cells_{res}_{from}_{to}.csv
#   - closure_species_{res}_{from}_{to}.csv
#   - closure_mechanism_{res}_{from}_{to}.csv        (periods per source type)
#   - closure_mechanism_group_{res}_{from}_{to}.csv  (the same per class x order)
#   - closure_cells_group_{res}_{from}_{to}.csv      (pairs per cell x class x order)
#   - closure_group_{res}_{from}_{to}.csv
#   - closure_datasets_{res}_{from}_{to}.csv
#   - closure_datasets_lost_{res}_{from}_{to}.csv
#
# Usage:
#   Rscript -e 'source("scripts/14_gap_closure.R")'
#   GAP_CLOSURE_PAIR="2021-01-01,2024-01-01" Rscript -e 'source(...)'   # one pair
#   GAP_CLOSURE_PAIR="2024-01-01,2026-10-01" Rscript -e 'source(...)'   # snapshot series
#   GAP_CLOSURE_PAIR="2024-01-01,2026-07-29" Rscript -e 'source(...)'   # cross-regime
#
# TIME POINTS = the directories under proc/timepoints/ PLUS the live cube at the
#   root. The live cube is not copied into timepoints/; it is labelled with its
#   download date (from data_sources_meta.rds) and its paths resolved to
#   proc/cubes/ and proc/gaps/. So the third time point above appears without
#   any extra run of run_timepoint.R.
#
# Dependencies: scripts/00_setup.R, data.table, arrow, R/closure.R
# ============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("R", "closure.R"))
source(here::here("R", "timepoint_io.R"))

if (!requireNamespace("arrow", quietly = TRUE)) {
  cli_abort(c("Package {.pkg arrow} is required",
              "i" = "Install: {.code install.packages('arrow')}"))
}
library(arrow)

if (nzchar(Sys.getenv("GAP_FINDER_TIMEPOINT", ""))) {
  cli_abort(c(
    "GAP_FINDER_TIMEPOINT is set ({Sys.getenv('GAP_FINDER_TIMEPOINT')}).",
    "x" = "Script 14 differences time points; it must not run inside one, or its \\
           output lands in that time point's directory.",
    "i" = "Unset it: {.code Sys.unsetenv('GAP_FINDER_TIMEPOINT')}"
  ))
}

# ============================================================================
# Configuration
# ============================================================================

tp_root     <- here(p_data_proc, "timepoints")
closure_dir <- here(p_data_proc, "closure")
if (!dir.exists(closure_dir)) dir.create(closure_dir, recursive = TRUE, showWarnings = FALSE)

# The source type of every dataset (platform / monitoring / collection /
# other). A dataset missing from the list counts as "other" and is named when
# its time point is read.
SOURCE_TYPES <- closure_read_types(here(cfg_get(
  "parameters.closure.source_types_file", glue("configs/dataset_types_{COUNTRY_CODE}.csv"))))
FROZEN_Q  <- as.numeric(cfg_get("parameters.closure.frozen_quantile", 0.10))
RESOLUTIONS <- c(10L, 50L)
KEY_SPACES  <- c("dyntaxa", "gbif")

# Regression fixtures, measured independently in numpy/pyproj over the same
# files. A pair listed here MUST reproduce these in GBIF key space at 10 km.
# A mismatch is a bug in this script, not new data - the inputs are frozen
# snapshots.
CLOSURE_EXPECTED <- list(
  `2021-01-01|2024-01-01` = list(
    pairs_from = 5293352, pairs_to = 6128667,
    pairs_gained = 898644, pairs_lost = 63329,
    cells_filled = 336, cells_regressed = 16,
    species_newly_recorded = 9637, species_no_longer_recorded = 281
  )
)

cli_h1("Gap closure - {COUNTRY_CODE}")

n_types <- table(factor(SOURCE_TYPES, levels = CLOSURE_SOURCE_TYPES))
cli_alert_info(
  "Source types: {paste(sprintf('%s %s', names(n_types), scales::comma(as.integer(n_types))), collapse = ', ')} datasets")

# ============================================================================
# Which time points, and are they comparable?
# ============================================================================

timepoints <- sort(basename(list.dirs(tp_root, recursive = FALSE)))
timepoints <- timepoints[vapply(
  timepoints,
  function(tp) file.exists(file.path(tp_root, tp, "cubes", "cube_10km.parquet")),
  logical(1))]

# ---------------------------------------------------------------------------
# THE LIVE CUBE IS A TIME POINT ALREADY. It does not need copying or re-running.
# With GAP_FINDER_TIMEPOINT unset, p_timepoint == p_data_proc, so the live
# pipeline's own outputs - proc/cubes/ and proc/gaps/taxonomic_match_table.csv -
# ARE this time point's cube and match table. They are simply at the root rather
# than under timepoints/. Labelling them with the cube's download date and
# resolving paths accordingly is the whole of the integration.
LIVE_TP    <- live_timepoint_date()
live_cube  <- here(p_cubes, "cube_10km.parquet")
live_match <- here(p_gaps, "taxonomic_match_table.csv")
live_ok    <- !is.na(LIVE_TP) && file.exists(live_cube) && file.exists(live_match)

if (live_ok) {
  timepoints <- sort(unique(c(timepoints, LIVE_TP)))
  cli_alert_info(
    "Live cube included as time point {.val {LIVE_TP}} (read from the root, not copied)")
} else {
  # NEVER stay silent here. A missing live point is the most likely reason
  # GAP_CLOSURE_PAIR fails to match, so name every cause that applies.
  cli_alert_warning(
    "Live cube NOT available as a time point - only the snapshot time points \\
     below can be compared.")
  if (is.na(LIVE_TP)) {
    cli_bullets(c("x" = "no cube download date in {.path {here(p_data_proc, 'data_sources_meta.rds')}}"))
  } else {
    cli_bullets(c("v" = "cube download date {.val {LIVE_TP}}"))
  }
  if (!file.exists(live_cube))  cli_bullets(c("x" = "missing {.path {live_cube}}"))
  if (!file.exists(live_match)) cli_bullets(c("x" = "missing {.path {live_match}}"))
  cli_bullets(c(
    "i" = "Fix by running the live pipeline at the repo root with \\
           {.envvar GAP_FINDER_TIMEPOINT} unset: {.code tar_make()}, then re-run this script."
  ))
}

if (length(timepoints) < 2L) {
  cli_abort(c(
    "Need at least two time points to difference; found \\
     {length(timepoints)} ({paste(timepoints, collapse = ', ')}).",
    "i" = "Run {.code Rscript run_timepoint.R <date>} for each."
  ))
}
cli_alert_success("Time points: {paste(timepoints, collapse = ', ')}")

TP_META <- stats::setNames(lapply(timepoints, tp_meta), timepoints)
meta_dt <- data.table::rbindlist(lapply(TP_META, as.data.frame))
print(meta_dt)

#' Within-regime pairs by default, unless one is named explicitly
pair_spec <- Sys.getenv("GAP_CLOSURE_PAIR", "")
if (nzchar(pair_spec)) {
  p <- trimws(strsplit(pair_spec, ",")[[1]])
  if (length(p) != 2L || !all(p %in% timepoints)) {
    cli_abort(c(
      "GAP_CLOSURE_PAIR must name two existing time points, comma separated.",
      "x" = "Not found: {.val {setdiff(p, timepoints)}}",
      "i" = "Available: {.val {timepoints}}"
    ))
  }
  pairs_to_do <- list(p)
} else {
  pairs_to_do <- closure_default_pairs(meta_dt)
  if (!length(pairs_to_do)) {
    cli_abort(c(
      "No regime has two time points, so there is no default pair.",
      "i" = "Name a cross-regime pair explicitly with {.envvar GAP_CLOSURE_PAIR}."
    ))
  }
}
cli_alert_info("Pairs: {paste(vapply(pairs_to_do, paste, character(1), collapse = ' -> '), collapse = ' | ')}")

# ============================================================================
# Difference each pair
# ============================================================================

for (pr in pairs_to_do) {
  from <- pr[1]; to <- pr[2]
  cli_h2("{from} -> {to}")

  rel_from <- reference_release(from); rel_to <- reference_release(to)
  if (is.na(rel_from) || is.na(rel_to)) {
    cli_alert_warning(
      "No pinned reference release for {if (is.na(rel_from)) from else to} - \\
       cannot prove the checklist held still across this pair."
    )
  } else if (!identical(rel_from, rel_to)) {
    cli_abort(c(
      "The national checklist moved between {from} ({rel_from}) and {to} ({rel_to}).",
      "x" = "Differencing across a checklist change reports taxonomy churn as \\
             gap closure.",
      "i" = "Re-run both time points against one Dyntaxa release, or difference \\
             a pair that shares one."
    ))
  } else {
    cli_alert_success("Both time points pinned to Dyntaxa {rel_from}")
  }

  # Defined by WHAT each time point is (reference_version.yml), not by a date,
  # so a later snapshot stays a snapshot and a second cube stays a cube.
  kind <- closure_pair_kind(TP_META[[from]], TP_META[[to]])
  if (kind$is_bridge) {
    cli_abort(c(
      "{from} ({TP_META[[from]]$regime}) and {to} ({TP_META[[to]]$regime}) are \\
       the same moment measured two ways.",
      "x" = "Their difference is method, not change - it is not a closure pair.",
      "i" = "Run {.code scripts/15_bridge.R} for this pair instead."
    ))
  }
  regime_boundary <- kind$regime_boundary
  ks_for_pair <- intersect(KEY_SPACES, kind$key_spaces)

  if (kind$key_backbone_differs) {
    # Legacy nub ids ("8128385") and COL ids ("4Z659") do not overlap, so a set
    # difference in GBIF space would report every baseline pair as lost and
    # every comparison pair as gained, without erroring. Dyntaxa taxonID is the
    # only key both sides reach.
    cli_alert_info(
      "GBIF key space skipped: {from} keys are {TP_META[[from]]$key_backbone}, \\
       {to} keys are {TP_META[[to]]$key_backbone}. Dyntaxa space still mixes the \\
       two name paths: taxa whose names differ between them appear lost or new.")
  }

  if (regime_boundary) {
    cli_alert_warning(c(
      "This pair straddles the snapshot -> cube boundary."
    ))
    cli_alert_info(
      "10 km is unreliable here: snapshot cells come from 2 dp coordinates and \\
       cube cells from full precision, so ~4% of records sit one cell away \\
       (~0.9% at 50 km). Treat 50 km as the headline across this pair."
    )
    cli_alert_info(
      "Filters differ too: the cube applies hasgeospatialissues = FALSE and \\
       occurrencestatus IN ('DETECTED', 'PRESENT'); the snapshots cannot. \\
       Residuals are small but they are not zero."
    )
  }
  baseline_year <- as.integer(format(as.Date(from), "%Y"))

  mt_from <- read_match_table(from)
  mt_to   <- read_match_table(to)

  summaries <- list()

  for (res_km in RESOLUTIONS) {
    all_cells <- grid_cellcodes(res_km)
    raw_from  <- read_grain(from, res_km)
    raw_to    <- read_grain(to,   res_km)

    for (ks in ks_for_pair) {
      tag <- sprintf("%dkm_%s", res_km, ks)
      rf <- closure_rekey(raw_from, mt_from, ks)
      rt <- closure_rekey(raw_to,   mt_to,   ks)
      # The GBIF-key grains are read again only by a later key space; freeing
      # them here keeps the peak memory of a Dyntaxa-only pair down.
      if (identical(ks, utils::tail(ks_for_pair, 1L))) {
        rm(raw_from, raw_to)
        invisible(gc())
      }
      gf <- rf$grain; gt <- rt$grain
      if (ks == "dyntaxa") {
        cli_alert_info(
          "{tag}: {scales::comma(rf$n_dropped)} / {scales::comma(rt$n_dropped)} \\
           species (from/to) carry no Dyntaxa taxonID and are excluded"
        )
      }

      pairs_from <- closure_pairs(gf)
      pairs_to   <- closure_pairs(gt)
      gained     <- data.table::fsetdiff(pairs_to, pairs_from)
      lost       <- data.table::fsetdiff(pairs_from, pairs_to)

      # The cell table covers the grid only. The grid follows the live cube's
      # cells, so a time point can hold a cell the current grid lacks; its
      # records still count in the pair totals, so it is named here and counted
      # in the summary.
      cv_from  <- closure_cell_view(gf)
      cv_to    <- closure_cell_view(gt)
      off_from <- cv_from[!eeacellcode %chin% all_cells]
      off_to   <- cv_to[!eeacellcode %chin% all_cells]
      if (nrow(off_from) || nrow(off_to)) {
        cli_alert_warning(
          "{tag}: cells outside the {res_km} km grid are left out of the cell table \\
           ({from}: {nrow(off_from)}, {scales::comma(sum(off_from$occ))} records; \\
           {to}: {nrow(off_to)}, {scales::comma(sum(off_to$occ))} records): \\
           {.val {utils::head(unique(c(off_from$eeacellcode, off_to$eeacellcode)), 10)}}")
      }

      cells <- closure_cells(cv_from, cv_to, all_cells)
      cells <- closure_frozen_low(cells, q = FROZEN_Q)
      species <- closure_species(pairs_from, pairs_to)
      mech    <- closure_mechanism(gained, gt, baseline_year)
      dsconc  <- closure_dataset_concentration(gained, gt)
      # The losses the same way, read from the baseline's records: which
      # datasets the vanished pairs rested on, and how many rested on one alone.
      dsloss  <- closure_dataset_concentration(lost, gf)

      # Taxonomic roll-up: class/order come from the reconciled match table, not
      # from the cube, so both key spaces are described by the same taxonomy.
      # The later time point wins for any taxon present at both ends; the
      # earlier one is consulted only for taxa that are GONE by `to`, which
      # would otherwise be unnamed and pile up in a single (NA, NA) group at
      # 100% loss. See closure_taxon_lookup() in R/closure.R.
      tax_cols <- intersect(c("class", "order", "backbone_scientificName",
                              "taxonRank", "threatStatus_redlist",
                              "threatStatus_backbone"), names(mt_to))
      lk_key <- if (ks == "gbif") "specieskey" else "backbone_taxonID"
      tl <- closure_taxon_lookup(mt_to, mt_from, tax_cols, lk_key)
      lk <- tl$lookup
      if (tl$n_fallback) {
        # cli::qty() supplies the pluralisation quantity explicitly. Without it
        # the quantity comes from the preceding substitution - scales::comma(),
        # a CHARACTER - and cli then reads it as singular forever (cli 3.6.2).
        cli_alert_info(
          "{tag}: {scales::comma(tl$n_fallback)} key{cli::qty(tl$n_fallback)}{?s} \\
           absent from the {to} taxonomy, annotated from {from} (the lost taxa)")
      }

      sp_out <- merge(species, lk, by = "key", all.x = TRUE)
      grp <- merge(species, lk[, .(key, class, order)], by = "key", all.x = TRUE)[
        , .(pairs_from = sum(cells_from), pairs_to = sum(cells_to),
            pairs_gained = sum(cells_gained), pairs_lost = sum(cells_lost),
            n_species = .N),
        by = .(class, order)]
      grp[, fill_rate := round(
        data.table::fifelse(pairs_from > 0, 100 * pairs_gained / pairs_from, NA_real_), 3)]
      # Loss rate belongs beside fill rate rather than in a caveat: records
      # re-identified between species, and names that moved, show up as pairs
      # vanishing from a group's baseline.
      grp[, loss_rate := round(
        data.table::fifelse(pairs_from > 0, 100 * pairs_lost / pairs_from, NA_real_), 3)]
      data.table::setorder(grp, -pairs_gained)

      # The tab's taxonomic filter reads these: periods and cells per
      # class x order. Dyntaxa key space only, the tab's.
      if (ks == "dyntaxa") {
        mech_grp  <- closure_mechanism(gained, gt, baseline_year, taxa = lk)
        cells_grp <- closure_cells_by_group(pairs_from, pairs_to, gained, lost, lk)
      }

      summ <- closure_summary(pairs_from, pairs_to, cells, species, mech,
                              key_space = ks, baseline_year = baseline_year,
                              regime_boundary = regime_boundary)
      # The national churn floor. Read it as a floor, not an estimate: a pair
      # that VANISHES because a name moved is visible, but a pair that APPEARS
      # for the same reason looks exactly like new data.
      summ <- rbind(summ, data.table::data.table(
        metric = c("taxonomic_churn_floor_pct",
                   # 1 when this resolution's cells are not comparable across the
                   # pair: 2 dp snapshot coordinates against full-precision cube
                   # coordinates move ~4% of records one cell at 10 km.
                   "coordinate_precision_unreliable",
                   # 1 when the two ends were built with different occurrence
                   # filters (the cube has hasgeospatialissues/occurrencestatus).
                   "filters_differ",
                   # 1 when the two ends key species in different backbones
                   # (legacy nub vs COL): GBIF space is skipped, and Dyntaxa
                   # space carries the name-path asymmetry.
                   "key_backbone_differs",
                   # Species excluded by the re-key because they carry no
                   # national-checklist taxonID. The app caveats its species
                   # tile with them: "newly recorded" in Dyntaxa space is only
                   # interpretable next to the species that are off the
                   # checklist entirely. In gbif space nothing is dropped
                   # and both are 0 by construction.
                   "species_off_checklist_from", "species_off_checklist_to",
                   # Cells with records outside the grid: in the pair totals,
                   # not in the cell counts.
                   "cells_off_grid_from", "cells_off_grid_to"),
        source_group = "total", key_space = ks,
        value = c(round(100 * nrow(lost) / max(nrow(pairs_from), 1), 3),
                  as.integer(regime_boundary && res_km == 10L),
                  as.integer(regime_boundary),
                  as.integer(kind$key_backbone_differs),
                  rf$n_dropped, rt$n_dropped,
                  nrow(off_from), nrow(off_to))), fill = TRUE)
      summ[, resolution := sprintf("%dkm", res_km)]
      summaries[[tag]] <- summ

      stem <- sprintf("_%s_%s_%s.csv", tag, from, to)
      data.table::fwrite(cells,  file.path(closure_dir, paste0("closure_cells",     stem)))
      data.table::fwrite(sp_out, file.path(closure_dir, paste0("closure_species",   stem)))
      data.table::fwrite(mech,   file.path(closure_dir, paste0("closure_mechanism", stem)))
      data.table::fwrite(grp,    file.path(closure_dir, paste0("closure_group",     stem)))
      data.table::fwrite(dsconc, file.path(closure_dir, paste0("closure_datasets",  stem)))
      data.table::fwrite(dsloss, file.path(closure_dir, paste0("closure_datasets_lost", stem)))
      if (ks == "dyntaxa") {
        data.table::fwrite(mech_grp,  file.path(closure_dir, paste0("closure_mechanism_group", stem)))
        data.table::fwrite(cells_grp, file.path(closure_dir, paste0("closure_cells_group", stem)))
        rm(mech_grp, cells_grp)
        invisible(gc())
      }

      cli_alert_success(
        "{tag}: {scales::comma(nrow(gained))} pairs gained, \\
         {scales::comma(nrow(lost))} lost, \\
         {nrow(cells[status == 'filled'])} cells filled"
      )

      # ---- regression fixture, GBIF space at 10 km ----------------------
      exp_key <- paste(from, to, sep = "|")
      if (ks == "gbif" && res_km == 10L && !is.null(CLOSURE_EXPECTED[[exp_key]])) {
        e   <- CLOSURE_EXPECTED[[exp_key]]
        tot <- summ[source_group == "total"]
        got <- vapply(names(e),
                      function(m) as.numeric(tot[metric == m, value][1]),
                      numeric(1))
        bad <- names(e)[!vapply(names(e),
                                function(m) isTRUE(all.equal(as.numeric(e[[m]]),
                                                             unname(got[[m]]))),
                                logical(1))]
        if (length(bad)) {
          cli_abort(c(
            "Closure {from} -> {to} does not reproduce the regression fixture.",
            "x" = "{paste(sprintf('%s: expected %s, got %s', bad,
                                  format(unlist(e[bad]), big.mark = ','),
                                  format(got[bad], big.mark = ',')), collapse = '; ')}",
            "i" = "The inputs are frozen snapshots, so this is a bug here, not new \\
                   data."
          ))
        }
        cli_alert_success(
          "Regression fixture: reproduces all {length(e)} expected numbers"
        )
      }
    }
    invisible(gc())
  }

  all_summ <- data.table::rbindlist(summaries, fill = TRUE)
  data.table::setcolorder(all_summ, c("metric", "resolution", "key_space",
                                      "source_group", "value"))
  out <- file.path(closure_dir, sprintf("closure_summary_%s_%s.csv", from, to))
  data.table::fwrite(all_summ, out)
  cli_alert_success("{.path {basename(out)}}")
}

cli_h2("Done")
cli_alert_info("Closure tables: {.path {closure_dir}}")
cli_alert_info("Next: re-run scripts/11_prepare_gap_finder_data.R to add these to the bundle.")
