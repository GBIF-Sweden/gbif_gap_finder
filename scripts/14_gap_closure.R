# scripts/14_gap_closure.R
# ============================================================================
# Difference two time points: what gaps closed, where, and by what mechanism
# ============================================================================
# Purpose:
#   Precompute every number the "Gaps filled" tab shows, so the app differences
#   nothing at runtime. Reads across time-point directories written by
#   run_timepoint.R (Stage 2) and the cubes written by 04b (Stage 1).
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
#   gbif    - specieskey. Not cross-regime safe, but it is what the Step 0 probe
#             measured, so it is the acceptance test that proves this code
#             correct - and it is the honest "all of GBIF" figure, since ~18% of
#             species carry no Dyntaxa taxonID.
#
# WHAT IS DELIBERATELY NOT COMPUTED:
#   A single continuous series across the snapshot -> cube boundary. That
#   boundary is a method change, not a measurement (different filters, different
#   backbone, different coordinate precision). `regime_boundary` is emitted in
#   closure_summary so the app can mark it; the line should be broken, not drawn.
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
#   - closure_mechanism_{res}_{from}_{to}.csv
#   - closure_group_{res}_{from}_{to}.csv
#   - closure_datasets_{res}_{from}_{to}.csv
#
# Usage:
#   Rscript -e 'source("scripts/14_gap_closure.R")'
#   GAP_CLOSURE_PAIR="2021-01-01,2024-01-01" Rscript -e 'source(...)'   # one pair
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

PLATFORMS <- as.character(cfg_get(
  "parameters.closure.source_groups.observation_platforms", character()))
FROZEN_Q  <- as.numeric(cfg_get("parameters.closure.frozen_quantile", 0.10))
# Groups losing at least this share of their baseline pairs are flagged as
# taxonomically volatile. 1% sits an order of magnitude above the stable clades
# (insects and birds run 0.15-0.2%) and well below the fungal orders (1.8-12%).
CHURN_WARN_PCT <- as.numeric(cfg_get("parameters.closure.churn_warn_pct", 1.0))
RESOLUTIONS <- c(10L, 50L)
KEY_SPACES  <- c("dyntaxa", "gbif")

# Regression fixtures, measured independently in numpy/pyproj over the same
# files (claude/finding-step0-probe-2026-09-09.md). A pair listed here MUST
# reproduce these in GBIF key space at 10 km. A mismatch is a bug in this
# script, not new data - the inputs are frozen snapshots.
CLOSURE_EXPECTED <- list(
  `2021-01-01|2024-01-01` = list(
    pairs_from = 5293352, pairs_to = 6128667,
    pairs_gained = 898644, pairs_lost = 63329,
    cells_filled = 336, cells_regressed = 16,
    species_newly_recorded = 9637, species_no_longer_recorded = 281
  )
)

cli_h1("Gap closure - {COUNTRY_CODE}")

if (!length(PLATFORMS)) {
  cli_alert_warning(
    "No {.field parameters.closure.source_groups.observation_platforms} in the \\
     config - every dataset will fall into {.val collections} and the source \\
     split will be meaningless."
  )
}

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
#' The live cube's download date, or NA - never today's date
#'
#' get_snapshot_date() falls back to Sys.Date() when the metadata carries no
#' cube date. That is right for staleness scoring and wrong here: it would
#' label the live time point with whatever day the script happened to run, and
#' the label is the time point's identity. This reads the same metadata and
#' returns NA rather than guessing.
live_timepoint_date <- function() {
  meta_path <- here(p_data_proc, "data_sources_meta.rds")
  if (!file.exists(meta_path)) return(NA_character_)
  meta  <- tryCatch(readRDS(meta_path), error = function(e) NULL)
  cubes <- meta$cubes
  if (is.null(cubes) || !length(cubes)) return(NA_character_)
  d <- tryCatch(
    do.call(c, lapply(cubes, function(x) {
      v <- x$created
      if (is.null(v) || all(is.na(v))) as.Date(NA) else as.Date(v)
    })),
    error = function(e) as.Date(NA))
  d <- d[!is.na(d)]
  if (!length(d)) NA_character_ else as.character(max(d))
}

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
  # GAP_CLOSURE_PAIR fails to match, and the previous version printed nothing
  # at all when the date could not be resolved - three states, two branches.
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

#' Where does this time point's data live: under timepoints/, or at the root?
tp_is_live <- function(tp) live_ok && identical(tp, LIVE_TP)
tp_cubes_dir <- function(tp) if (tp_is_live(tp)) here(p_cubes) else file.path(tp_root, tp, "cubes")
tp_gaps_dir  <- function(tp) if (tp_is_live(tp)) here(p_gaps)  else file.path(tp_root, tp, "gaps")

if (length(timepoints) < 2L) {
  cli_abort(c(
    "Need at least two time points to difference; found \\
     {length(timepoints)} ({paste(timepoints, collapse = ', ')}).",
    "i" = "Run {.code Rscript run_timepoint.R <date>} for each."
  ))
}
cli_alert_success("Time points: {paste(timepoints, collapse = ', ')}")

#' Consecutive pairs, unless one is named explicitly
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
  pairs_to_do <- lapply(seq_len(length(timepoints) - 1L),
                        function(i) timepoints[c(i, i + 1L)])
}

#' The pinned checklist must be the same at both ends, or the difference is
#' partly taxonomy churn rather than data. run_timepoint.R records it; this
#' turns that record into a check.
reference_release <- function(tp) {
  # The live point has no reference_version.yml - run_timepoint.R writes those,
  # and the live pipeline is driven by targets. Its equivalent is the tracked
  # provenance file that 01b rewrites on every run, which is where
  # run_timepoint.R copies from in the first place.
  f <- if (tp_is_live(tp)) here("provenance", glue("upstream_versions_{COUNTRY_CODE}.yml"))
       else file.path(tp_root, tp, "reference_version.yml")
  if (!file.exists(f)) return(NA_character_)
  r <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
  if (is.null(r$taxonomy$published)) NA_character_ else as.character(r$taxonomy$published)
}

# ============================================================================
# Readers
# ============================================================================

#' Reduce one cube to the closure grain
#'
#' One row per (specieskey, eeacellcode, datasetkey) with min_year, occurrences
#' and record count. Only five columns are read: the cube is ~42 M rows and the
#' other twelve are not needed here.
read_grain <- function(tp, res_km) {
  path <- file.path(tp_cubes_dir(tp), sprintf("cube_%dkm.parquet", res_km))
  if (!file.exists(path)) {
    cli_abort(c("Missing cube: {.path {path}}",
                "i" = "Run {.code scripts/04b_build_historical_cubes.R} first."))
  }
  dt <- data.table::as.data.table(arrow::read_parquet(
    path, col_select = c("specieskey", "eeacellcode", "datasetkey",
                         "year", "occurrences")))
  # The aggregation lives in R/closure.R so it can be unit-tested; doing it
  # inline here is what let an integer/Inf type mismatch reach the real data.
  g <- closure_build_grain(dt, PLATFORMS)
  rm(dt); invisible(gc())
  cli_alert_info(
    "{tp} {res_km} km: {scales::comma(nrow(g))} (species x cell x dataset) rows, \\
     {scales::comma(sum(g$occ))} occurrences"
  )
  g[]
}

#' specieskey -> Dyntaxa taxonID and the reconciled taxonomy, for one time point
read_match_table <- function(tp) {
  f <- file.path(tp_gaps_dir(tp), "taxonomic_match_table.csv")
  if (!file.exists(f)) {
    cli_abort(c("Missing match table for {tp}: {.path {f}}",
                "i" = "Run {.code Rscript run_timepoint.R {tp}} first (script 09a)."))
  }
  m <- data.table::fread(f, showProgress = FALSE)
  # Fail here, by name, rather than 20 minutes later inside a data.table join.
  # `class` and `order` drive the taxonomic panel; `backbone_taxonID` is the
  # entire Dyntaxa key space.
  need <- c("specieskey", "backbone_taxonID", "class", "order")
  absent <- setdiff(need, names(m))
  if (length(absent)) {
    cli_abort(c(
      "Match table for {tp} is missing column{?s}: {paste(absent, collapse = ', ')}",
      "i" = "Header found: {paste(names(m), collapse = ', ')}",
      "x" = "Script 09a's output shape has changed; closure cannot be computed \\
             against it without checking what moved."
    ))
  }
  m[, specieskey := as.character(specieskey)]
  if ("backbone_taxonID" %in% names(m)) m[, backbone_taxonID := as.character(backbone_taxonID)]
  m
}

#' Full cell universe for a resolution - from the grid, never from the data
grid_cellcodes <- function(res_km) {
  f <- here(p_data_proc, sprintf("cellcodes_%dkm.txt", res_km))
  if (!file.exists(f)) cli_abort(c("Missing {.path {f}}", "i" = "Run script 02."))
  unique(readLines(f, warn = FALSE))
}

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

  # The snapshot regime runs to 2024-01-01; anything later comes from a live
  # cube and is a different measurement.
  # A pair straddles the regime boundary when exactly one end is the live cube.
  # Defined by WHAT the time point is, not by a hard-coded date, so a snapshot
  # delivered in 2027 stays a snapshot and a second cube stays a cube.
  regime_boundary <- xor(tp_is_live(from), tp_is_live(to))
  ks_for_pair <- KEY_SPACES

  if (regime_boundary) {
    # GBIF key space CANNOT cross this boundary and must not be attempted.
    # The snapshots key on legacy Backbone nub integers ("8128385"); the cube
    # keys on Catalogue of Life ids ("4Z659"). The two vocabularies do not
    # overlap, so a set difference would report every baseline pair as lost and
    # every comparison pair as gained - roughly six million of each, all of it
    # fiction, and none of it erroring. Dyntaxa taxonID is the only key both
    # sides reach, which is the entire argument of the harmonisation brief.
    ks_for_pair <- "dyntaxa"
    cli_alert_warning(c(
      "This pair straddles the snapshot -> cube boundary."
    ))
    cli_alert_info("GBIF key space skipped: legacy nub ids and COL ids share no vocabulary")
    cli_alert_info(
      "10 km is unreliable here: snapshot cells come from 2 dp coordinates and \\
       cube cells from full precision, so ~4% of records sit one cell away \\
       (~0.9% at 50 km). Treat 50 km as the headline across this pair."
    )
    cli_alert_info(
      "Filters differ too: the cube applies hasgeospatialissues = FALSE and \\
       occurrencestatus = 'PRESENT'; the snapshots cannot. Residuals are small \\
       but they are not zero."
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

      cells <- closure_cells(closure_cell_view(gf), closure_cell_view(gt), all_cells)
      cells <- closure_frozen_low(cells, q = FROZEN_Q)
      species <- closure_species(pairs_from, pairs_to)
      mech    <- closure_mechanism(gained, gt, baseline_year)
      dsconc  <- closure_dataset_concentration(gained, gt)

      # Taxonomic roll-up: class/order come from the reconciled match table, not
      # from the cube, so both key spaces are described by the same taxonomy.
      tax_cols <- intersect(c("class", "order", "backbone_scientificName",
                              "taxonRank", "threatStatus_redlist",
                              "threatStatus_backbone"), names(mt_to))
      lk_key <- if (ks == "gbif") "specieskey" else "backbone_taxonID"
      lk <- unique(mt_to[, c(lk_key, tax_cols), with = FALSE])
      data.table::setnames(lk, lk_key, "key")
      lk <- unique(lk, by = "key")

      sp_out <- merge(species, lk, by = "key", all.x = TRUE)
      grp <- merge(species, lk[, .(key, class, order)], by = "key", all.x = TRUE)[
        , .(pairs_from = sum(cells_from), pairs_to = sum(cells_to),
            pairs_gained = sum(cells_gained), pairs_lost = sum(cells_lost),
            n_species = .N),
        by = .(class, order)]
      grp[, fill_rate := round(
        data.table::fifelse(pairs_from > 0, 100 * pairs_gained / pairs_from, NA_real_), 3)]
      # Loss rate is the taxonomic-churn indicator, and it belongs beside fill
      # rate rather than in a caveat. Backbone reassignment inside a clade shows
      # up as pairs vanishing from the baseline: Agaricales loses 4.3% and
      # Boletales 12.2% of their baseline pairs between 2021 and 2024, against
      # 0.19% for Coleoptera and 0.15% for Odonata. A reader ranking groups by
      # fill rate alone cannot tell a volatile clade from a stable one.
      # See claude/finding-backbone-churn-within-regime-2026-09-10.md.
      grp[, loss_rate := round(
        data.table::fifelse(pairs_from > 0, 100 * pairs_lost / pairs_from, NA_real_), 3)]
      grp[, churn_flag := !is.na(loss_rate) & loss_rate >= CHURN_WARN_PCT]
      data.table::setorder(grp, -pairs_gained)

      summ <- closure_summary(pairs_from, pairs_to, cells, species, mech,
                              key_space = ks, baseline_year = baseline_year,
                              regime_boundary = regime_boundary)
      # The national churn floor. Read it as a floor, not an estimate: a pair
      # that VANISHES because a name moved is visible, but a pair that APPEARS
      # for the same reason looks exactly like new data.
      summ <- rbind(summ, data.table::data.table(
        metric = c("taxonomic_churn_floor_pct", "groups_flagged_volatile",
                   # 1 when this resolution's cells are not comparable across the
                   # pair: 2 dp snapshot coordinates against full-precision cube
                   # coordinates move ~4% of records one cell at 10 km.
                   "coordinate_precision_unreliable",
                   # 1 when the two ends were built with different occurrence
                   # filters (the cube has hasgeospatialissues/occurrencestatus).
                   "filters_differ"),
        source_group = "total", key_space = ks,
        value = c(round(100 * nrow(data.table::fsetdiff(pairs_from, pairs_to)) /
                          max(nrow(pairs_from), 1), 3),
                  sum(grp$churn_flag, na.rm = TRUE),
                  as.integer(regime_boundary && res_km == 10L),
                  as.integer(regime_boundary))), fill = TRUE)
      summ[, resolution := sprintf("%dkm", res_km)]
      summaries[[tag]] <- summ

      stem <- sprintf("_%s_%s_%s.csv", tag, from, to)
      data.table::fwrite(cells,  file.path(closure_dir, paste0("closure_cells",     stem)))
      data.table::fwrite(sp_out, file.path(closure_dir, paste0("closure_species",   stem)))
      data.table::fwrite(mech,   file.path(closure_dir, paste0("closure_mechanism", stem)))
      data.table::fwrite(grp,    file.path(closure_dir, paste0("closure_group",     stem)))
      data.table::fwrite(dsconc, file.path(closure_dir, paste0("closure_datasets",  stem)))

      cli_alert_success(
        "{tag}: {scales::comma(nrow(gained))} pairs gained, \\
         {scales::comma(nrow(data.table::fsetdiff(pairs_from, pairs_to)))} lost, \\
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
            "Closure {from} -> {to} does not reproduce the Step 0 probe.",
            "x" = "{paste(sprintf('%s: expected %s, got %s', bad,
                                  format(unlist(e[bad]), big.mark = ','),
                                  format(got[bad], big.mark = ',')), collapse = '; ')}",
            "i" = "The inputs are frozen snapshots, so this is a bug here, not new \\
                   data. See claude/finding-step0-probe-2026-09-09.md."
          ))
        }
        cli_alert_success(
          "Regression fixture: reproduces all {length(e)} Step 0 probe numbers"
        )
      }
    }
    rm(raw_from, raw_to); invisible(gc())
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
cli_alert_info("Next: Step 4 - add these to the bundle in 11 and build the tab.")
