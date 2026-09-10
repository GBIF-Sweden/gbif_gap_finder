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
    cli_abort("GAP_CLOSURE_PAIR must name two existing time points, comma separated.")
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
  f <- file.path(tp_root, tp, "reference_version.yml")
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
  path <- file.path(tp_root, tp, "cubes", sprintf("cube_%dkm.parquet", res_km))
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
  f <- file.path(tp_root, tp, "gaps", "taxonomic_match_table.csv")
  if (!file.exists(f)) {
    cli_abort(c("Missing match table for {tp}: {.path {f}}",
                "i" = "Run {.code Rscript run_timepoint.R {tp}} first (script 09a)."))
  }
  m <- data.table::fread(f, showProgress = FALSE)
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

#' Re-key a grain into one of the two key spaces
#'
#' @return list(grain = re-keyed grain, n_dropped = keys with no taxonID)
rekey_grain <- function(grain, match_tbl, key_space) {
  g <- data.table::copy(grain)
  if (identical(key_space, "gbif")) {
    g[, key := as.character(specieskey)]
    return(list(grain = g[, .(key, eeacellcode, datasetkey, source_group,
                              min_year, occ, n_rec)], n_dropped = 0L))
  }
  lk <- unique(match_tbl[!is.na(backbone_taxonID) & nzchar(backbone_taxonID),
                         .(specieskey, key = backbone_taxonID)])
  g[, specieskey := as.character(specieskey)]
  before <- data.table::uniqueN(g$specieskey)
  g <- merge(g, lk, by = "specieskey")
  after <- data.table::uniqueN(g$specieskey)
  list(grain = g[, .(key, eeacellcode, datasetkey, source_group,
                     min_year, occ, n_rec)],
       n_dropped = before - after)
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
  regime_boundary <- (as.Date(from) <= as.Date("2024-01-01")) &&
                     (as.Date(to)   >  as.Date("2024-01-01"))
  if (regime_boundary) {
    cli_alert_warning(
      "This pair straddles the snapshot -> cube boundary. The app must mark it: \\
       the step is a method change, not a result."
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

    for (ks in KEY_SPACES) {
      tag <- sprintf("%dkm_%s", res_km, ks)
      rf <- rekey_grain(raw_from, mt_from, ks)
      rt <- rekey_grain(raw_to,   mt_to,   ks)
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
      data.table::setorder(grp, -pairs_gained)

      summ <- closure_summary(pairs_from, pairs_to, cells, species, mech,
                              key_space = ks, baseline_year = baseline_year,
                              regime_boundary = regime_boundary)
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
