# scripts/15_bridge.R
# ============================================================================
# The bridge: a snapshot and a cube of the same date, compared
# ============================================================================
# Purpose:
#   Measure the method gap between the two regimes. A GBIF Sweden snapshot and
#   a GBIF SQL cube taken a day apart hold the same records, so their
#   difference is the method alone: the cube's presence-only and
#   no-geospatial-issue filters, the snapshot's NFI exclusion, 2 dp versus
#   full-precision gridding, and records interpreted differently between the
#   two reads. The output says how far a snapshot number and a cube number can
#   be put side by side, per resolution and per class x order.
#
#   Not a closure pair: "gained" and "lost" here would be method noise, so
#   script 14 refuses this pair and this script reports it instead.
#
#   Arithmetic in R/bridge.R (tools/test_bridge.R), readers in R/timepoint_io.R
#   (shared with script 14).
#
# RUN IT WITH GAP_FINDER_TIMEPOINT UNSET (as script 14).
#
# Usage:
#   Rscript -e 'source("scripts/15_bridge.R")'
#   GAP_BRIDGE_PAIR="2026-10-01,2026-10-02" Rscript -e 'source("scripts/15_bridge.R")'
#   Without GAP_BRIDGE_PAIR it takes the closest snapshot / cube pair less than
#   31 days apart.
#
# Inputs: as script 14 (cubes, match tables, reference_version.yml).
# Outputs (data/{CC}/proc/bridge/), {snap}_{cube} = the two time points:
#   - bridge_summary_{snap}_{cube}.csv          metric x resolution x key_space
#   - bridge_groups_{res}_{ks}_{snap}_{cube}.csv     factors per class x order
#   - bridge_datasets_{res}_{ks}_{snap}_{cube}.csv   who carries the one-sided pairs
#   - bridge_dataset_occ_{snap}_{cube}.csv      occurrences per dataset, both sides
#
# Dependencies: scripts/00_setup.R, data.table, arrow, R/closure.R,
#               R/timepoint_io.R, R/bridge.R
# ============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("R", "closure.R"))
source(here::here("R", "timepoint_io.R"))
source(here::here("R", "bridge.R"))

if (!requireNamespace("arrow", quietly = TRUE)) {
  cli_abort(c("Package {.pkg arrow} is required",
              "i" = "Install: {.code install.packages('arrow')}"))
}
library(arrow)

if (nzchar(Sys.getenv("GAP_FINDER_TIMEPOINT", ""))) {
  cli_abort(c(
    "GAP_FINDER_TIMEPOINT is set ({Sys.getenv('GAP_FINDER_TIMEPOINT')}).",
    "x" = "Script 15 compares two time points; it must not run inside one.",
    "i" = "Unset it: {.code Sys.unsetenv('GAP_FINDER_TIMEPOINT')}"
  ))
}

tp_root    <- here(p_data_proc, "timepoints")
bridge_dir <- here(p_data_proc, "bridge")
if (!dir.exists(bridge_dir)) dir.create(bridge_dir, recursive = TRUE, showWarnings = FALSE)

PLATFORMS   <- as.character(cfg_get(
  "parameters.closure.source_groups.observation_platforms", character()))
BRIDGE_DAYS <- 31L
RESOLUTIONS <- c(10L, 50L)

cli_h1("Bridge - {COUNTRY_CODE}")

# ============================================================================
# Time points
# ============================================================================

timepoints <- sort(basename(list.dirs(tp_root, recursive = FALSE)))
timepoints <- timepoints[vapply(
  timepoints,
  function(tp) file.exists(file.path(tp_root, tp, "cubes", "cube_10km.parquet")),
  logical(1))]

LIVE_TP <- live_timepoint_date()
live_ok <- !is.na(LIVE_TP) &&
  file.exists(here(p_cubes, "cube_10km.parquet")) &&
  file.exists(here(p_gaps, "taxonomic_match_table.csv"))
if (live_ok) timepoints <- sort(unique(c(timepoints, LIVE_TP)))

TP_META <- stats::setNames(lapply(timepoints, tp_meta), timepoints)
meta_dt <- data.table::rbindlist(lapply(TP_META, as.data.frame))
print(meta_dt)

pair_spec <- Sys.getenv("GAP_BRIDGE_PAIR", "")
if (nzchar(pair_spec)) {
  pr <- trimws(strsplit(pair_spec, ",")[[1]])
  if (length(pr) != 2L || !all(pr %in% timepoints)) {
    cli_abort(c(
      "GAP_BRIDGE_PAIR must name two existing time points, comma separated.",
      "x" = "Not found: {.val {setdiff(pr, timepoints)}}",
      "i" = "Available: {.val {timepoints}}"
    ))
  }
} else {
  cand <- data.table::CJ(snap = meta_dt[regime == "snapshot", tp],
                         cube = meta_dt[regime == "cube", tp])
  if (nrow(cand)) {
    cand[, days := abs(as.numeric(as.Date(cube) - as.Date(snap)))]
    cand <- cand[days < BRIDGE_DAYS][order(days)]
  }
  if (!nrow(cand)) {
    cli_abort(c(
      "No snapshot and cube less than {BRIDGE_DAYS} days apart.",
      "i" = "Time points: {paste(meta_dt$tp, meta_dt$regime, sep = ':', collapse = ', ')}"
    ))
  }
  pr <- c(cand$snap[1], cand$cube[1])
}

kind <- closure_pair_kind(TP_META[[pr[1]]], TP_META[[pr[2]]], bridge_days = BRIDGE_DAYS)
if (!kind$is_bridge) {
  cli_abort(c(
    "{pr[1]} and {pr[2]} are not a bridge pair.",
    "x" = "A bridge needs one snapshot and one cube less than {BRIDGE_DAYS} days apart.",
    "i" = "Regimes: {TP_META[[pr[1]]]$regime} / {TP_META[[pr[2]]]$regime}"
  ))
}
snap_tp <- if (TP_META[[pr[1]]]$regime == "snapshot") pr[1] else pr[2]
cube_tp <- setdiff(pr, snap_tp)
cli_alert_success("Bridge: snapshot {.val {snap_tp}} vs cube {.val {cube_tp}}")

rel_s <- reference_release(snap_tp); rel_c <- reference_release(cube_tp)
if (!is.na(rel_s) && !is.na(rel_c) && !identical(rel_s, rel_c)) {
  cli_abort(c(
    "The national checklist differs: {snap_tp} ({rel_s}) vs {cube_tp} ({rel_c}).",
    "i" = "Re-run {.code Rscript run_timepoint.R {snap_tp}} against the current release."
  ))
}
key_spaces <- intersect(c("dyntaxa", "gbif"), kind$key_spaces)
if (!"gbif" %in% key_spaces) {
  cli_alert_info("GBIF key space skipped: the two sides use different key backbones")
}

mt_s <- read_match_table(snap_tp)
mt_c <- read_match_table(cube_tp)

# ============================================================================
# Compare
# ============================================================================

summaries <- list()
occ_tables <- list()

grains <- list()
for (res_km in RESOLUTIONS) {
  grains[[as.character(res_km)]] <- list(snap = read_grain(snap_tp, res_km),
                                         cube = read_grain(cube_tp, res_km))
}

for (ks in key_spaces) {
  # Re-keyed grains and pairs for both resolutions first: the 10 km locality
  # test needs the 50 km pairs of the other side.
  rk <- lapply(grains, function(g) list(
    snap = closure_rekey(g$snap, mt_s, ks)$grain,
    cube = closure_rekey(g$cube, mt_c, ks)$grain))
  pairs <- lapply(rk, function(g) list(snap = closure_pairs(g$snap),
                                       cube = closure_pairs(g$cube)))

  # Groups are described by the cube side's taxonomy, the snapshot's for keys
  # the cube lacks (same rule as closure_taxon_lookup()).
  lk_key <- if (ks == "gbif") "specieskey" else "backbone_taxonID"
  lk <- closure_taxon_lookup(mt_c, mt_s, c("class", "order"), lk_key)$lookup

  for (res_km in RESOLUTIONS) {
    r <- as.character(res_km)
    tag <- sprintf("%dkm_%s", res_km, ks)
    p_s <- pairs[[r]]$snap; p_c <- pairs[[r]]$cube
    cmp <- bridge_compare_pairs(p_s, p_c)

    loc_s <- loc_c <- NULL
    if (res_km == 10L) {
      loc_s <- bridge_is_local(cmp$snapshot_only, pairs[["50"]]$cube)
      loc_c <- bridge_is_local(cmp$cube_only,     pairs[["50"]]$snap)
    }

    summ <- bridge_summary(cmp, p_s, p_c, loc_s, loc_c,
                           rk[[r]]$snap, rk[[r]]$cube)
    summ[, `:=`(resolution = sprintf("%dkm", res_km), key_space = ks)]
    summaries[[tag]] <- summ

    grp <- bridge_group_factors(p_s, p_c, cmp, lk)
    sup <- merge(
      bridge_dataset_support(cmp$snapshot_only, rk[[r]]$snap)[
        , .(datasetkey, snapshot_only_any = pairs_any, snapshot_only_sole = pairs_sole)],
      bridge_dataset_support(cmp$cube_only, rk[[r]]$cube)[
        , .(datasetkey, cube_only_any = pairs_any, cube_only_sole = pairs_sole)],
      by = "datasetkey", all = TRUE)
    for (col in setdiff(names(sup), "datasetkey")) {
      data.table::set(sup, which(is.na(sup[[col]])), col, 0L)
    }
    data.table::setorder(sup, -snapshot_only_sole, -cube_only_sole)

    stem <- sprintf("_%s_%s_%s.csv", tag, snap_tp, cube_tp)
    data.table::fwrite(grp, file.path(bridge_dir, paste0("bridge_groups", stem)))
    data.table::fwrite(sup, file.path(bridge_dir, paste0("bridge_datasets", stem)))

    v <- function(m) summ[metric == m, value]
    cli_alert_success(
      "{tag}: {scales::comma(v('pairs_snapshot'))} snapshot pairs, \\
       {scales::comma(v('pairs_cube'))} cube pairs; snapshot-only \\
       {v('snapshot_only_pct')}%, cube-only {v('cube_only_pct')}%"
    )
    if (res_km == 10L) {
      cli_alert_info(
        "{tag}: snapshot-only pairs local (taxon in the same 50 km cell of the \\
         cube) {scales::comma(v('snapshot_only_local'))}, regional \\
         {scales::comma(v('snapshot_only_regional'))}"
      )
    }
  }

  if (ks == "dyntaxa") {
    occ_tables[[ks]] <- bridge_dataset_occ(grains[["10"]]$snap, grains[["10"]]$cube)
  }
  rm(rk, pairs); invisible(gc())
}

all_summ <- data.table::rbindlist(summaries, use.names = TRUE)
data.table::setcolorder(all_summ, c("metric", "resolution", "key_space", "value"))
out <- file.path(bridge_dir, sprintf("bridge_summary_%s_%s.csv", snap_tp, cube_tp))
data.table::fwrite(all_summ, out)
cli_alert_success("{.path {basename(out)}}")

# All records, before any re-keying: the dataset view of the record filters.
occ <- occ_tables[["dyntaxa"]]
out_occ <- file.path(bridge_dir, sprintf("bridge_dataset_occ_%s_%s.csv", snap_tp, cube_tp))
data.table::fwrite(occ, out_occ)
cli_alert_success("{.path {basename(out_occ)}}")

cli_h2("Done")
cli_alert_info("Bridge tables: {.path {bridge_dir}}")
