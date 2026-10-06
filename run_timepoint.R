#!/usr/bin/env Rscript
# run_timepoint.R
# ============================================================================
# Run the gap pipeline against ONE historical time point
# ============================================================================
# Usage:
#   Rscript run_timepoint.R 2021-01-01
#   Rscript run_timepoint.R 2021-01-01 --from 07     # resume at script 07
#
# What this is:
#   A wrapper, not a `targets` branch. `_targets.R` sources scripts/00_setup.R
#   once per session, which fixes p_derived / p_gaps / p_cubes / p_output for the
#   whole run; branching over time points would mean mutating those globals
#   INSIDE each branch — session-level state that leaks between branches and
#   breaks outright the first time `targets` runs two in parallel. Historical
#   points are also computed once and frozen, so they gain nothing from
#   incrementality. `targets` runs the live cube; this script is only for the
#   frozen points.
#
# What it does NOT run, and why:
#   01a/01b/02/03  downloads and reference data are time-INVARIANT and pinned.
#                  Re-running them would move the Dyntaxa checklist under the
#                  series and manufacture change that is not data.
#   04             the live cube converter. 04b already built this time point's
#                  cubes.
#   09a1           the COL crosswalk: ~22k API calls, time-invariant, and shared.
#                  It must already exist at the root; asserted below.
#   11             THE APP BUNDLE. 11 writes shiny_app/gap_finder/data/{CC}/
#                  shiny_data.rds — a path outside every path global, so running
#                  it here would overwrite the DEPLOYED app's 80 MB LFS-tracked
#                  bundle with a 2021 snapshot. Nothing needs it: the closure
#                  metrics (script 14) read the time-point directories directly,
#                  and the app bundle is rebuilt once, from the live pipeline.
#   12/13          guardrail + metrics snapshot, both live-pipeline specific.
#
# Outputs land under data/{CC}/proc/timepoints/{tp}/ — derived/, gaps/, output/,
# plus reference_version.yml recording which upstream releases this point was
# computed against.
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || args[1] %in% c("-h", "--help")) {
  cat("Usage: Rscript run_timepoint.R <YYYY-MM-DD> [--from <script>]\n")
  quit(status = if (length(args)) 0L else 1L)
}

tp <- args[1]
if (is.na(suppressWarnings(as.Date(tp))) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", tp)) {
  stop("Time point must be an ISO date, e.g. 2021-01-01 (got: ", tp, ")", call. = FALSE)
}
from <- if ("--from" %in% args) args[match("--from", args) + 1L] else NULL

# Set the env var BEFORE 00_setup.R runs: R/globals.R reads it to compute every
# time-varying path, and it is read once per session.
Sys.setenv(GAP_FINDER_TIMEPOINT = tp)

suppressPackageStartupMessages(library(here))
source(here::here("scripts", "00_setup.R"))

cli_h1("Time point {tp}")
cli_alert_info("Writing under {.path {p_timepoint}}")

# ============================================================================
# Preconditions — every one of these is a silent-wrong-answer if skipped
# ============================================================================

if (!identical(p_timepoint, here(p_data_proc, "timepoints", tp))) {
  cli_abort(c(
    "p_timepoint did not pick up GAP_FINDER_TIMEPOINT.",
    "x" = "Got {.path {p_timepoint}}.",
    "i" = "R/globals.R must read {.envvar GAP_FINDER_TIMEPOINT} — check that \\
           it supports time points."
  ))
}

cubes <- file.path(p_cubes, c("cube_10km.parquet", "cube_50km.parquet"))
missing_cubes <- cubes[!file.exists(cubes)]
if (length(missing_cubes)) {
  cli_abort(c(
    "No cube for time point {tp}: {paste(basename(missing_cubes), collapse = ', ')}",
    "i" = "Run {.code Rscript -e 'source(\"scripts/04b_build_historical_cubes.R\")'} first."
  ))
}

# The pinned, time-invariant inputs. 09a matches on NAMES against the national
# checklist, so if the checklist moved between time points the series measures
# the checklist, not the data.
pinned <- c(
  taxa_reference   = here(p_data_proc, "taxa_reference_current.rds"),
  col_crosswalk    = here(p_data_proc, "col_crosswalk.rds"),
  grid_10km        = here(p_data_proc, "grids_10km.gpkg"),
  grid_50km        = here(p_data_proc, "grids_50km.gpkg")
)
absent <- pinned[!file.exists(pinned)]
if (length(absent)) {
  cli_abort(c(
    "Pinned reference input{?s} missing: {paste(names(absent), collapse = ', ')}",
    "i" = "These are shared by every time point and are built by the LIVE \\
           pipeline (scripts 02, 03, 09a1). Run {.code tar_make()} first.",
    "x" = "Do not let a time-point run rebuild them — a moving checklist would \\
           manufacture change that is not data."
  ))
}

ensure_dirs()

# ============================================================================
# Pin the upstream versions this time point was computed against
# ============================================================================
# `git diff provenance/` answers "did anything upstream move?" for the live
# pipeline. A frozen time point needs the same answer permanently attached to
# it, because it will be differenced against a point computed months later.

ref_src <- here("provenance", glue("upstream_versions_{COUNTRY_CODE}.yml"))
ref_dst <- file.path(p_timepoint, "reference_version.yml")
if (file.exists(ref_src)) {
  ref <- yaml::read_yaml(ref_src)
  pin <- ref[intersect(c("taxonomy", "redlist", "invasives", "sensitive",
                         "col_backbone"), names(ref))]
  pin$timepoint <- tp
  pin$note <- paste(
    "Upstream releases this time point was computed against. Frozen:",
    "a later time point differenced against this one must have been computed",
    "against the SAME releases, or the difference is part taxonomy churn."
  )
  yaml::write_yaml(pin, ref_dst)
  cli_alert_success("Pinned reference versions: {.path {basename(ref_dst)}}")
  if (!is.null(pin$taxonomy$published)) {
    cli_alert_info("Dyntaxa release: {pin$taxonomy$published}")
  }
} else {
  cli_alert_warning(
    "No {.path {basename(ref_src)}} — this time point will carry no record of \\
     which checklist it used. Run script 01b to create it."
  )
}

# ============================================================================
# Run
# ============================================================================

steps <- c(
  "05" = "05_validate_inputs.R",
  "06a" = "06a_make_core_summaries.R",
  "06b" = "06b_make_species_summaries.R",
  "07"  = "07_spatial_gaps.R",
  "08"  = "08_temporal_gaps.R",
  "09a" = "09a_reconcile_taxonomy.R",
  "09b" = "09b_taxonomic_gaps.R",
  "09c" = "09c_scope_summaries.R",
  "10"  = "10_make_gap_overview.R"
)

if (!is.null(from)) {
  if (!from %in% names(steps)) {
    cli_abort("--from must be one of: {paste(names(steps), collapse = ', ')}")
  }
  steps <- steps[match(from, names(steps)):length(steps)]
  cli_alert_info("Resuming at {from}")
}

t_all <- Sys.time()
for (i in seq_along(steps)) {
  nm <- names(steps)[i]
  cli_h2("[{i}/{length(steps)}] script {nm} — {tp}")
  t0 <- Sys.time()
  # A fresh env per script keeps its objects out of the global environment, so a
  # variable one script leaves behind cannot be silently read by the next.
  source(here("scripts", steps[[i]]), local = new.env(parent = globalenv()))
  cli_alert_success(
    "script {nm} done in {round(as.numeric(difftime(Sys.time(), t0, units = 'mins')), 1)} min"
  )
}

cli_h1("Time point {tp} complete")
cli_alert_success(
  "{length(steps)} script{?s} in \\
   {round(as.numeric(difftime(Sys.time(), t_all, units = 'mins')), 1)} min"
)
cli_dl(c(
  "Derived"   = p_derived,
  "Gaps"      = p_gaps,
  "Output"    = p_output,
  "Reference" = ref_dst
))
cli_alert_info(
  "The app bundle (script 11) is deliberately NOT run for a time point — it \\
   would overwrite the deployed app's data. Closure metrics read these \\
   directories directly."
)
