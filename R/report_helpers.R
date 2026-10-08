# R/report_helpers.R
# ============================================================================
# Standalone helpers for the summary report (analysis/gap_finder_report.Rmd)
# ============================================================================
# Deliberately dependency-free: the report sources this file before it loads
# anything else. It must therefore not depend on 00_setup.R, on the config, or
# on any package beyond base R (`here` is used only if installed, with a
# fallback).
#
# Nothing here has side effects — sourcing it twice, or from two different
# environments, is safe.
# ============================================================================

.gf_root <- function() {
  if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
}

#' Locate the prepared Gap Finder bundle
#'
#' The bundle lives at a per-country path (`data/{CC}/shiny_data.rds`). Honour
#' GBIF_GAP_COUNTRY, else take the first bundle present, else fall back to the
#' legacy flat location (run.R's gap_finder_data_path() does the last two).
#'
#' @return Path to shiny_data.rds. Errors with a readable message if none exists.
gap_finder_bundle_path <- function() {
  base <- file.path(.gf_root(), "shiny_app", "gap_finder", "data")
  cc   <- Sys.getenv("GBIF_GAP_COUNTRY", "")

  if (nzchar(cc)) {
    p <- file.path(base, cc, "shiny_data.rds")
    if (file.exists(p)) return(p)
  }
  hits <- Sys.glob(file.path(base, "*", "shiny_data.rds"))
  if (length(hits) >= 1L) return(hits[1L])

  legacy <- file.path(base, "shiny_data.rds")
  if (file.exists(legacy)) return(legacy)

  stop("No Gap Finder bundle found under ", base,
       ". Run run_gap_finder_prep() (script 11) first.", call. = FALSE)
}

#' The code version of the pipeline
#'
#' Single source of truth is the git tag. An explicit GAP_FINDER_VERSION wins,
#' as in the app image, so a report rendered for a release can carry that
#' release's number before the tag exists. "dev" when neither is available,
#' which is honest rather than claiming a release.
gap_finder_version <- function() {
  v <- Sys.getenv("GAP_FINDER_VERSION", "")
  if (nzchar(v)) return(v)
  v <- tryCatch(
    suppressWarnings(system2("git", c("-C", shQuote(.gf_root()), "describe",
                                      "--tags", "--always", "--dirty"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (length(v) && nzchar(v[1])) sub("^v", "", v[1]) else "dev"
}

#' Header stamp for a report: data date, code version, render date
#'
#' Keeps the DATA date and the CODE version distinct and shows both, so a reader
#' can tell "these are July's numbers, rendered today by v1.0.0" from the header
#' alone. The data date is the bundle's `metadata$snapshot_date` (when GBIF cut
#' the cube), the same date the app shows, so report and app cannot disagree.
#'
#' @param snapshot_date Date (or NA) — the bundle's metadata$snapshot_date.
report_stamp <- function(snapshot_date) {
  snap <- if (is.null(snapshot_date) || all(is.na(snapshot_date))) as.Date(NA)
          else as.Date(snapshot_date)
  ver  <- gap_finder_version()
  # Only a numeric version earns a "v" — "Gap Finder vdev" reads like a typo.
  ver  <- if (grepl("^[0-9]", ver)) paste0("v", ver) else ver
  paste0(
    "Data as of ", if (is.na(snap)) "unknown" else format(snap, "%d %B %Y"),
    " · rendered ", format(Sys.Date(), "%d %B %Y"),
    " · Gap Finder ", ver
  )
}
