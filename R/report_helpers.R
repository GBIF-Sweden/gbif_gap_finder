# R/report_helpers.R
# ============================================================================
# Standalone helpers for analysis/*.Rmd
# ============================================================================
# Deliberately dependency-free: this file is sourced from the YAML `date:`
# field of every report, which rmarkdown evaluates BEFORE any chunk runs. It
# must therefore not depend on 00_setup.R, on the config, or on any package
# beyond base R (`here` is used only if already attached, with a fallback).
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
#' Single source of truth is the git tag. In a container there is no git, so an
#' explicit GAP_FINDER_VERSION (baked at image build) wins. "dev" when neither
#' is available, which is honest rather than claiming a release.
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

#' The data snapshot date — when GBIF cut the cube
#'
#' NOT the same as when the bundle was packaged. Read from the small
#' data_sources_meta.rds (never the ~80 MB bundle), using the same rule as
#' globals::get_snapshot_date(): the latest `created` across the cube downloads.
gap_finder_snapshot_date <- function(country = Sys.getenv("GBIF_GAP_COUNTRY", "SE")) {
  p <- file.path(.gf_root(), "data", country, "proc", "data_sources_meta.rds")
  if (!file.exists(p)) return(as.Date(NA))
  meta <- tryCatch(readRDS(p), error = function(e) NULL)
  cubes <- meta$cubes
  if (is.null(cubes) || !length(cubes)) return(as.Date(NA))
  d <- suppressWarnings(do.call(c, lapply(cubes, function(x) {
    if (is.null(x$created) || all(is.na(x$created))) as.Date(NA) else as.Date(x$created)
  })))
  d <- d[!is.na(d)]
  if (length(d)) max(d) else as.Date(NA)
}

#' Header stamp for a report: data date, code version, render date
#'
#' Keeps the DATA date and the CODE version distinct and shows both, so a reader
#' can tell "these are July's numbers, rendered today by v0.5.1" from the header
#' alone. The render date alone would silently imply the data is as fresh as the
#' PDF.
report_stamp <- function() {
  snap <- gap_finder_snapshot_date()
  ver  <- gap_finder_version()
  # Only a numeric version earns a "v" — "Gap Finder vdev" reads like a typo.
  ver  <- if (grepl("^[0-9]", ver)) paste0("v", ver) else ver
  paste0(
    "Data as of ", if (is.na(snap)) "unknown" else format(snap, "%d %B %Y"),
    " · rendered ", format(Sys.Date(), "%d %B %Y"),
    " · Gap Finder ", ver
  )
}
