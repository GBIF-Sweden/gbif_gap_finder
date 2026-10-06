#!/usr/bin/env Rscript
# tools/test_timepoint_paths.R
# ============================================================================
# Regression test for the time-point path switch (R/globals.R)
# ============================================================================
# The whole safety argument for the time-point switch is one sentence: with
# GAP_FINDER_TIMEPOINT UNSET, every path is byte-identical to the live layout
# below. If that ever stops being true, the live pipeline silently starts
# writing somewhere else and nothing errors.
#
# It is worth a test rather than an assurance because the failure is invisible:
# `p_output` in particular is a SIBLING of proc/ in the live layout, so the
# obvious implementation (repoint everything under p_timepoint) quietly moves
# data/{CC}/output to data/{CC}/proc/output the moment it ships.
#
#   Rscript tools/test_timepoint_paths.R
#
# Exits non-zero on any failure, so it can go in CI.
# ============================================================================

suppressPackageStartupMessages({ library(here); library(cli) })

fail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) cli_alert_success(msg)
  else { cli_alert_danger(msg); fail <<- fail + 1L }
}

#' Source globals.R in a clean child process and return its path variables
paths_with <- function(timepoint = "") {
  script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf('Sys.setenv(GAP_FINDER_TIMEPOINT = "%s")', timepoint),
    'suppressPackageStartupMessages({library(here); library(cli); library(yaml)})',
    'suppressMessages(suppressWarnings(source(here::here("R", "globals.R"))))',
    'vars <- c("p_data", "p_data_raw", "p_data_proc", "p_timepoint", "p_logs",',
    '          "p_derived", "p_by_order", "p_by_family", "p_gaps", "p_cubes",',
    '          "p_output", "p_tables", "p_integrated")',
    'cat(paste(vars, vapply(vars, function(v) get(v), character(1)), sep = "\t"), sep = "\n")'
  ), script)
  out <- suppressWarnings(system2("Rscript", script, stdout = TRUE, stderr = FALSE))
  out <- out[grepl("^p_[a-z_]+\t", out)]
  setNames(sub("^[^\t]+\t", "", out), sub("\t.*$", "", out))
}

cc <- Sys.getenv("GBIF_GAP_COUNTRY", "SE")
live <- paths_with("")
tp   <- paths_with("2021-01-01")

cli_h2("Live layout (GAP_FINDER_TIMEPOINT unset) must be unchanged")

expected <- c(
  p_data       = here("data", cc),
  p_data_raw   = here("data", cc, "raw"),
  p_data_proc  = here("data", cc, "proc"),
  p_timepoint  = here("data", cc, "proc"),
  p_logs       = here("logs"),
  p_derived    = here("data", cc, "proc", "derived"),
  p_by_order   = here("data", cc, "proc", "derived", "by_order"),
  p_by_family  = here("data", cc, "proc", "derived", "by_family"),
  p_gaps       = here("data", cc, "proc", "gaps"),
  p_cubes      = here("data", cc, "proc", "cubes"),
  # the one that would move silently under a naive implementation:
  p_output     = here("data", cc, "output"),
  p_tables     = here("data", cc, "output", "tables"),
  p_integrated = here("data", cc, "output", "tables", "integrated")
)

for (v in names(expected)) {
  ok(identical(unname(live[[v]]), unname(expected[[v]])),
     sprintf("%-13s %s", v, live[[v]]))
}

cli_h2("Time-point layout (GAP_FINDER_TIMEPOINT=2021-01-01)")

root <- here("data", cc, "proc", "timepoints", "2021-01-01")
moves <- c("p_timepoint", "p_derived", "p_by_order", "p_by_family", "p_gaps",
           "p_cubes", "p_output", "p_tables", "p_integrated")
for (v in moves) {
  ok(startsWith(tp[[v]], root), sprintf("%-13s moves under the time point", v))
}
stays <- c("p_data", "p_data_raw", "p_data_proc", "p_logs")
for (v in stays) {
  ok(identical(unname(tp[[v]]), unname(live[[v]])),
     sprintf("%-13s stays at the root (time-invariant)", v))
}
ok(identical(unname(tp[["p_output"]]), file.path(root, "output")),
   "p_output           lands inside the time point, not beside proc/")

cli_h2("Result")
if (fail == 0L) {
  cli_alert_success("All path checks passed")
  quit(status = 0L)
}
cli_abort("{fail} path check{?s} failed - do NOT ship this globals.R")
