# scripts/00_setup.R
# ============================================================================
# Project Setup & Environment Initialisation
# ============================================================================
# Purpose:
#   Bootstrap the project environment. Every other script sources this
#   file first, so it must be safe to run repeatedly (idempotent).
#
# Actions:
#   1. Ensure core packages are available
#   2. Load project configuration and global constants
#   3. Create the directory structure
#   4. Record session info for reproducibility
#
# Inputs:  R/packages.R, R/globals.R, config.yml
# Outputs: logs/session_<timestamp>.txt
# ============================================================================

# Bootstrap: ensure 'here' is available ------------------------------------
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

library(here)
library(purrr)
library(glue)
library(cli)

# Load package management functions -----------------------------------------
source(here("R", "packages.R"))

# Check for missing packages ------------------------------------------------
missing_pkgs <- check_packages()

if (length(missing_pkgs) > 0) {
  cli_alert_warning(
    "Missing packages detected: {.pkg {missing_pkgs}}"
  )
  cli_alert_info(
    "To install, run: {.code install_missing_packages()}"
  )
  cli_alert_info(
    "Then lock dependencies: {.code renv::snapshot()}"
  )
}

# Load project globals and configuration ------------------------------------
source(here("R", "globals.R"))

# Display configured data paths ---------------------------------------------
country_name <- cfg_get("country.name", "(not set)")
cli_h2("Project: GBIF Gap Analysis \u2014 {country_name}")

cli_dl(c(
  "GBIF cube"          = raw_gbif_cube_dir,
  "Grid 10km"          = raw_grid_10km_dir,

  "Grid 50km"          = raw_grid_50km_dir,
  "National red list"  = raw_redlist_dir,
  "National taxonomy"  = raw_taxonomy_dir
))

# Ensure required directories exist -----------------------------------------
required_dirs <- c(p_logs, p_data_proc, p_output)

purrr::walk(required_dirs, \(d) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    cli_alert_success("Created directory: {.path {d}}")
  }
})

# Record session info for reproducibility -----------------------------------
session_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
session_file <- here(
  p_logs,
  glue("session_{session_timestamp}.txt")
)

session_info <- c(
  glue("Run time:  {Sys.time()}"),
  glue("R version: {R.version.string}"),
  "",
  strrep("=", 72),
  "Session Info:",
  strrep("=", 72),
  "",
  capture.output(sessionInfo())
)

writeLines(session_info, session_file)
cli_alert_success("Session log: {.path {session_file}}")

# Check renv status ---------------------------------------------------------
renv_lock <- here("renv.lock")

if (!file.exists(renv_lock)) {
  cli_alert_warning(
    "renv.lock not found. Initialise with {.code renv::init()}"
  )
} else {
  cli_alert_info(
    "renv.lock present. Use {.code renv::restore()} to sync"
  )
}

cli_alert_success("Setup complete!")
