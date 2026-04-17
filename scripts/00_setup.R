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
# Inputs:  R/packages.R, R/globals.R, configs/config_{CC}.yml
# Outputs: logs/session_<timestamp>.txt
# ============================================================================

# Bootstrap: ensure 'here' is available ------------------------------------
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# Load package management functions -----------------------------------------
source(here("R", "packages.R"))

# Check for missing packages ------------------------------------------------
missing_pkgs <- check_packages()
if (length(missing_pkgs) > 0) {
  cli::cli_alert_warning(
    "Missing packages detected: {.pkg {missing_pkgs}}"
  )
  cli::cli_alert_info(
    "To install, run: {.code install_missing_packages()}"
  )
  cli::cli_alert_info(
    "Then lock dependencies: {.code renv::snapshot()}"
  )
}

# Load all required packages ------------------------------------------------
load_packages()

# Load project globals and configuration ------------------------------------
source(here("R", "globals.R"))

# Display configured data paths ---------------------------------------------
country_name <- cfg_get("country.name", "(not set)")
cli_h2("Project: GBIF Gap Analysis \u2014 {country_name}")

cli_dl(c(
  "Data directory"     = p_data,
  "Cubes"              = raw_gbif_cube_dir,
  "Grids (shared)"     = raw_grid_dir,
  "Taxonomy"           = raw_taxonomy_dir,
  "Red list"           = raw_redlist_dir,
  "Invasives"          = if (exists("raw_invasives_dir")) raw_invasives_dir else "(not configured)",
  "Sensitive"          = if (exists("raw_sensitive_dir")) raw_sensitive_dir else "(not configured)",
  "Admin boundaries"   = raw_admin_dir
))

# Grid abstraction ---------------------------------------------------------
CELLCODE_FIELD  <- cfg_get("parameters.grid.cellcode_field", "eeacellcode")
GRID_PREFIX     <- cfg_get("parameters.grid.grid_prefix", "EEA")
GRID_RESOLUTIONS <- cfg_get("parameters.grid.resolutions", c(10, 50))

# Ensure required directories exist -----------------------------------------
ensure_dirs()

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
