# scripts/00_setup.R
# ==============================================================================
# Project Setup & Environment Initialization
# ==============================================================================
# This script:
# - Ensures required packages are available
# - Loads project configuration and global constants
# - Creates necessary directory structure
# - Records session info for reproducibility

# Bootstrap: ensure 'here' package is available ---------------------------
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

library(here) # finding the project root and building paths
library(purrr) # functional programming
library(glue) # string interpolation
library(cli) # 'command-line-interface, pretty console output with colors and formatting

# Load package management functions --------------------------------------
source(here("R", "packages.R"))

# Check for missing packages ----------------------------------------------
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

# Load project globals and configuration ----------------------------------
source(here("R", "globals.R"))

# Display configured data paths -------------------------------------------
cli::cli_h2("Configured Raw Data Locations")
cli::cli_dl(c(
  "GBIF cube" = raw_gbif_cube_dir,
  "EEA grid 10km" = raw_grid_10km_dir,
  "EEA grid 50km" = raw_grid_50km_dir,
  "Red List SE" = raw_redlist_se_dir,
  "Red List IUCN" = raw_redlist_iucn_dir,
  "Dyntaxa" = raw_dyntaxa_dir
))

# Ensure required directories exist ---------------------------------------
required_dirs <- c(
  p_logs,
  p_data_proc,
  p_output
)

purrr::walk(required_dirs, ~{
  if (!dir.exists(.x)) {
    dir.create(.x, recursive = TRUE, showWarnings = FALSE)
    cli::cli_alert_success("Created directory: {.path {(.x)}}")
  }
})

# Record session info for reproducibility ---------------------------------
session_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
session_file <- here(p_logs, glue::glue("session_{session_timestamp}.txt"))

session_info <- c(
  glue::glue("Run time: {Sys.time()}"),
  glue::glue("R version: {R.version.string}"),
  "",
  "=" |> rep(80) |> paste(collapse = ""),
  "Session Info:",
  "=" |> rep(80) |> paste(collapse = ""),
  "",
  capture.output(sessionInfo())
)

writeLines(session_info, session_file)
cli::cli_alert_success("Session log written: {.path {session_file}}")

# Check renv status -------------------------------------------------------
renv_lock <- here("renv.lock")

if (!file.exists(renv_lock)) {
  cli::cli_alert_warning(
    "renv.lock not found. Initialize with {.code renv::init()}"
  )
} else {
  cli::cli_alert_info(
    "renv.lock present. Use {.code renv::restore()} to sync packages"
  )
}

cli::cli_alert_success("Setup complete!")
