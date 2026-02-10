# R/globals.R
# ============================================================================
# Project-Wide Configuration, Paths, and Constants
# ============================================================================
# Purpose:
#   Central configuration loader for the GBIF gap analysis pipeline.
#   Defines directory paths, CRS constants, config helpers, and
#   shared utility functions used by all downstream scripts.
#
# Sourced by: scripts/00_setup.R (and transitively by every script)
#
# Dependencies:
#   - here
#   - cli
#   - yaml (for config loading)
#   - stringr (for guess_cellcode_field)
# ============================================================================

library(here)
library(cli)

# ============================================================================
# Configuration Loading
# ============================================================================

#' Read YAML configuration file
#'
#' @param path Path to config.yml (default: project root)
#' @return List of configuration values, or empty list if not found
read_config <- function(path = here("config.yml")) {
  if (!file.exists(path)) {
    cli_alert_warning(
      "config.yml not found at: {.path {path}}"
    )
    cli_alert_info("Using default paths")
    return(list())
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    cli_abort(c(
      "Package {.pkg yaml} is required to read config.yml",
      "i" = "Install with: {.code install.packages('yaml')}"
    ))
  }

  yaml::read_yaml(path)
}

# Load configuration at source time
cfg <- read_config()

#' Get a nested configuration value by dotted path
#'
#' @param name Dotted key path (e.g., "paths.data_proc")
#' @param default Value returned when key is missing
#' @return Configuration value or `default`
#'
#' @examples
#' cfg_get("parameters.crs", 3035)
#' cfg_get("paths.data_raw", "data_raw")
cfg_get <- function(name, default = NULL) {
  keys   <- strsplit(name, ".", fixed = TRUE)[[1]]
  result <- cfg

  for (key in keys) {
    if (is.null(result) || is.null(result[[key]])) {
      return(default)
    }
    result <- result[[key]]
  }

  result
}

# ============================================================================
# Global Options
# ============================================================================

options(stringsAsFactors = FALSE)

# Timezone: configurable per country (default UTC)
Sys.setenv(TZ = cfg_get("country.timezone", "UTC"))

# ============================================================================
# Project Paths
# ============================================================================

# Top-level directories
p_root     <- here()
p_R        <- here("R")
p_scripts  <- here("scripts")
p_analysis <- here("analysis")

p_data_raw  <- here("data_raw")
p_data_proc <- here("data_proc")
p_output    <- here("output")
p_docs      <- here("docs")
p_logs      <- here("logs")

# Derived data paths (from scripts 06a/06b)
p_derived   <- here("data_proc", "derived")
p_by_order  <- here("data_proc", "derived", "by_order")
p_by_family <- here("data_proc", "derived", "by_family")

# Gap analysis paths (from scripts 07-09)
p_gaps <- here("data_proc", "gaps")

# Output paths (from script 10)
p_tables     <- here("output", "tables")
p_integrated <- here("output", "tables", "integrated")

# ============================================================================
# Coordinate Reference System
# ============================================================================

# Default CRS for EEA grids: ETRS89-LAEA Europe (EPSG:3035)
# Override in config.yml via parameters.crs for non-EEA grids
CRS_PROJECT     <- cfg_get("parameters.crs", 3035)
CRS_LAEA        <- 3035
CRS_ETRS89_LAEA <- 3035

# Legacy alias for backward compatibility
CRS_SWEREF99TM <- 3006

# Configure sf package defaults
if (requireNamespace("sf", quietly = TRUE)) {
  sf::sf_use_s2(TRUE)
}

# ============================================================================
# Raw Data Directories (from config.yml with fallback defaults)
# ============================================================================

raw_gbif_cube_dir <- cfg_get(
  "paths.gbif_cube_dir",
  here(p_data_raw, "gbif_occurrence_cubes")
)

raw_grid_10km_dir <- cfg_get(
  "paths.grid_10km_dir",
  here(p_data_raw, "eea_grid_10km")
)

raw_grid_50km_dir <- cfg_get(
  "paths.grid_50km_dir",
  here(p_data_raw, "eea_grid_50km")
)

raw_redlist_dir <- cfg_get(
  "paths.redlist_dir",
  cfg_get(
    "paths.redlist_se_dir",
    here(p_data_raw, "red_list")
  )
)

# Backward-compatible alias
raw_redlist_se_dir <- raw_redlist_dir

raw_redlist_iucn_dir <- cfg_get(
  "paths.redlist_iucn_dir",
  here(p_data_raw, "red_list_iucn")
)

raw_taxonomy_dir <- cfg_get(
  "paths.taxonomy_dir",
  cfg_get(
    "paths.dyntaxa_dir",
    here(p_data_raw, "taxonomy")
  )
)

# Backward-compatible alias
raw_dyntaxa_dir <- raw_taxonomy_dir

# Optional file references from config
redlist_se_file  <- cfg_get("files.redlist_se", NULL)
redlist_iucn_file <- cfg_get("files.redlist_iucn", NULL)
dyntaxa_file     <- cfg_get("files.dyntaxa", NULL)

# ============================================================================
# Processed Output Paths
# ============================================================================

out_grid_10km_gpkg  <- here(p_data_proc, "grids_10km.gpkg")
out_grid_50km_gpkg  <- here(p_data_proc, "grids_50km.gpkg")

out_redlist_se_rds  <- here(p_data_proc, "red_list_se_current.rds")
out_redlist_iucn_rds <- here(p_data_proc, "red_list_iucn_current.rds")

out_dyntaxa_rds <- here(p_data_proc, "dyntaxa_current.rds")

# ============================================================================
# Utility Functions
# ============================================================================

#' Get current timestamp in standard format
#'
#' @return Character string "YYYY-MM-DD HH:MM:SS"
timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Log message with timestamp (legacy compatibility)
#'
#' @param ... Message components to paste
#' @note Prefer cli functions for new code
log_msg <- function(...) {
  cli_alert_info(paste0(..., collapse = ""))
}

# ============================================================================
# Grid Helper Functions
# ============================================================================

#' Guess which column contains the grid cell code
#'
#' Examines column names using a priority system:
#'   1. Fields matching both "eea" and "code" (most specific)
#'   2. Fields matching both "cell" and "code"
#'   3. Any field with keywords: eea, cell, code, grid
#'
#' @param field_names Character vector of column names
#' @return The best-matching field name, or `NA_character_`
#'
#' @examples
#' guess_cellcode_field(c("FID", "CELLCODE", "Shape_Area"))
#' # "CELLCODE"
#' guess_cellcode_field(c("id", "eea_cell_code", "geometry"))
#' # "eea_cell_code"
guess_cellcode_field <- function(field_names) {
  names_lower <- stringr::str_to_lower(field_names)

  # Priority 1: both "eea" and "code"
  idx <- stringr::str_detect(names_lower, "eea") &
    stringr::str_detect(names_lower, "code")
  if (any(idx)) return(field_names[which(idx)[1]])

  # Priority 2: both "cell" and "code"
  idx <- stringr::str_detect(names_lower, "cell") &
    stringr::str_detect(names_lower, "code")
  if (any(idx)) return(field_names[which(idx)[1]])

  # Priority 3: any relevant keyword
  idx <- stringr::str_detect(names_lower, "eea|cell|code|grid")
  if (any(idx)) return(field_names[which(idx)[1]])

  NA_character_
}

#' Standardise grid cell code column to 'eeacellcode'
#'
#' Renames the detected cell code column for consistency across
#' the pipeline. Works with both data.table and data.frame objects.
#'
#' @param dt A data.table or data.frame with a cell code column
#' @return The same object with standardised column name
standardize_cellcode <- function(dt) {
  current_name <- guess_cellcode_field(names(dt))

  if (!is.na(current_name) && current_name != "eeacellcode") {
    if (inherits(dt, "data.table")) {
      data.table::setnames(dt, current_name, "eeacellcode")
    } else {
      names(dt)[names(dt) == current_name] <- "eeacellcode"
    }
  }

  dt
}

# ============================================================================
# Directory Helpers
# ============================================================================

#' Check whether a raw-data directory exists (non-blocking)
#'
#' @param path Directory path
#' @param label Human-readable label for messages
check_raw_dir <- function(path, label) {
  if (!dir.exists(path)) {
    cli_alert_warning(
      "{label} directory not found: {.path {path}}"
    )
  }
}

# Quietly check all expected raw data directories
suppressMessages({
  check_raw_dir(raw_gbif_cube_dir, "GBIF cube")
  check_raw_dir(raw_grid_10km_dir, "10km grid")
  check_raw_dir(raw_grid_50km_dir, "50km grid")
  check_raw_dir(raw_redlist_dir,   "National red list")
  check_raw_dir(raw_taxonomy_dir,  "National taxonomy")
})

#' Create all derived/output directories if they don't exist
#'
#' @return Invisible NULL
ensure_dirs <- function() {
  dirs <- c(
    p_derived, p_by_order, p_by_family,
    p_gaps, p_tables, p_integrated
  )
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(NULL)
}
