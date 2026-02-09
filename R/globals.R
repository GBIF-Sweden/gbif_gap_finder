# R/globals.R
# ==============================================================================
# Project-Wide Configuration, Paths, and Constants
# ==============================================================================
# This file defines:
# - Project directory paths
# - Configuration loading utilities
# - Standard CRS (EPSG:3035 for EEA grids)
# - Global options and settings
#
# Sourced by scripts/00_setup.R

library(here)
library(cli)

# Global options ----------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(TZ = "Europe/Stockholm")

# Project paths -----------------------------------------------------------
p_root      <- here()
p_R         <- here("R")
p_scripts   <- here("scripts")
p_analysis  <- here("analysis")

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
p_gaps      <- here("data_proc", "gaps")

# Output paths (from script 10)
p_tables    <- here("output", "tables")
p_integrated <- here("output", "tables", "integrated")

# Configuration loading ---------------------------------------------------

#' Read YAML configuration file
#' @param path Path to config.yml
#' @return List of configuration values
read_config <- function(path = here("config.yml")) {
  if (!file.exists(path)) {
    cli_alert_warning(
      "config.yml not found at: {.path {path}}"
    )
    cli_alert_info("Using default paths")
    return(list())
  }
  
  if (!requireNamespace("yaml", quietly = TRUE)) {
    cli_abort(
      c(
        "Package {.pkg yaml} is required to read config.yml",
        "i" = "Install with: {.code install.packages('yaml')}"
      )
    )
  }
  
  yaml::read_yaml(path)
}

# Load configuration
cfg <- read_config()

#' Get nested configuration value
#' @param name Dotted path to config value (e.g., "paths.data_proc")
#' @param default Default value if not found
#' @return Configuration value or default
cfg_get <- function(name, default = NULL) {
  keys <- strsplit(name, ".", fixed = TRUE)[[1]]
  
  result <- cfg
  for (key in keys) {
    if (is.null(result) || is.null(result[[key]])) {
      return(default)
    }
    result <- result[[key]]
  }
  
  result
}

# Coordinate Reference System (CRS) ---------------------------------------
# Standard CRS for EEA grids: ETRS89-LAEA Europe (EPSG:3035)
CRS_PROJECT     <- 3035
CRS_LAEA        <- 3035
CRS_ETRS89_LAEA <- 3035  # Explicit name alias

# Legacy alias for backwards compatibility
CRS_SWEREF99TM <- 3006  # Swedish national CRS (not used for EEA grids)

# Configure sf package defaults
if (requireNamespace("sf", quietly = TRUE)) {
  sf::sf_use_s2(TRUE)  # Use spherical geometry by default
}

# Raw data directories ----------------------------------------------------
# These are configured via config.yml with fallback defaults

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

raw_redlist_se_dir <- cfg_get(
  "paths.redlist_se_dir",
  here(p_data_raw, "red_list_se")
)

raw_redlist_iucn_dir <- cfg_get(
  "paths.redlist_iucn_dir",
  here(p_data_raw, "red_list_iucn")
)

raw_dyntaxa_dir <- cfg_get(
  "paths.dyntaxa_dir",
  here(p_data_raw, "dyntaxa")
)

# Optional file references from config
redlist_se_file <- cfg_get("files.redlist_se", NULL)
redlist_iucn_file <- cfg_get("files.redlist_iucn", NULL)
dyntaxa_file <- cfg_get("files.dyntaxa", NULL)

# Processed output paths --------------------------------------------------
# Standard output locations for processed data

out_grid_10km_gpkg <- here(p_data_proc, "grids_10km.gpkg")
out_grid_50km_gpkg <- here(p_data_proc, "grids_50km.gpkg")

out_redlist_se_rds <- here(p_data_proc, "red_list_se_current.rds")
out_redlist_iucn_rds <- here(p_data_proc, "red_list_iucn_current.rds")

out_dyntaxa_rds <- here(p_data_proc, "dyntaxa_current.rds")

# Utility functions -------------------------------------------------------

#' Get current timestamp in standard format
#' @return Character timestamp
timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Log message with timestamp (legacy compatibility)
#' @param ... Message components to paste
#' @note Consider using cli functions instead for new code
log_msg <- function(...) {
  cli_alert_info(paste0(..., collapse = ""))
}

# Grid helper functions ----------------------------------------------------

#' Guess which field contains the EEA cell code
#' 
#' Examines column names to identify the most likely EEA cell code field.
#' Uses a priority system: (1) fields with both "eea" and "code", 
#' (2) fields with "cell" and "code", (3) any field with relevant keywords.
#' 
#' @param field_names Character vector of column names from a grid dataset
#' @return Character: the best-matching field name, or NA_character_ if none found
#' @examples
#' guess_cellcode_field(c("FID", "CELLCODE", "Shape_Area"))
#' # Returns: "CELLCODE"
#' guess_cellcode_field(c("id", "eea_cell_code", "geometry"))
#' # Returns: "eea_cell_code"
guess_cellcode_field <- function(field_names) {
  names_lower <- stringr::str_to_lower(field_names)
  
  # Priority 1: Fields with both "eea" and "code" (most specific)
  candidates <- field_names[
    stringr::str_detect(names_lower, "eea") & 
      stringr::str_detect(names_lower, "code")
  ]
  if (length(candidates) > 0) {
    return(candidates[1])
  }
  
  # Priority 2: Fields with "cell" and "code"
  candidates <- field_names[
    stringr::str_detect(names_lower, "cell") & 
      stringr::str_detect(names_lower, "code")
  ]
  if (length(candidates) > 0) {
    return(candidates[1])
  }
  
  # Priority 3: Any field with relevant keywords
  candidates <- field_names[
    stringr::str_detect(names_lower, "eea|cell|code|grid")
  ]
  if (length(candidates) > 0) {
    return(candidates[1])
  }
  
  NA_character_
}

#' Standardize EEA cell code column name
#'
#' Renames the detected cell code column to 'eeacellcode' for consistency.
#' 
#' @param dt A data.table or data.frame with a cell code column
#' @return The same object with standardized column name
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

# Directory validation (non-blocking) -------------------------------------
# Check for expected raw data directories and notify if missing
# This does not stop execution, just informs the user

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
  check_raw_dir(raw_redlist_se_dir, "Swedish Red List")
  check_raw_dir(raw_redlist_iucn_dir, "IUCN Red List")
  check_raw_dir(raw_dyntaxa_dir, "Dyntaxa")
})

# Create derived directories if they don't exist --------------------------
ensure_dirs <- function() {
  dirs <- c(p_derived, p_by_order, p_by_family, p_gaps, p_tables, p_integrated)
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
}
