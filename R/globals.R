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
  here(p_data_raw, "redlist")
)

raw_taxonomy_dir <- cfg_get(
  "paths.taxonomy_dir",
  here(p_data_raw, "taxonomy")
)

# ============================================================================
# Processed Output Paths
# ============================================================================

out_grid_10km_gpkg  <- here(p_data_proc, "grids_10km.gpkg")
out_grid_50km_gpkg  <- here(p_data_proc, "grids_50km.gpkg")

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

# ============================================================================
# tar_render() Placeholders
# ============================================================================
# tarchetypes::tar_render() scans Rmd chunk options at plan-definition time.
# Some Rmds use eval=has_threat_data or eval=!is.null(grid10) in chunk headers.
# These are properly defined inside the Rmds at render time, but tar_render()
# needs them to exist when scanning. Defining them here (before _targets.R is
# parsed) avoids spurious error messages at startup.
has_threat_data <- TRUE
grid10          <- NULL

# ============================================================================
# Schema Validators
# ============================================================================
# These validators ensure that outputs from producer scripts match what
# consumer scripts expect. They catch column renames, type changes, and
# missing fields early — before they cause cryptic errors downstream.
#
# Called by: scripts/09a (reconciliation), scripts/09b (gap tables)
# ============================================================================

#' Validate a data frame against a schema
#'
#' @param dt        Data frame or data.table to validate
#' @param schema    Named list: name -> list(required, type)
#' @param label     Human-readable name for error messages
#' @return Invisible TRUE if valid, otherwise stops
validate_schema <- function(dt, schema, label = "dataset") {

  if (is.null(dt)) {
    cli_alert_danger("Validation failed: {label} is NULL")
    stop(paste0("Schema validation failed: ", label, " is NULL"), call. = FALSE)
  }

  if (nrow(dt) == 0) {
    cli_alert_warning("Validation warning: {label} has 0 rows")
  }

  col_names <- names(dt)
  issues <- character(0)

  for (col in names(schema)) {
    spec     <- schema[[col]]
    required <- isTRUE(spec$required)

    if (!col %in% col_names) {
      if (required) {
        issues <- c(issues, paste0("Missing required column: ", col))
      }
      next
    }

    # Type check
    if (!is.null(spec$type) && col %in% col_names) {
      actual <- class(dt[[col]])[1]
      ok <- actual %in% spec$type ||
        (any(spec$type == "numeric")   && actual %in% c("numeric", "integer", "double")) ||
        (any(spec$type == "character") && actual %in% c("character", "factor"))
      if (!ok) {
        issues <- c(issues, paste0(
          col, ": expected ", paste(spec$type, collapse = "/"), " but got ", actual
        ))
      }
    }
  }

  if (length(issues) > 0) {
    cli_alert_danger("Schema validation failed for {label}:")
    for (issue in issues) cli_alert_warning("  {issue}")
    stop(
      paste0("Schema validation failed for ", label, ":\n",
             paste("  -", issues, collapse = "\n")),
      call. = FALSE
    )
  }

  n_required <- sum(vapply(schema, function(s) isTRUE(s$required), logical(1)))
  n_present  <- sum(names(schema) %in% col_names)
  cli_alert_success(
    "Schema OK: {label} ({n_present}/{length(schema)} columns, {scales::comma(nrow(dt))} rows)"
  )
  invisible(TRUE)
}

# --- Reconciliation (09a output) -------------------------------------------
# Consumed by: 09b
schema_reconciliation <- list(
  specieskey              = list(required = TRUE,  type = "numeric"),
  species                 = list(required = TRUE,  type = "character"),
  total_occ               = list(required = TRUE,  type = "numeric"),
  backbone_taxonID        = list(required = TRUE,  type = "character"),
  backbone_scientificName = list(required = TRUE,  type = "character"),
  match_tier              = list(required = TRUE,  type = "character"),
  match_type              = list(required = TRUE,  type = "character"),
  match_name_used         = list(required = TRUE,  type = "character"),
  taxonRank               = list(required = FALSE, type = "character"),
  kingdom                 = list(required = FALSE, type = "character"),
  phylum                  = list(required = FALSE, type = "character"),
  class                   = list(required = FALSE, type = "character"),
  order                   = list(required = FALSE, type = "character"),
  family                  = list(required = FALSE, type = "character"),
  threatStatus_backbone   = list(required = FALSE, type = "character"),
  threatStatus_redlist    = list(required = FALSE, type = "character")
)

validate_reconciliation <- function(dt) {
  validate_schema(dt, schema_reconciliation, "reconciliation (09a)")
}

# --- Taxonomic match summary (09b output) ----------------------------------
# Consumed by: 10, 11, 12, Rmd reports
schema_match_summary <- list(
  taxonID          = list(required = TRUE,  type = "character"),
  scientificName   = list(required = TRUE,  type = "character"),
  taxonRank        = list(required = FALSE, type = "character"),
  kingdom          = list(required = FALSE, type = "character"),
  phylum           = list(required = FALSE, type = "character"),
  class            = list(required = FALSE, type = "character"),
  order            = list(required = FALSE, type = "character"),
  family           = list(required = FALSE, type = "character"),
  threatStatus     = list(required = FALSE, type = "character"),
  matched_any      = list(required = TRUE,  type = "logical"),
  n_gbif_species   = list(required = TRUE,  type = "numeric"),
  gbif_total_occ   = list(required = TRUE,  type = "numeric"),
  best_match_tier  = list(required = FALSE, type = "character"),
  gbif_specieskeys = list(required = FALSE, type = "character")
)

validate_match_summary <- function(dt) {
  validate_schema(dt, schema_match_summary, "taxonomic_match_summary (09b)")
}

# --- Missing threatened taxa (09b output) ----------------------------------
# Consumed by: 10, 11
schema_missing_threatened <- list(
  taxonID        = list(required = TRUE,  type = "character"),
  scientificName = list(required = TRUE,  type = "character"),
  threatStatus   = list(required = TRUE,  type = "character"),
  taxonRank      = list(required = FALSE, type = "character"),
  kingdom        = list(required = FALSE, type = "character"),
  phylum         = list(required = FALSE, type = "character"),
  class          = list(required = FALSE, type = "character"),
  order          = list(required = FALSE, type = "character"),
  family         = list(required = FALSE, type = "character")
)

validate_missing_threatened <- function(dt) {
  validate_schema(dt, schema_missing_threatened, "missing_threatened (09b)")
}

# --- Coverage by rank (09b output) -----------------------------------------
# Consumed by: 10, 11
schema_coverage_by_rank <- list(
  taxonRank    = list(required = TRUE, type = "character"),
  n_ref_total  = list(required = TRUE, type = "numeric"),
  n_in_gbif    = list(required = TRUE, type = "numeric"),
  pct_coverage = list(required = TRUE, type = "numeric"),
  n_missing    = list(required = TRUE, type = "numeric")
)

validate_coverage_by_rank <- function(dt) {
  validate_schema(dt, schema_coverage_by_rank, "coverage_by_rank (09b)")
}

# --- Coverage by threat status (09b output) --------------------------------
# Consumed by: 10
schema_coverage_by_threat <- list(
  threatStatus = list(required = TRUE, type = "character"),
  n_ref_total  = list(required = TRUE, type = "numeric"),
  n_in_gbif    = list(required = TRUE, type = "numeric"),
  pct_coverage = list(required = TRUE, type = "numeric"),
  n_missing    = list(required = TRUE, type = "numeric")
)

validate_coverage_by_threat <- function(dt) {
  validate_schema(dt, schema_coverage_by_threat, "coverage_by_threat (09b)")
}

# --- Spatial coverage per taxon (09b output) -------------------------------
# Consumed by: 10
schema_spatial_coverage <- list(
  taxonID        = list(required = TRUE,  type = "character"),
  scientificName = list(required = TRUE,  type = "character"),
  n_cells_10km   = list(required = TRUE,  type = "numeric"),
  n_cells_50km   = list(required = TRUE,  type = "numeric"),
  total_occ_10km = list(required = TRUE,  type = "numeric"),
  total_occ_50km = list(required = TRUE,  type = "numeric"),
  poorly_sampled = list(required = FALSE, type = "logical"),
  taxonRank      = list(required = FALSE, type = "character"),
  threatStatus   = list(required = FALSE, type = "character"),
  family         = list(required = FALSE, type = "character"),
  order          = list(required = FALSE, type = "character")
)

validate_spatial_coverage <- function(dt) {
  validate_schema(dt, schema_spatial_coverage, "spatial_coverage (09b)")
}
