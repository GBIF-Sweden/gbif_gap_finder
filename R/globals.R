# R/globals.R
# ============================================================================
# Project-Wide Configuration, Paths, and Constants
# ============================================================================
# Purpose:
#   Central configuration loader for the GBIF gap analysis pipeline.
#   All paths are derived from the country code in the config file.
#   Scripts, R functions, Rmd templates, and Shiny apps are shared
#   across countries — only the data directories differ.
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
#' Searches for config in this order:
#'   1. configs/config_{GBIFGAPS_COUNTRY}.yml (env var override)
#'   2. configs/config_SE.yml (default)
#'   3. config.yml (legacy, project root)
#'
#' @return List of configuration values
read_config <- function() {
  # Check for environment variable override
  country_env <- Sys.getenv("GBIFGAPS_COUNTRY", "")

  candidates <- c(
    if (nchar(country_env) > 0) here("configs", paste0("config_", country_env, ".yml")),
    here("configs", "config_SE.yml"),
    here("config.yml")  # legacy fallback
  )

  for (path in candidates) {
    if (file.exists(path)) {
      if (!requireNamespace("yaml", quietly = TRUE)) {
        cli_abort("Package {.pkg yaml} is required to read config")
      }
      cli_alert_info("Config: {.path {path}}")
      return(yaml::read_yaml(path))
    }
  }

  cli_alert_warning("No config file found — using defaults")
  return(list())
}

# Load configuration at source time
cfg <- read_config()

#' Get a nested configuration value by dotted path
#'
#' @param name Dotted key path (e.g., "paths.data_proc")
#' @param default Value returned when key is missing
#' @return Configuration value or `default`
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

# Country code — used to derive all data paths
COUNTRY_CODE <- cfg_get("country.code", "SE")

# ============================================================================
# Project Paths — Country-Aware
# ============================================================================
# Shared code directories (not country-specific)
p_root     <- here()
p_R        <- here("R")
p_scripts  <- here("scripts")
p_analysis <- here("analysis")
p_docs     <- here("docs")
p_configs  <- here("configs")

# Country-specific data directories
p_data     <- here("data", COUNTRY_CODE)
p_data_raw <- here("data", COUNTRY_CODE, "raw")
p_data_proc <- here("data", COUNTRY_CODE, "proc")
p_output   <- here("data", COUNTRY_CODE, "output")
p_logs     <- here("logs")

# Derived data paths (from scripts 06a/06b)
p_derived   <- here(p_data_proc, "derived")
p_by_order  <- here(p_data_proc, "derived", "by_order")
p_by_family <- here(p_data_proc, "derived", "by_family")

# Gap analysis paths (from scripts 07-09)
p_gaps <- here(p_data_proc, "gaps")

# Cube path (from script 04)
p_cubes <- here(p_data_proc, "cubes")

# Output paths (from script 10)
p_tables     <- here(p_output, "tables")
p_integrated <- here(p_output, "tables", "integrated")

# ============================================================================
# Coordinate Reference System
# ============================================================================

CRS_PROJECT     <- cfg_get("parameters.crs", 3035)
CRS_LAEA        <- 3035
CRS_ETRS89_LAEA <- 3035

if (requireNamespace("sf", quietly = TRUE)) {
  sf::sf_use_s2(TRUE)
}

# ============================================================================
# Raw Data Directories (derived from country path)
# ============================================================================

raw_gbif_cube_dir  <- cfg_get("paths.gbif_cube_dir",  here(p_data_raw, "cubes"))
raw_grid_dir       <- cfg_get("paths.grid_dir",       here("data", "shared", "grids"))
raw_redlist_dir    <- cfg_get("paths.redlist_dir",     here(p_data_raw, "redlist"))
raw_taxonomy_dir   <- cfg_get("paths.taxonomy_dir",    here(p_data_raw, "taxonomy"))
raw_invasives_dir  <- cfg_get("paths.invasives_dir",   here(p_data_raw, "invasives"))
raw_sensitive_dir  <- cfg_get("paths.sensitive_dir",   here(p_data_raw, "sensitive"))
raw_admin_dir      <- cfg_get("paths.admin_dir",       here(p_data_raw, "admin"))

# ============================================================================
# Processed Output Paths
# ============================================================================

out_grid_10km_gpkg  <- here(p_data_proc, "grids_10km.gpkg")
out_grid_50km_gpkg  <- here(p_data_proc, "grids_50km.gpkg")

# ============================================================================
# Utility Functions
# ============================================================================

timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Safe sum: coerce to numeric, handle NAs
safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

#' Safe max: coerce to numeric, handle all-NA
safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(as.numeric(x), na.rm = TRUE)
}

#' Parse yearmonth to Date
#'
#' Handles: "2025-01" (string), 202501 (6-digit int), 20251 (5-digit int)
#' @param x Character or integer vector of yearmonth values
#' @return Date vector (first of month)
parse_yearmonth <- function(x) {
  x_chr <- stringr::str_trim(as.character(x))
  out <- rep(as.Date(NA), length(x_chr))

  valid_input <- !is.na(x_chr) & x_chr != "" & x_chr != "NA"
  if (!any(valid_input)) return(out)

  # Format: "2025-01"
  fmt_dash <- valid_input & stringr::str_detect(x_chr, "^[0-9]{4}-[0-9]{2}$")
  if (any(fmt_dash)) {
    out[fmt_dash] <- as.Date(paste0(x_chr[fmt_dash], "-01"))
  }

  # Format: "202501" (6 digits)
  fmt6 <- valid_input & !fmt_dash & stringr::str_detect(x_chr, "^[0-9]{6}$")
  if (any(fmt6)) {
    yr <- substr(x_chr[fmt6], 1, 4)
    mo <- substr(x_chr[fmt6], 5, 6)
    out[fmt6] <- as.Date(paste0(yr, "-", mo, "-01"))
  }

  # Format: "20251" (5 digits — single-digit month)
  fmt5 <- valid_input & !fmt_dash & !fmt6 & stringr::str_detect(x_chr, "^[0-9]{5}$")
  if (any(fmt5)) {
    yr <- substr(x_chr[fmt5], 1, 4)
    mo <- substr(x_chr[fmt5], 5, 5)
    out[fmt5] <- as.Date(paste0(yr, "-0", mo, "-01"))
  }

  out
}

#' Extract year from yearmonth (integer)
#'
#' Handles "2025-01" and 202501 formats, returns integer year.
#' @param x Character or integer vector of yearmonth values
#' @return Integer vector of years
parse_year <- function(x) {
  x_chr <- stringr::str_trim(as.character(x))
  out <- rep(NA_integer_, length(x_chr))

  not_na <- !is.na(x_chr) & x_chr != "" & x_chr != "NA"

  valid_dash <- not_na & stringr::str_detect(x_chr, "^[0-9]{4}-[0-9]{2}$")
  out[valid_dash] <- as.integer(stringr::str_sub(x_chr[valid_dash], 1, 4))

  valid_int <- not_na & !valid_dash & stringr::str_detect(x_chr, "^[0-9]{5,6}$")
  out[valid_int] <- as.integer(substr(x_chr[valid_int], 1, 4))

  out
}

# ============================================================================
# Grid Helper Functions
# ============================================================================

guess_cellcode_field <- function(field_names) {
  names_lower <- stringr::str_to_lower(field_names)

  idx <- stringr::str_detect(names_lower, "eea") &
    stringr::str_detect(names_lower, "code")
  if (any(idx)) return(field_names[which(idx)[1]])

  idx <- stringr::str_detect(names_lower, "cell") &
    stringr::str_detect(names_lower, "code")
  if (any(idx)) return(field_names[which(idx)[1]])

  idx <- stringr::str_detect(names_lower, "eea|cell|code|grid")
  if (any(idx)) return(field_names[which(idx)[1]])

  NA_character_
}

#' Standardise grid cell code column to 'eeacellcode'
standardise_cellcode <- function(df) {
  if ("eeacellcode" %in% names(df)) return(df)
  field <- guess_cellcode_field(names(df))
  if (is.na(field)) {
    cli_alert_warning("Could not detect cell code column")
    return(df)
  }
  cli_alert_info("Renaming {.field {field}} → eeacellcode")
  if (inherits(df, "data.table")) {
    data.table::setnames(df, field, "eeacellcode")
  } else {
    names(df)[names(df) == field] <- "eeacellcode"
  }
  df
}

# Print country at load time (00_setup.R handles the detailed path listing)
cli_alert_info("Country: {cfg_get('country.name', COUNTRY_CODE)} ({COUNTRY_CODE})")

#' Create all derived/output directories if they don't exist
ensure_dirs <- function() {
  dirs <- c(
    p_data_raw, p_data_proc, p_output,
    p_derived, p_by_order, p_by_family,
    p_gaps, p_tables, p_integrated,
    raw_gbif_cube_dir, raw_grid_dir,
    raw_redlist_dir, raw_taxonomy_dir,
    raw_invasives_dir, raw_sensitive_dir, raw_admin_dir
  )
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(NULL)
}

# ============================================================================
# Rmd Placeholders (referenced by analysis/*.Rmd, resolved by tar_render)
# ============================================================================
has_threat_data <- TRUE
grid10          <- NULL

# ============================================================================
# Schema Validators
# ============================================================================

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
      if (required) issues <- c(issues, paste0("Missing required column: ", col))
      next
    }
    if (!is.null(spec$type) && col %in% col_names) {
      actual <- class(dt[[col]])[1]
      ok <- actual %in% spec$type ||
        (any(spec$type == "numeric")   && actual %in% c("numeric", "integer", "double")) ||
        (any(spec$type == "character") && actual %in% c("character", "factor"))
      if (!ok) {
        issues <- c(issues, paste0(col, ": expected ", paste(spec$type, collapse = "/"), " but got ", actual))
      }
    }
  }
  if (length(issues) > 0) {
    cli_alert_danger("Schema validation failed for {label}:")
    for (issue in issues) cli_alert_warning("  {issue}")
    stop(paste0("Schema validation failed for ", label, ":\n",
                paste("  -", issues, collapse = "\n")), call. = FALSE)
  }
  n_required <- sum(vapply(schema, function(s) isTRUE(s$required), logical(1)))
  n_present  <- sum(names(schema) %in% col_names)
  cli_alert_success("Schema OK: {label} ({n_present}/{length(schema)} columns, {scales::comma(nrow(dt))} rows)")
  invisible(TRUE)
}

# --- Schema definitions (unchanged) ---
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
validate_reconciliation <- function(dt) validate_schema(dt, schema_reconciliation, "reconciliation (09a)")

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
validate_match_summary <- function(dt) validate_schema(dt, schema_match_summary, "taxonomic_match_summary (09b)")

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
validate_missing_threatened <- function(dt) validate_schema(dt, schema_missing_threatened, "missing_threatened (09b)")

schema_coverage_by_rank <- list(
  taxonRank    = list(required = TRUE, type = "character"),
  n_ref_total  = list(required = TRUE, type = "numeric"),
  n_in_gbif    = list(required = TRUE, type = "numeric"),
  pct_coverage = list(required = TRUE, type = "numeric"),
  n_missing    = list(required = TRUE, type = "numeric")
)
validate_coverage_by_rank <- function(dt) validate_schema(dt, schema_coverage_by_rank, "coverage_by_rank (09b)")

schema_coverage_by_threat <- list(
  threatStatus = list(required = TRUE, type = "character"),
  n_ref_total  = list(required = TRUE, type = "numeric"),
  n_in_gbif    = list(required = TRUE, type = "numeric"),
  pct_coverage = list(required = TRUE, type = "numeric"),
  n_missing    = list(required = TRUE, type = "numeric")
)
validate_coverage_by_threat <- function(dt) validate_schema(dt, schema_coverage_by_threat, "coverage_by_threat (09b)")

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
validate_spatial_coverage <- function(dt) validate_schema(dt, schema_spatial_coverage, "spatial_coverage (09b)")
