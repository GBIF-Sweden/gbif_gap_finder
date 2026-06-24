# R/globals.R
# ============================================================================
# Project-Wide Configuration, Paths, Constants, and Shared Utilities
# ============================================================================
# Purpose:
#   Central configuration loader and shared utility library for the
#   GBIF gap finder pipeline.  All paths are derived from the country
#   code in the config file.  Scripts, R functions, Rmd templates, and
#   Shiny apps are shared across countries — only the data directories
#   differ.
#
#   This file also contains every helper function that is used by more
#   than one pipeline script (cube readers, file readers, filename
#   sanitisers, etc.) so that individual scripts never redefine them.
#
# Sourced by: scripts/00_setup.R (and transitively by every script)
#
# Dependencies (loaded by R/packages.R before this file is sourced):
#   - here, cli, yaml, stringr, data.table, arrow (optional), sf (optional)
# ============================================================================

# Null-coalescing operator (safe to define once; rlang also exports this)
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# Configuration Loading
# ============================================================================

#' Read YAML configuration file
#'
#' Searches for config in this order:
#'   1. configs/config_{GBIF_GAP_COUNTRY}.yml (env var override)
#'   2. configs/config_SE.yml (default)
#'   3. config.yml (legacy, project root)
#'
#' @return List of configuration values
read_config <- function() {
  # Check for environment variable override
  country_env <- Sys.getenv("GBIF_GAP_COUNTRY", "")

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
    p_data_raw, p_data_proc, p_output, p_logs,
    p_derived, p_by_order, p_by_family,
    p_gaps, p_cubes, p_tables, p_integrated,
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
# Grid Completion (zero-fill to the full grid)
# ============================================================================

#' Complete a per-cell count table to the FULL grid universe and flag gaps.
#'
#' Both 07 (full cube) and 09c (scope-filtered cube) need the same operation:
#' take counts keyed by cell (+ basis, + grid), expand to every grid cell so
#' empty cells are present as explicit zeros, then flag `has_data`/`gap_zero`.
#' Centralising it here guarantees the two never drift, and — crucially — the
#' full grid is REQUIRED: there is no silent fall-back to "cells that appear in
#' the data", because that collapses the denominator onto the numerator and
#' reports ~100 % coverage with zero empty cells (the bug that made the app's
#' Spatial/Overview disagree with the Priorities zero-cell count).
#'
#' @param counts        data.table/data.frame of counts. Must contain `cell_col`,
#'                      every column in `facet_cols`, and the `value_cols`.
#' @param all_cellcodes Character vector of EVERY cell in the grid (from the grid
#'                      file, not from the data). Required and non-empty.
#' @param facet_cols    Columns crossed with the grid cells (e.g. "basisofrecord",
#'                      or c("grid", "basisofrecord")). Each existing level is kept.
#' @param cell_col      Name of the cell-code column (default "eeacellcode").
#' @param value_cols    Count columns zero-filled on completion.
#' @return data.table with one row per (cell x facet combo), zeros filled, plus
#'         logical `has_data` (occurrences > 0) and `gap_zero` (occurrences == 0).
complete_to_grid <- function(counts, all_cellcodes,
                             facet_cols = "basisofrecord",
                             cell_col   = "eeacellcode",
                             value_cols = c("occurrences", "n_species")) {
  if (is.null(all_cellcodes) || length(all_cellcodes) == 0L) {
    cli_abort(c(
      "complete_to_grid(): the full grid cell list is empty or missing.",
      "i" = "Coverage and zero-cell counts are measured against the whole grid.",
      "x" = "Refusing to fall back to data-only cells (that reports ~100% coverage \\
             and hides every empty cell). Check the grid GeoPackage exists and \\
             that {.fn st_read} returned a cell-code column."
    ))
  }
  counts        <- data.table::as.data.table(data.table::copy(counts))
  all_cellcodes <- unique(as.character(all_cellcodes))

  # Universe = every grid cell x every existing combination of the facet levels.
  facet_levels <- lapply(facet_cols, function(fc) unique(counts[[fc]]))
  universe_args <- c(stats::setNames(list(all_cellcodes), cell_col),
                     stats::setNames(facet_levels, facet_cols),
                     list(unique = TRUE))
  universe <- do.call(data.table::CJ, universe_args)

  key_cols <- c(cell_col, facet_cols)
  keep     <- c(key_cols, intersect(value_cols, names(counts)))
  complete <- merge(universe, counts[, ..keep], by = key_cols, all.x = TRUE)

  for (vc in value_cols) {
    if (vc %in% names(complete)) complete[is.na(get(vc)), (vc) := 0]
  }
  complete[, has_data := occurrences > 0]
  complete[, gap_zero := occurrences == 0]
  complete[]
}

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

# ============================================================================
# Shared Cube Reader
# ============================================================================
# Consolidated cube-reading logic used by 06a, 06b, 08, 09c.
# Each script used to define its own variant; now they all call this.

#' Read a parquet cube into data.table
#'
#' Opens a parquet file via arrow, optionally selects columns, creates
#' a yearmonth integer column from year + month, and optionally recodes
#' missing higher-taxonomy values to "Unplaced".
#'
#' @param parquet_path Path to parquet file
#' @param cols         Character vector of columns to select (NULL = all)
#' @param grid_label   Optional grid label string added as a `grid` column
#' @param recode_taxonomy If TRUE, recode NA/empty order/family/class to
#'                        "Unplaced" (default TRUE)
#' @return A data.table
read_cube <- function(parquet_path, cols = NULL, grid_label = NULL,
                      recode_taxonomy = TRUE) {
  if (!file.exists(parquet_path)) cli_abort("Not found: {.path {parquet_path}}")
  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli_abort("Package {.pkg arrow} is required to read parquet cubes")
  }

  ds <- arrow::open_dataset(parquet_path)
  if (!is.null(cols)) {
    available <- intersect(cols, names(ds$schema))
    ds <- ds |> dplyr::select(dplyr::all_of(available))
  }
  dt <- data.table::as.data.table(dplyr::collect(ds))

  # Create yearmonth from year + month (NA-safe)
  if (all(c("year", "month") %in% names(dt)) && !"yearmonth" %in% names(dt)) {
    dt[, yearmonth := fifelse(
      !is.na(year) & !is.na(month),
      as.integer(year) * 100L + as.integer(month),
      NA_integer_
    )]
  }

  # Add grid label
  if (!is.null(grid_label)) dt[, grid := grid_label]

  # Recode missing taxonomy
  if (recode_taxonomy) {
    if ("order"  %in% names(dt)) dt[is.na(order)  | order  == "", order  := "Unplaced"]
    if ("family" %in% names(dt)) dt[is.na(family) | family == "", family := "Unplaced"]
    if ("class"  %in% names(dt)) dt[is.na(class)  | class  == "", class  := "Unplaced"]
  }

  dt
}

# ============================================================================
# Shared File Readers
# ============================================================================

#' Read a derived summary CSV safely
#'
#' Checks file existence, reads with fread, validates required columns,
#' and adds a grid column if missing (inferred from filename).
#'
#' @param filename  Filename relative to p_derived
#' @param required_cols  Character vector of required column names
#' @param base_dir  Base directory (default p_derived)
#' @return A data.table
read_derived_summary <- function(filename, required_cols = NULL,
                                 base_dir = p_derived) {
  path <- here::here(base_dir, filename)
  if (!file.exists(path)) cli_abort("File not found: {.path {path}}")

  dt <- data.table::fread(path)

  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(dt))
    if (length(missing_cols) > 0) {
      cli_abort(c(
        "Missing columns in {.path {filename}}",
        "x" = "Missing: {paste(missing_cols, collapse = ', ')}"
      ))
    }
  }

  # Add grid column if missing (infer from filename)
  if (!("grid" %in% names(dt))) {
    grid_suffix <- stringr::str_extract(filename, "\\d+km")
    if (!is.na(grid_suffix)) dt[, grid := paste0("grid", grid_suffix)]
  }

  dt
}

#' Safely read a file, returning NULL on missing/error
#'
#' @param path Full file path
#' @param type "csv" or "rds"
#' @return Data or NULL
safe_read <- function(path, type = "csv") {
  if (!file.exists(path)) return(NULL)
  tryCatch({
    if (type == "csv") data.table::fread(path)
    else if (type == "rds") readRDS(path)
  }, error = function(e) {
    cli_alert_warning("Error reading {basename(path)}: {e$message}")
    NULL
  })
}

# ============================================================================
# Shared Filename Utility
# ============================================================================

#' Sanitise a string for use in filenames
#'
#' Replaces non-alphanumeric characters with underscores and collapses
#' runs of underscores.
#' @param x Character string
#' @return Sanitised string
clean_for_filename <- function(x) {
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9]", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  stringr::str_remove(x, "^_|_$")
}

# ============================================================================
# Taxonomy Helpers
# ============================================================================

#' Classify backbone taxa as accepted vs synonym (NA-safe)
#'
#' Uses taxonomicStatus as the primary signal, falling back to the
#' taxonID == acceptedNameUsageID self-reference convention.
#' This avoids the pitfall where acceptedNameUsageID is NA for accepted
#' taxa, which would produce is_accepted = NA and silently drop them.
#'
#' @param dt A data.table with at least taxonID; optionally
#'           taxonomicStatus and acceptedNameUsageID
#' @return The input data.table with an `is_accepted` logical column added
classify_accepted <- function(dt) {
  if ("taxonomicStatus" %in% names(dt)) {
    # Primary signal: taxonomicStatus (normalised to "accepted" by 03)
    dt[, is_accepted := (taxonomicStatus == "accepted")]
    # NA taxonomicStatus: fall back to ID comparison
    dt[is.na(is_accepted) & "acceptedNameUsageID" %in% names(dt),
       is_accepted := (!is.na(acceptedNameUsageID) & taxonID == acceptedNameUsageID)]
    # Still NA: fall back to TRUE if acceptedNameUsageID is NA/empty
    # (many checklists leave this blank for accepted taxa)
    dt[is.na(is_accepted), is_accepted := (
      !("acceptedNameUsageID" %in% names(dt)) |
        is.na(acceptedNameUsageID) |
        acceptedNameUsageID == ""
    )]
  } else if ("acceptedNameUsageID" %in% names(dt)) {
    # No taxonomicStatus: use ID comparison, treating NA as accepted
    dt[, is_accepted := (
      is.na(acceptedNameUsageID) |
        acceptedNameUsageID == "" |
        taxonID == acceptedNameUsageID
    )]
  } else {
    # No status info at all: assume all accepted
    cli_alert_warning("No taxonomicStatus or acceptedNameUsageID — assuming all taxa are accepted")
    dt[, is_accepted := TRUE]
  }
  dt
}

#' Resolve threat status from multiple columns (first non-NA per row)
#'
#' Uses data.table::fcoalesce() to pick the first non-NA value across
#' the specified columns, in priority order.
#'
#' @param dt   A data.table
#' @param cols Character vector of column names to coalesce, in priority order
#' @return The input data.table with a `threatStatus` column added/updated
resolve_threat_status <- function(dt, cols = c("threatStatus_redlist",
                                               "threatStatus_backbone")) {
  available <- intersect(cols, names(dt))
  if (length(available) == 0) {
    dt[, threatStatus := NA_character_]
    cli_alert_warning("No threat status columns found")
    return(dt)
  }
  if (length(available) == 1) {
    dt[, threatStatus := get(available)]
  } else {
    # fcoalesce picks the first non-NA value across columns, per row
    dt[, threatStatus := do.call(data.table::fcoalesce, .SD),
       .SDcols = available]
  }
  cli_alert_info("Threat status resolved from: {paste(available, collapse = ' > ')}")
  dt
}
