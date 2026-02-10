# scripts/04_ingest_gbif_cubes.R
# ============================================================================
# GBIF Occurrence Cube Ingestion
# ============================================================================
# Purpose:
#   Read GBIF occurrence cube CSV files (10km and 50km grids), compute
#   summary statistics, optionally perform full ingestion, and write
#   processed cubes and metadata to data_proc/.
#
# Inputs:
#   - data_raw/gbif_occurrence_cubes/*.csv  (via config.yml)
#
# Outputs (in data_proc/):
#   - cubes/cube_<grid>_<basis>.fst    Processed cube files
#   - cube_manifest.csv                Ingestion log
#   - cube_totals_by_basisOfRecord.csv Quick totals
#
# Performance notes:
#   Uses data.table::fread() for speed on large CSVs (>1 GB).
#   Minimal column reads for totals; full ingest toggleable.
#
# Dependencies: scripts/00_setup.R, data.table, fst, dplyr
# ============================================================================

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(cli)
library(glue)

source(here("scripts", "00_setup.R"))

# ============================================================================
# Configuration
# ============================================================================

OUTPUT_FORMAT           <- "fst"
LARGE_FILE_THRESHOLD_GB <- 1.2
FULL_INGEST_LARGE_FILES <- TRUE

if (OUTPUT_FORMAT == "fst" &&
    !requireNamespace("fst", quietly = TRUE)) {
  cli_abort(c(
    "Package {.pkg fst} required for OUTPUT_FORMAT = 'fst'",
    "i" = "Install: {.code install.packages('fst')}"
  ))
}

# Paths
cube_dir      <- here(raw_gbif_cube_dir)
data_proc_dir <- here(cfg_get("paths.data_proc", "data_proc"))

cube_map <- cfg_get("files.cube_files")

if (is.null(cube_map) || length(cube_map) == 0) {
  cli_abort(c(
    "Cube configuration missing from config.yml",
    "i" = "Expected key: files.cube_files"
  ))
}
if (!dir.exists(cube_dir)) {
  cli_abort("Cube directory not found: {.path {cube_dir}}")
}

out_cube_dir  <- file.path(data_proc_dir, "cubes")
dir.create(out_cube_dir, showWarnings = FALSE, recursive = TRUE)

manifest_path <- file.path(data_proc_dir, "cube_manifest.csv")
totals_path   <- file.path(
  data_proc_dir, "cube_totals_by_basisOfRecord.csv"
)

# ============================================================================
# Helper Functions
# ============================================================================

#' Standardise data.table column names to lowercase
#'
#' @param dt A data.table
#' @return Same data.table with cleaned names (modified in place)
standardize_names_dt <- function(dt) {
  new_names <- names(dt) |>
    str_replace_all("\\s+", "_") |>
    str_replace_all("[^A-Za-z0-9_]", "") |>
    str_to_lower()
  setnames(dt, new_names)
  dt
}

#' Get file size in bytes
#'
#' @param path File path
#' @return Numeric size in bytes
file_size_bytes <- function(path) {
  as.numeric(file.info(path)$size)
}

#' Read essential columns from a cube file (fast, low memory)
#'
#' @param path Cube CSV path
#' @return data.table with minimal columns
read_cube_minimal <- function(path) {
  header       <- names(fread(path, nrows = 0, encoding = "UTF-8"))
  header_lower <- str_to_lower(header)

  col_occ  <- header[which(header_lower == "occurrences")[1]]
  col_ym   <- header[which(header_lower == "yearmonth")[1]]
  col_cell <- header[which(header_lower == "eeacellcode")[1]]

  if (is.na(col_occ)) {
    cli_abort(
      "'occurrences' column not found in: {.path {path}}"
    )
  }

  cols <- c(col_occ, col_ym, col_cell)
  cols <- cols[!is.na(cols)]

  fread(path, select = cols, showProgress = FALSE,
        encoding = "UTF-8")
}

#' Read full cube file
#'
#' @param path Cube CSV path
#' @return data.table with all columns
read_cube_full <- function(path) {
  fread(path, showProgress = TRUE, encoding = "UTF-8")
}

#' Safe sum with NA handling
#'
#' @param x Numeric vector
#' @return Sum as numeric
safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

#' Write cube to disk in the configured format
#'
#' @param dt        data.table to write
#' @param base_path Output path (without extension)
#' @param format    "fst" or "rds"
#' @return Full output path (with extension)
write_cube <- function(dt, base_path, format = OUTPUT_FORMAT) {
  if (format == "fst") {
    out_path <- paste0(base_path, ".fst")
    fst::write_fst(as.data.frame(dt), out_path, compress = 50)
  } else if (format == "rds") {
    out_path <- paste0(base_path, ".rds")
    saveRDS(dt, out_path, compress = "xz")
  } else {
    cli_abort("Unknown OUTPUT_FORMAT: {format}")
  }
  out_path
}

# ============================================================================
# Process Cube Files
# ============================================================================

cli_h2("Processing GBIF Occurrence Cubes")

manifest_list <- list()
totals_list   <- list()

for (grid_name in names(cube_map)) {
  cli_h3("Grid: {grid_name}")

  basis_list <- cube_map[[grid_name]]

  for (basis_name in names(basis_list)) {
    filename <- basis_list[[basis_name]]
    filepath <- file.path(cube_dir, filename)

    if (!file.exists(filepath)) {
      cli_abort("Cube file not found: {.path {filepath}}")
    }

    size_bytes <- file_size_bytes(filepath)
    size_gb    <- size_bytes / 1024^3

    cli_alert_info(
      "{.strong {basis_name}}: {.path {filename}} ({round(size_gb, 2)} GB)"
    )

    # --- Step 1: Always compute totals (minimal read) ---
    mini <- read_cube_minimal(filepath) |>
      standardize_names_dt()

    total_occ <- if ("occurrences" %in% names(mini)) {
      safe_sum(mini$occurrences)
    } else {
      NA_real_
    }

    min_ym <- if ("yearmonth" %in% names(mini)) {
      suppressWarnings(min(mini$yearmonth, na.rm = TRUE))
    } else {
      NA
    }

    max_ym <- if ("yearmonth" %in% names(mini)) {
      suppressWarnings(max(mini$yearmonth, na.rm = TRUE))
    } else {
      NA
    }

    totals_list[[length(totals_list) + 1]] <- tibble(
      grid              = grid_name,
      basisOfRecord     = basis_name,
      source_file       = filename,
      file_size_gb      = round(size_gb, 3),
      total_occurrences = total_occ,
      min_yearmonth     = as.character(min_ym),
      max_yearmonth     = as.character(max_ym)
    )

    # --- Step 2: Decide on full ingestion ---
    is_large      <- size_bytes >= (LARGE_FILE_THRESHOLD_GB * 1024^3)
    do_full       <- !is_large || FULL_INGEST_LARGE_FILES

    if (is_large && !FULL_INGEST_LARGE_FILES) {
      cli_alert_warning(
        "Skipping full ingest (large file). Set FULL_INGEST_LARGE_FILES=TRUE to include."
      )
    }

    output_path <- NA_character_
    n_rows      <- nrow(mini)
    n_cols      <- ncol(mini)

    # --- Step 3: Full ingestion (if enabled) ---
    if (do_full) {
      dt <- read_cube_full(filepath) |>
        standardize_names_dt()

      # Add provenance columns (data.table := for speed)
      dt[, `:=`(
        grid          = grid_name,
        basisofrecord = basis_name,
        source_file   = filename
      )]

      n_rows <- nrow(dt)
      n_cols <- ncol(dt)

      base_path <- file.path(
        out_cube_dir,
        glue("cube_{grid_name}_{basis_name}")
      )
      output_path <- write_cube(dt, base_path)

      cli_alert_success(
        "Written: {.path {basename(output_path)}} ({scales::comma(n_rows)} rows)"
      )

      rm(dt)
    }

    manifest_list[[length(manifest_list) + 1]] <- tibble(
      grid           = grid_name,
      basisOfRecord  = basis_name,
      source_file    = filename,
      file_size_gb   = round(size_gb, 3),
      full_ingest    = do_full,
      output_format  = OUTPUT_FORMAT,
      processed_file = output_path,
      rows           = n_rows,
      cols           = n_cols
    )

    rm(mini)
    invisible(gc())
  }
}

# ============================================================================
# Write Metadata
# ============================================================================

cli_h2("Writing Metadata")

manifest_df <- bind_rows(manifest_list)
totals_df   <- bind_rows(totals_list)

write_csv(manifest_df, manifest_path)
cli_alert_success("Manifest: {.path {manifest_path}}")

write_csv(totals_df, totals_path)
cli_alert_success("Totals: {.path {totals_path}}")

# ============================================================================
# Summary
# ============================================================================

cli_h2("Ingestion Summary")

summary_stats <- manifest_df |>
  summarise(
    n_files          = n(),
    n_fully_ingested = sum(full_ingest),
    total_rows       = sum(rows, na.rm = TRUE),
    total_size_gb    = sum(file_size_gb)
  )

cli_alert_info("Files processed: {summary_stats$n_files}")
cli_alert_info("Fully ingested:  {summary_stats$n_fully_ingested}")
cli_alert_info(
  "Total rows: {scales::comma(summary_stats$total_rows)}"
)
cli_alert_info(
  "Total size: {round(summary_stats$total_size_gb, 2)} GB"
)

cli_alert_success("GBIF cube ingestion complete!")
cli_alert_info("Next: source('scripts/05_validate_inputs.R')")
