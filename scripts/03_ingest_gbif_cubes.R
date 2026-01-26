# scripts/03_ingest_gbif_cubes.R
# ==============================================================================
# GBIF Occurrence Cube Ingestion
# ==============================================================================
# This script:
# - Reads GBIF occurrence cube CSV files (10km and 50km grids)
# - Computes summary statistics for all files (memory-efficient)
# - Optionally performs full ingestion for detailed analysis
# - Writes processed cubes and metadata to data_proc/
#
# Large file handling:
# - Always computes totals via minimal read (fast, low memory)
# - Full ingest can be toggled for large files (>1.2 GB)

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

# Configuration -----------------------------------------------------------

# Output format: "fst" (fast I/O) or "rds" (base R compatible)
OUTPUT_FORMAT <- "fst"

# Large file threshold and behavior
LARGE_FILE_THRESHOLD_GB <- 1.2
FULL_INGEST_LARGE_FILES <- TRUE  # Set FALSE to skip full ingest of large files

# Validate fst availability if selected
if (OUTPUT_FORMAT == "fst" && !requireNamespace("fst", quietly = TRUE)) {
  cli_abort(
    c(
      "Package {.pkg fst} is required for OUTPUT_FORMAT = 'fst'",
      "i" = "Install with: {.code install.packages('fst')}",
      "i" = "Then run: {.code renv::snapshot()}"
    )
  )
}

# Get cube configuration from YAML ----------------------------------------
cube_dir <- here(cfg_get("paths.gbif_cube_dir"))
cube_map <- cfg_get("files.cube_files")
data_proc_dir <- here(cfg_get("paths.data_proc", "data_proc"))

# Validate configuration
if (is.null(cube_map) || length(cube_map) == 0) {
  cli_abort(
    c(
      "Cube configuration missing from config.yml",
      "i" = "Expected keys: paths.gbif_cube_dir, files.cube_files"
    )
  )
}

if (!dir.exists(cube_dir)) {
  cli_abort("Cube directory not found: {.path {cube_dir}}")
}

# Create output directories -----------------------------------------------
out_cube_dir <- here(data_proc_dir, "cubes")
dir.create(out_cube_dir, showWarnings = FALSE, recursive = TRUE)

manifest_path <- here(data_proc_dir, "cube_manifest.csv")
totals_path <- here(data_proc_dir, "cube_totals_by_basisOfRecord.csv")

# Helper functions --------------------------------------------------------

#' Standardize data.table column names
#' @param dt data.table
#' @return data.table with cleaned names
standardize_names_dt <- function(dt) {
  new_names <- names(dt) |>
    str_replace_all("\\s+", "_") |>
    str_replace_all("[^A-Za-z0-9_]", "") |>
    str_to_lower()
  
  setnames(dt, new_names)
  dt
}

#' Get file size in bytes
#' @param path File path
#' @return Numeric size in bytes
file_size_bytes <- function(path) {
  as.numeric(file.info(path)$size)
}

#' Read cube file minimally (for totals computation)
#' @param path Cube file path
#' @return data.table with essential columns only
read_cube_minimal <- function(path) {
  # Read header to get exact column names
  header <- names(fread(path, nrows = 0, encoding = "UTF-8"))
  header_lower <- str_to_lower(header)
  
  # Find essential columns (case-insensitive)
  col_occurrences <- header[which(header_lower == "occurrences")[1]]
  col_yearmonth <- header[which(header_lower == "yearmonth")[1]]
  col_eeacellcode <- header[which(header_lower == "eeacellcode")[1]]
  
  if (is.na(col_occurrences)) {
    cli_abort("Column 'occurrences' not found in: {.path {path}}")
  }
  
  # Select only available essential columns
  cols_to_read <- c(col_occurrences, col_yearmonth, col_eeacellcode)
  cols_to_read <- cols_to_read[!is.na(cols_to_read)]
  
  fread(
    path,
    select = cols_to_read,
    showProgress = FALSE,
    encoding = "UTF-8"
  )
}

#' Read full cube file
#' @param path Cube file path
#' @return data.table with all columns
read_cube_full <- function(path) {
  fread(path, showProgress = TRUE, encoding = "UTF-8")
}

#' Safe sum with NA handling
#' @param x Numeric vector
#' @return Sum (numeric)
safe_sum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

#' Write cube to disk in specified format
#' @param dt data.table to write
#' @param base_path Output path (without extension)
#' @param format Output format ("fst" or "rds")
#' @return Full output path
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

# Process cube files ------------------------------------------------------
cli_h2("Processing GBIF Occurrence Cubes")

manifest_list <- list()
totals_list <- list()

for (grid_name in names(cube_map)) {
  cli_h3("Grid: {grid_name}")
  
  basis_list <- cube_map[[grid_name]]
  
  for (basis_name in names(basis_list)) {
    filename <- basis_list[[basis_name]]
    filepath <- here(cube_dir, filename)
    
    # Validate file exists
    if (!file.exists(filepath)) {
      cli_abort("Cube file not found: {.path {filepath}}")
    }
    
    # Get file size
    size_bytes <- file_size_bytes(filepath)
    size_gb <- size_bytes / 1024^3
    
    cli_alert_info(
      "{.strong {basis_name}}: {.path {filename}} ({round(size_gb, 2)} GB)"
    )
    
    # STEP 1: Always compute totals (minimal read) ---------------------------
    mini <- read_cube_minimal(filepath) |>
      standardize_names_dt()
    
    total_occurrences <- if ("occurrences" %in% names(mini)) {
      safe_sum(mini$occurrences)
    } else {
      NA_real_
    }
    
    # Handle yearmonth (may be character or numeric)
    min_yearmonth <- if ("yearmonth" %in% names(mini)) {
      suppressWarnings(min(mini$yearmonth, na.rm = TRUE))
    } else {
      NA
    }
    
    max_yearmonth <- if ("yearmonth" %in% names(mini)) {
      suppressWarnings(max(mini$yearmonth, na.rm = TRUE))
    } else {
      NA
    }
    
    # Store totals
    totals_list[[length(totals_list) + 1]] <- tibble(
      grid = grid_name,
      basisOfRecord = basis_name,
      source_file = filename,
      file_size_gb = round(size_gb, 3),
      total_occurrences = total_occurrences,
      min_yearmonth = as.character(min_yearmonth),
      max_yearmonth = as.character(max_yearmonth)
    )
    
    # STEP 2: Decide on full ingestion --------------------------------------
    is_large <- size_bytes >= (LARGE_FILE_THRESHOLD_GB * 1024^3)
    do_full_ingest <- !is_large || FULL_INGEST_LARGE_FILES
    
    if (is_large && !FULL_INGEST_LARGE_FILES) {
      cli_alert_warning(
        "Skipping full ingest (large file). Set FULL_INGEST_LARGE_FILES=TRUE to ingest."
      )
    }
    
    # Initialize tracking variables
    output_path <- NA_character_
    n_rows <- nrow(mini)
    n_cols <- ncol(mini)
    
    # STEP 3: Full ingestion (if enabled) -----------------------------------
    if (do_full_ingest) {
      dt <- read_cube_full(filepath) |>
        standardize_names_dt()
      
      # Add provenance columns
      dt[, `:=`(
        grid = grid_name,
        basisofrecord = basis_name,
        source_file = filename
      )]
      
      n_rows <- nrow(dt)
      n_cols <- ncol(dt)
      
      # Write processed cube
      base_path <- here(
        out_cube_dir, 
        glue("cube_{grid_name}_{basis_name}")
      )
      
      output_path <- write_cube(dt, base_path, format = OUTPUT_FORMAT)
      
      cli_alert_success(
        "Written: {.path {basename(output_path)}} ({n_rows} rows, {n_cols} cols)"
      )
      
      # Clean up large object
      rm(dt)
    }
    
    # Store manifest entry
    manifest_list[[length(manifest_list) + 1]] <- tibble(
      grid = grid_name,
      basisOfRecord = basis_name,
      source_file = filename,
      file_size_gb = round(size_gb, 3),
      full_ingest = do_full_ingest,
      output_format = OUTPUT_FORMAT,
      processed_file = output_path,
      rows = n_rows,
      cols = n_cols
    )
    
    # Clean up
    rm(mini)
    invisible(gc())
  }
}

# Combine and save results ------------------------------------------------
cli_h2("Writing Metadata")

manifest_df <- bind_rows(manifest_list)
totals_df <- bind_rows(totals_list)

write_csv(manifest_df, manifest_path)
cli_alert_success("Manifest: {.path {manifest_path}}")

write_csv(totals_df, totals_path)
cli_alert_success("Totals: {.path {totals_path}}")

# Summary statistics ------------------------------------------------------
cli_h2("Ingestion Summary")

summary_stats <- manifest_df |>
  summarise(
    n_files = n(),
    n_fully_ingested = sum(full_ingest),
    total_rows = sum(rows, na.rm = TRUE),
    total_size_gb = sum(file_size_gb)
  )

cli_alert_info("Files processed: {summary_stats$n_files}")
cli_alert_info("Fully ingested: {summary_stats$n_fully_ingested}")
cli_alert_info("Total rows: {scales::comma(summary_stats$total_rows)}")
cli_alert_info("Total size: {round(summary_stats$total_size_gb, 2)} GB")

cli_alert_success("GBIF cube ingestion complete!")
