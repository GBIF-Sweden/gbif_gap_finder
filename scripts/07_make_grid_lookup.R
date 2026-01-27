# scripts/07_make_grid_lookup.R
# ==============================================================================
# Create Grid Lookup Tables
# ==============================================================================
# This script:
# - Reads processed grid files (10km and 50km)
# - Identifies the EEA cell code field in each grid
# - Creates lookup tables mapping polygon IDs to cell codes
# - Validates uniqueness and completeness
# - Writes lookup tables to data_proc/derived/
#
# These lookups enable joining grid geometries with cube summaries

library(here)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------
p_derived <- here(p_data_proc, "derived")

# Create output directory
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)

# Grid file paths
grid_files <- list(
  grid10km = list(
    path = here(p_data_proc, "grids_10km.gpkg"),
    output = "grid_lookup_10km.csv",
    label = "10km"
  ),
  grid50km = list(
    path = here(p_data_proc, "grids_50km.gpkg"),
    output = "grid_lookup_50km.csv",
    label = "50km"
  )
)

# Validate inputs ---------------------------------------------------------
cli_h2("Validating Grid Files")

purrr::walk(grid_files, ~{
  if (!file.exists(.x$path)) {
    cli_abort("Grid file not found: {.path {(.x$path)}}")
  }
  cli_alert_success("{(.x$label)} grid file present")
})


#' Create lookup table for a grid
#' @param grid sf object with grid geometries
#' @param grid_label Human-readable label (e.g., "10km")
#' @param output_file Output filename
#' @return List with code_field and output path
create_lookup <- function(grid, grid_label, output_file) {
  
  cli_h3("Processing {grid_label} Grid")
  
  # Identify cell code field
  field_names <- names(grid)
  code_field <- guess_cellcode_field(field_names)
  
  if (is.na(code_field)) {
    cli_abort(c(
      "Could not identify cell code field for {grid_label} grid",
      "i" = "Available columns:",
      paste0("  • ", field_names)
    ))
  }
  
  cli_alert_info("Cell code field: {.field {code_field}}")
  
  # Extract and validate cell codes
  cell_codes <- grid[[code_field]]
  
  # Check for NA values
  n_na <- sum(is.na(cell_codes))
  if (n_na > 0) {
    cli_abort(c(
      "Cell code field '{code_field}' contains {n_na} NA value{?s}",
      "x" = "All cells must have valid codes"
    ))
  }
  
  # Check uniqueness
  n_total <- nrow(grid)
  n_unique <- length(unique(cell_codes))
  
  if (n_unique != n_total) {
    cli_abort(c(
      "Cell codes are not unique in {grid_label} grid",
      "x" = "Found {n_unique} unique codes for {n_total} rows",
      "i" = "Field: {code_field}",
      "i" = "Consider using a different field or checking data quality"
    ))
  }
  
  cli_alert_success("Validation passed: {scales::comma(n_unique)} unique cells")
  
  # Create stable polygon IDs
  # Format: gridlabel_000001, gridlabel_000002, etc.
  poly_ids <- glue("{grid_label}_{str_pad(seq_len(n_total), width = 6, pad = '0')}")
  
  # Create lookup table
  lookup <- tibble(
    poly_id = poly_ids,
    eeacellcode = as.character(cell_codes)
  )
  
  # Write output
  output_path <- here(p_derived, output_file)
  write_csv(lookup, output_path)
  
  cli_alert_success("Written: {.path {output_file}} ({scales::comma(nrow(lookup))} rows)")
  
  invisible(list(
    code_field = code_field,
    output_path = output_path,
    n_cells = nrow(lookup)
  ))
}

# Process grids -----------------------------------------------------------
cli_h2("Creating Lookup Tables")

lookup_results <- purrr::map(grid_files, ~{
  # Read grid
  grid <- st_read(.x$path, quiet = TRUE)
  
  # Create lookup
  create_lookup(
    grid = grid,
    grid_label = .x$label,
    output_file = .x$output
  )
})

# Summary -----------------------------------------------------------------
cli_h2("Summary")

summary_table <- tibble(
  grid = names(grid_files),
  cell_field = purrr::map_chr(lookup_results, "code_field"),
  n_cells = purrr::map_int(lookup_results, "n_cells"),
  output_file = purrr::map_chr(grid_files, "output")
)

print(summary_table)

cli_alert_success("Grid lookup tables created!")
cli_alert_info("Output location: {.path {p_derived}}")
cli_alert_info(
  "Use these lookups to join grid geometries with cube summaries on {.field eeacellcode}"
)
