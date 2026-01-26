# scripts/01_ingest_grids.R
# ==============================================================================
# EEA Grid Ingestion & Standardization
# ==============================================================================
# This script:
# - Reads EEA grid shapefiles (10km and 50km resolution)
# - Standardizes CRS to SWEREF99 TM
# - Handles complex geometry types (MULTISURFACE -> MULTIPOLYGON)
# - Validates and repairs geometries
# - Writes processed grids to data_proc/

library(here)
library(sf)
library(dplyr)
library(purrr)
library(cli)
library(glue)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------
grid_configs <- list(
  grid10km = list(
    dir = cfg_get("paths.grid_10km_dir"),
    file = cfg_get("files.grids.grid10km"),
    output = out_grid_10km_gpkg
  ),
  grid50km = list(
    dir = cfg_get("paths.grid_50km_dir"),
    file = cfg_get("files.grids.grid50km"),
    output = out_grid_50km_gpkg
  )
)

# Validate configuration --------------------------------------------------
purrr::walk(grid_configs, ~{
  if (is.null(.x$dir) || is.null(.x$file)) {
    cli_abort("Missing grid configuration in config.yml")
  }
})

# Helper functions --------------------------------------------------------

#' Read spatial file with error handling
#' @param path Full path to spatial file
#' @return sf object
read_grid_safe <- function(path) {
  if (!file.exists(path)) {
    cli_abort("Grid file not found: {.path {path}}")
  }
  
  cli_alert_info("Reading: {.path {path}}")
  
  tryCatch(
    st_read(path, quiet = TRUE),
    error = function(e) {
      cli_abort("Failed to read grid file: {e$message}")
    }
  )
}

#' Standardize grid CRS and geometry type
#' @param grid sf object with grid geometries
#' @param target_crs Target CRS (default: SWEREF99 TM)
#' @return Standardized sf object
standardize_grid <- function(grid, target_crs = CRS_SWEREF99TM) {
  
  # Validate input CRS
  if (is.na(st_crs(grid))) {
    cli_abort("Input grid has no CRS defined")
  }
  
  cli_alert_info("Original CRS: {st_crs(grid)$input}")
  
  # Transform to target CRS
  grid <- st_transform(grid, target_crs)
  cli_alert_success("Transformed to: {st_crs(grid)$input}")
  
  # Temporarily disable s2 for planar operations
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2))
  
  # Check geometry types
  geom_types <- unique(st_geometry_type(grid))
  cli_alert_info("Geometry types: {paste(geom_types, collapse = ', ')}")
  
  # Handle MULTISURFACE (common in EEA 50km grid)
  if (any(geom_types == "MULTISURFACE")) {
    cli_alert_info("Converting MULTISURFACE geometries...")
    
    grid <- grid |>
      st_cast("GEOMETRYCOLLECTION", warn = FALSE) |>
      st_collection_extract("POLYGON", warn = FALSE) |>
      st_cast("MULTIPOLYGON", warn = FALSE)
  } else {
    # Standardize to MULTIPOLYGON
    grid <- st_cast(grid, "MULTIPOLYGON", warn = FALSE)
  }
  
  # Validate and repair geometries
  invalid_count <- sum(!st_is_valid(grid))
  
  if (invalid_count > 0) {
    cli_alert_warning("Found {invalid_count} invalid geometries - repairing...")
    
    grid <- tryCatch(
      st_make_valid(grid),
      error = function(e) {
        cli_alert_warning("st_make_valid failed, using buffer(0) method")
        st_buffer(grid, 0)
      }
    )
  }
  
  # Verify final geometry types
  final_types <- unique(st_geometry_type(grid))
  cli_alert_success("Final geometry types: {paste(final_types, collapse = ', ')}")
  
  grid
}

# Process grids -----------------------------------------------------------
cli_h2("Processing EEA Grids")

grids_processed <- purrr::map(grid_configs, ~{
  # Construct file path
  grid_path <- here(.x$dir, .x$file)
  
  # Read and standardize
  grid <- read_grid_safe(grid_path) |>
    standardize_grid()
  
  # Write output
  cli_alert_info("Writing to: {.path {(.x$output)}}")
  st_write(
    grid, 
    .x$output, 
    delete_dsn = TRUE, 
    quiet = TRUE
  )
  
  cli_alert_success(
    "Processed {nrow(grid)} grid cells ({ncol(grid)} attributes)"
  )
  
  grid
})

# Summary -----------------------------------------------------------------
cli_h2("Ingestion Summary")

summary_table <- tibble::tibble(
  grid = names(grid_configs),
  n_cells = purrr::map_int(grids_processed, nrow),
  n_attributes = purrr::map_int(grids_processed, ncol),
  output_file = purrr::map_chr(grid_configs, "output")
)

print(summary_table)

cli_alert_success("Grid ingestion complete!")
