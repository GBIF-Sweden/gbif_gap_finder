# scripts/02_ingest_grids.R
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

# Configuration (from config.yml) ------------------------------------------

# Directories
dir_grid_10km <- here(cfg_get("paths.grid_10km_dir", "data_raw/eea_grid_10km"))
dir_grid_50km <- here(cfg_get("paths.grid_50km_dir", "data_raw/eea_grid_50km"))
dir_data_proc <- here(cfg_get("paths.data_proc", "data_proc"))

# File names (just the filename, not full path)
file_grid_10km <- cfg_get("files.grids.grid10km", "se_10km.shp")
file_grid_50km <- cfg_get("files.grids.grid50km", "EEA_50km_grid_v2024.gpkg")

# Build grid configuration list
grid_configs <- list(
  grid10km = list(
    dir    = dir_grid_10km,
    file   = file_grid_10km,
    output = file.path(dir_data_proc, "grids_10km.gpkg")
  ),
  grid50km = list(
    dir    = dir_grid_50km,
    file   = file_grid_50km,
    output = file.path(dir_data_proc, "grids_50km.gpkg")
  )
)

# CRS for Europe (EPSG code, not a path!)
target_crs <- cfg_get("parameters.crs", CRS_ETRS89_LAEA)

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
#' @param crs Target CRS (default: SWEREF99 TM)
#' @return Standardized sf object
standardize_grid <- function(grid, crs = target_crs) {
  
  # Validate input CRS
  if (is.na(st_crs(grid))) {
    cli_abort("Input grid has no CRS defined")
  }
  
  cli_alert_info("Original CRS: {st_crs(grid)$input}")
  
  # Transform to target CRS (ETRS89-LAEA / EPSG:3035 for EEA grids)
  if (st_crs(grid)$epsg == crs) {
    cli_alert_success("Already in target CRS: EPSG:{crs}")
  } else {
    grid <- st_transform(grid, crs)
    st_crs(grid) <- crs  # Force official EPSG definition
    cli_alert_success("Transformed to: EPSG:{crs}")
  }
  
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
  # Construct file path (dir is already full path, file is just filename)
  grid_path <- file.path(.x$dir, .x$file)
  
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
