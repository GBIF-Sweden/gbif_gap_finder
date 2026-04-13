# scripts/02_ingest_grids.R
# ============================================================================
# EEA Grid Ingestion & Standardisation
# ============================================================================
# Purpose:
#   Read EEA grid files (10km and 50km) from data/shared/grids/,
#   standardise CRS and geometry types, clip to country extent using
#   the GBIF cube cell codes, and write processed GeoPackages.
#
# Inputs:
#   - data/shared/grids/Grid_ETRS89-LAEA_10K.shp (Europe-wide 10km)
#   - data/shared/grids/EEA_50km_grid_v2024.gpkg  (Europe-wide 50km)
#   - data/{CC}/raw/cubes/cube_10km.csv (to determine country cells)
#
# Outputs:
#   - data/{CC}/proc/grids_10km.gpkg (country-clipped)
#   - data/{CC}/proc/grids_50km.gpkg (country-clipped)
#
# Dependencies: scripts/00_setup.R, sf, dplyr, data.table
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(cli)
library(data.table)

source(here("scripts", "00_setup.R"))

# ============================================================================
# Configuration
# ============================================================================

file_grid_10km <- cfg_get("files.grids.grid10km", "Grid_ETRS89-LAEA_10K.shp")
file_grid_50km <- cfg_get("files.grids.grid50km", "EEA_50km_grid_v2024.gpkg")

grid_configs <- list(
  grid10km = list(
    file   = file.path(raw_grid_dir, file_grid_10km),
    output = file.path(p_data_proc, "grids_10km.gpkg")
  ),
  grid50km = list(
    file   = file.path(raw_grid_dir, file_grid_50km),
    output = file.path(p_data_proc, "grids_50km.gpkg")
  )
)

target_crs <- cfg_get("parameters.crs", CRS_ETRS89_LAEA)

# ============================================================================
# Determine country cell codes from cube data
# ============================================================================
# The Europe-wide grid has ~100k+ cells. We clip to only the cells
# that appear in this country's GBIF cube data.

cli_h2("Determining Country Cell Codes")

cube_10km_file <- cfg_get("files.cubes.grid10km", "cube_10km.csv")
cube_10km_path <- here(raw_gbif_cube_dir, cube_10km_file)

# Also check for parquet
cube_10km_parquet <- here(raw_gbif_cube_dir, sub("\\.(csv|tsv)$", ".parquet", cube_10km_file))

country_cells_10km <- NULL
if (file.exists(cube_10km_parquet) && requireNamespace("arrow", quietly = TRUE)) {
  cli_alert_info("Reading cell codes from parquet: {basename(cube_10km_parquet)}")
  country_cells_10km <- arrow::open_dataset(cube_10km_parquet) |>
    dplyr::distinct(eeacellcode) |>
    dplyr::collect() |>
    dplyr::pull(eeacellcode)
} else if (file.exists(cube_10km_path)) {
  cli_alert_info("Reading cell codes from CSV: {basename(cube_10km_path)}")
  country_cells_10km <- unique(fread(cube_10km_path, select = "eeacellcode")$eeacellcode)
}

if (!is.null(country_cells_10km)) {
  country_cells_10km <- country_cells_10km[country_cells_10km != "" & !is.na(country_cells_10km)]
  cli_alert_success("Found {scales::comma(length(country_cells_10km))} unique cells for {COUNTRY_CODE}")
} else {
  cli_alert_warning("No cube data found — will keep all grid cells (no clipping)")
}

# Derive 50km cell codes from 10km codes (first 9 characters: "50kmE123N456" pattern)
# EEA 10km code format: 10kmE1234N5678 → 50km parent: extract E/N at 50km resolution
country_cells_50km <- NULL
if (!is.null(country_cells_10km)) {
  # Parse 10km cell codes to get 50km parents
  # 10km format: "10kmExxxxNyyyy" → extract Exxxx and Nyyyy, round down
  parse_10km <- function(codes) {
    e_vals <- as.integer(sub(".*E(\\d+)N.*", "\\1", codes))
    n_vals <- as.integer(sub(".*N(\\d+)$", "\\1", codes))
    # 50km cells: round down to nearest 50
    e50 <- (e_vals %/% 5) * 5
    n50 <- (n_vals %/% 5) * 5
    unique(paste0("50kmE", e50, "N", n50))
  }
  country_cells_50km <- parse_10km(country_cells_10km)
  cli_alert_info("Derived {length(country_cells_50km)} unique 50km parent cells")
}

# ============================================================================
# Helper Functions
# ============================================================================

read_grid_safe <- function(path) {
  if (!file.exists(path)) cli_abort("Grid file not found: {.path {path}}")
  cli_alert_info("Reading: {.path {basename(path)}}")
  tryCatch(st_read(path, quiet = TRUE),
    error = function(e) cli_abort("Failed to read: {e$message}"))
}

standardize_grid <- function(grid, crs = target_crs) {
  if (is.na(st_crs(grid))) cli_abort("Grid has no CRS")

  # Transform CRS
  if (is.na(st_crs(grid)$epsg) || st_crs(grid)$epsg != crs) {
    grid <- st_transform(grid, crs)
    st_crs(grid) <- crs
  }
  cli_alert_info("CRS: EPSG:{crs}")

  # Disable s2 for planar operations
  old_s2 <- sf_use_s2(); sf_use_s2(FALSE); on.exit(sf_use_s2(old_s2))

  # Fix geometry types
  geom_types <- unique(st_geometry_type(grid))
  if (any(geom_types == "MULTISURFACE")) {
    grid <- grid |>
      st_cast("GEOMETRYCOLLECTION", warn = FALSE) |>
      st_collection_extract("POLYGON", warn = FALSE) |>
      st_cast("MULTIPOLYGON", warn = FALSE)
  } else {
    grid <- st_cast(grid, "MULTIPOLYGON", warn = FALSE)
  }

  # Validate geometries
  invalid_count <- sum(!st_is_valid(grid))
  if (invalid_count > 0) {
    cli_alert_warning("Repairing {invalid_count} invalid geometries")
    grid <- tryCatch(st_make_valid(grid), error = function(e) st_buffer(grid, 0))
  }

  grid
}

# ============================================================================
# Process Grids
# ============================================================================

cli_h2("Processing EEA Grids for {COUNTRY_CODE}")

grids_processed <- list()

for (grid_name in names(grid_configs)) {
  gc <- grid_configs[[grid_name]]
  cli_h3("{grid_name}")

  grid <- read_grid_safe(gc$file) |> standardize_grid()

  # Standardise cell code column name
  grid <- standardise_cellcode(grid)

  # Clip to country extent
  if (grid_name == "grid10km" && !is.null(country_cells_10km)) {
    # Use admin boundary (GADM) if available, otherwise convex hull of data cells
    admin_path <- here(raw_admin_dir, "admin_level1.gpkg")
    if (file.exists(admin_path)) {
      cli_alert_info("Clipping to admin boundary (GADM level 1)")
      admin <- st_read(admin_path, quiet = TRUE) |> st_transform(target_crs)
      country_boundary <- st_union(admin) |> st_buffer(1000)  # 1km buffer for edge cells
      n_before <- nrow(grid)
      grid <- grid[st_intersects(grid, country_boundary, sparse = FALSE)[, 1], ]
    } else {
      # Fallback: convex hull of data cells with minimal buffer
      cli_alert_info("No admin boundary found — using convex hull of data cells")
      cells_with_data <- grid |> filter(eeacellcode %in% country_cells_10km)
      country_hull <- st_convex_hull(st_union(cells_with_data)) |> st_buffer(5000)
      n_before <- nrow(grid)
      grid <- grid[st_intersects(grid, country_hull, sparse = FALSE)[, 1], ]
    }
    n_with_data <- sum(grid$eeacellcode %in% country_cells_10km)
    n_empty <- nrow(grid) - n_with_data
    cli_alert_success("Clipped: {scales::comma(n_before)} → {scales::comma(nrow(grid))} cells ({n_with_data} with data, {n_empty} empty)")
  } else if (grid_name == "grid50km" && !is.null(country_cells_50km)) {
    n_before <- nrow(grid)
    grid <- grid |> filter(eeacellcode %in% country_cells_50km)
    cli_alert_success("Clipped: {scales::comma(n_before)} → {scales::comma(nrow(grid))} cells")
  } else if (grid_name == "grid50km" && !is.null(country_cells_10km)) {
    # Fallback: clip 50km grid using 10km grid extent
    cli_alert_info("Clipping 50km grid by 10km extent")
    if ("grid10km" %in% names(grids_processed)) {
      bbox_10km <- st_bbox(grids_processed$grid10km) |> st_as_sfc()
      n_before <- nrow(grid)
      grid <- grid[st_intersects(grid, bbox_10km, sparse = FALSE)[, 1], ]
      cli_alert_success("Clipped: {scales::comma(n_before)} → {scales::comma(nrow(grid))} cells")
    }
  }

  cli_alert_info("Writing: {.path {basename(gc$output)}}")
  st_write(grid, gc$output, delete_dsn = TRUE, quiet = TRUE)
  cli_alert_success("{nrow(grid)} cells")

  grids_processed[[grid_name]] <- grid
}

# ============================================================================
# Summary
# ============================================================================

cli_h2("Grid Summary")
for (nm in names(grids_processed)) {
  g <- grids_processed[[nm]]
  cli_alert_success("{nm}: {scales::comma(nrow(g))} cells, {ncol(g)} attributes")
}

cli_alert_success("Grid ingestion complete!")
cli_alert_info("Next: source('scripts/03_ingest_taxonomy.R')")
