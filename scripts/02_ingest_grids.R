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

source(here::here("scripts", "00_setup.R"))

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
  cli_alert_success(
    "Found {scales::comma(length(country_cells_10km))} unique cells for {COUNTRY_CODE}"
  )
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
    # Assign each cell to the country by CENTROID-in-boundary (not mere
    # intersection), and always keep any cell that actually carries this
    # country's data. The previous logic kept every cell that *intersected* an
    # outward-buffered boundary, which pulled in border cells sitting mostly in
    # neighbouring countries (Norway/Finland) and then surfaced them as
    # "zero-coverage" Swedish gaps (T-D7). Centroid-in-country drops the foreign
    # spill; the `has_data` clause guarantees no real border records are lost.
    admin_path <- here(raw_admin_dir, "admin_level1.gpkg")
    if (file.exists(admin_path)) {
      cli_alert_info("Clipping to admin boundary (GADM level 1, centroid-in-country)")
      admin <- st_read(admin_path, quiet = TRUE) |> st_transform(target_crs)
      country_boundary <- st_union(admin)
      in_country <- st_intersects(st_centroid(st_geometry(grid)),
                                  country_boundary, sparse = FALSE)[, 1]
    } else {
      # Fallback: convex hull of data cells, centroid-in-hull (no outward buffer)
      cli_alert_info(
        "No admin boundary found — using convex hull of data cells (centroid-in-hull)"
      )
      cells_with_data <- grid |> filter(eeacellcode %in% country_cells_10km)
      country_hull <- st_convex_hull(st_union(cells_with_data))
      in_country <- st_intersects(st_centroid(st_geometry(grid)),
                                  country_hull, sparse = FALSE)[, 1]
    }
    has_data <- grid$eeacellcode %in% country_cells_10km
    n_before <- nrow(grid)
    grid <- grid[in_country | has_data, ]
    n_with_data <- sum(grid$eeacellcode %in% country_cells_10km)
    n_empty <- nrow(grid) - n_with_data
    cli_alert_success(
      "Clipped: {scales::comma(n_before)} → {scales::comma(nrow(grid))} cells \\
       ({n_with_data} with data, {n_empty} empty, centroid-in-country)"
    )
  } else if (grid_name == "grid50km") {
    # Clip the 50km grid the SAME way as 10km: assign each cell to the country by
    # CENTROID-in-admin-boundary, and always keep any cell that carries data. Do
    # NOT derive the 50km universe from data-bearing cells (the old
    # `eeacellcode %in% country_cells_50km` filter) — that collapses the coverage
    # denominator onto the data and reports ~100% coverage with zero empty 50km
    # cells. Empty in-country cells must survive so the downstream zero-fill can
    # flag them as gaps. (Fix 2026-07-21.)
    admin_path <- here(raw_admin_dir, "admin_level1.gpkg")
    if (file.exists(admin_path)) {
      cli_alert_info("Clipping 50km to admin boundary (GADM level 1, centroid-in-country)")
      admin <- st_read(admin_path, quiet = TRUE) |> st_transform(target_crs)
      country_boundary <- st_union(admin)
      in_country <- st_intersects(st_centroid(st_geometry(grid)),
                                  country_boundary, sparse = FALSE)[, 1]
    } else if (!is.null(country_cells_10km) && "grid10km" %in% names(grids_processed)) {
      cli_alert_info("No admin boundary — clipping 50km by the (centroid-clipped) 10km hull")
      hull_10km <- st_convex_hull(st_union(grids_processed$grid10km))
      in_country <- st_intersects(st_centroid(st_geometry(grid)),
                                  hull_10km, sparse = FALSE)[, 1]
    } else {
      cli_alert_warning(
        "No admin boundary or 10km grid — keeping full 50km grid (coverage may be biased)"
      )
      in_country <- rep(TRUE, nrow(grid))
    }
    has_data <- if (!is.null(country_cells_50km)) {
      grid$eeacellcode %in% country_cells_50km
    } else rep(FALSE, nrow(grid))

    # T-I2: parse_10km() synthesises 50km parent codes by string math, so a
    # derived code can fail to match any real cell in the EEA 50km grid (regex
    # miss, rounding/edge artefact, or code-format drift). Such codes drop out of
    # the `%in%` above silently and under-count data-bearing 50km cells. Warn
    # (against the full, pre-clip grid) so the mismatch is visible.
    if (!is.null(country_cells_50km)) {
      missing_50km <- setdiff(country_cells_50km, grid$eeacellcode)
      if (length(missing_50km) > 0) {
        cli_alert_warning(
          "{length(missing_50km)} of {length(country_cells_50km)} derived 50km parent codes \\
           are absent from the EEA 50km grid \\
           (e.g. {paste(head(missing_50km, 3), collapse = ', ')}); those data-bearing cells \\
           are under-counted in coverage."
        )
      }
    }

    n_before <- nrow(grid)
    grid <- grid[in_country | has_data, ]
    n_with_data <- sum(grid$eeacellcode %in% country_cells_50km)
    n_empty <- nrow(grid) - n_with_data
    cli_alert_success(
      "Clipped: {scales::comma(n_before)} → {scales::comma(nrow(grid))} cells \\
       ({n_with_data} with data, {n_empty} empty, centroid-in-country)"
    )
  }

  cli_alert_info("Writing: {.path {basename(gc$output)}}")
  st_write(grid, gc$output, delete_dsn = TRUE, quiet = TRUE)
  cli_alert_success("{nrow(grid)} cells")

  # T-A1: cache the clipped cell-code list as a sidecar .txt so 07 (and future
  # consumers) can read codes without re-opening the gpkg geometry.
  # grid10km -> cellcodes_10km.txt, grid50km -> cellcodes_50km.txt.
  codes_path <- file.path(p_data_proc, paste0("cellcodes_", sub("^grid", "", grid_name), ".txt"))
  writeLines(unique(as.character(grid$eeacellcode)), codes_path)
  cli_alert_success("Cached {basename(codes_path)}")

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
