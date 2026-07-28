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

# T-D5 marine cells: master switch + zone. When FALSE (default) script 02 builds
# a land-only grid exactly as before; when TRUE, empty EEZ sea cells are added so
# marine coverage + zero-coverage gaps are measured (see configs/config_{CC}.yml).
marine_enabled <- isTRUE(cfg_get("marine.enabled", FALSE))
marine_zone    <- cfg_get("marine.zone", "eez")

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
# Marine Helpers (T-D5)
# ============================================================================

#' Fetch (or load from cache) a national marine zone polygon in the target CRS.
#'
#' Order of resolution: (1) an explicit local file (marine.eez_file); (2) a
#' cached GeoPackage under data/{CC}/raw/marine/; (3) download via mregions2
#' (Marine Regions EEZ, or the 12 nm territorial sea) filtered to this country,
#' then cache it. The result is validated and transformed to `crs`.
load_marine_zone <- function(zone = "eez", crs = target_crs) {
  marine_dir <- here(p_data_raw, "marine")
  if (!dir.exists(marine_dir)) dir.create(marine_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(marine_dir, glue("marine_{zone}.gpkg"))

  read_zone <- function(p) {
    z <- st_read(p, quiet = TRUE)
    if (is.na(st_crs(z))) cli_abort("Marine zone has no CRS: {.path {p}}")
    st_make_valid(st_transform(z, crs))
  }

  eez_file <- cfg_get("marine.eez_file", NULL)
  if (!is.null(eez_file) && nzchar(eez_file)) {
    ep <- if (file.exists(eez_file)) eez_file else here(eez_file)
    if (!file.exists(ep)) cli_abort("marine.eez_file not found: {.path {ep}}")
    cli_alert_info("Marine zone from file: {.path {basename(ep)}}")
    return(read_zone(ep))
  }

  if (file.exists(cache_file) && !isTRUE(cfg_get("marine.force_download", FALSE))) {
    cli_alert_info("Marine zone from cache: {.path {basename(cache_file)}}")
    return(read_zone(cache_file))
  }

  if (!requireNamespace("mregions2", quietly = TRUE)) {
    cli_abort(c(
      "marine.enabled = true needs the 'mregions2' package.",
      "i" = "Install it, then re-run: install.packages('mregions2'); renv::snapshot()",
      "i" = "Or point marine.eez_file at a local EEZ GeoPackage/shapefile."
    ))
  }

  layer <- if (identical(zone, "territorial")) "eez_12nm" else "eez"
  filt  <- cfg_get("marine.cql_filter", NULL)
  if (is.null(filt) || !nzchar(filt)) {
    if (identical(zone, "eez")) {
      mrgid <- cfg_get("marine.mrgid", NULL)
      if (is.null(mrgid)) cli_abort("marine.zone = 'eez' needs marine.mrgid (Marine Regions EEZ id).")
      filt <- glue("mrgid = {as.integer(mrgid)}")
    } else {
      cname <- cfg_get("country.name", COUNTRY_CODE)
      filt  <- glue("territory1 = '{cname}'")
    }
  }

  cli_alert_info("Fetching marine zone from Marine Regions (layer {layer}; {filt})")
  z <- tryCatch(
    mregions2::mrp_get(layer, cql_filter = as.character(filt)),
    error = function(e) cli_abort(c(
      "Marine Regions fetch failed: {e$message}",
      "i" = "Check the network, or set marine.eez_file to a local file."
    ))
  )
  if (is.null(z) || nrow(z) == 0) {
    cli_abort(c(
      "Marine Regions returned 0 features for: {filt}",
      "i" = "Verify marine.mrgid / marine.cql_filter for {COUNTRY_CODE}."
    ))
  }
  z <- st_make_valid(st_transform(z, crs))
  tryCatch(
    st_write(z, cache_file, delete_dsn = TRUE, quiet = TRUE),
    error = function(e) cli_alert_warning("Could not cache marine zone: {e$message}")
  )
  cli_alert_success("Marine zone fetched + cached: {.path {basename(cache_file)}} ({nrow(z)} feature(s))")
  z
}

#' Add empty marine (EEZ) grid cells to a land grid, in EEA cube coding.
#'
#' Builds a LAEA fishnet at `cellsize_m` aligned to the EEA reference-grid
#' lattice, keeps cells whose centroid falls inside `zone`, codes them
#' {prefix}E{round(x/10000)}N{round(y/10000)} to match the GBIF cube, drops any
#' code already present in the land grid, and row-binds the remaining empty sea
#' cells with a logical `marine` flag (land = FALSE, sea = TRUE). Aborts if a
#' fishnet cell shared with the land grid is not co-located (lattice mismatch).
add_marine_cells <- function(grid, zone, cellsize_m, prefix, crs = target_crs) {
  old_s2 <- sf_use_s2(); sf_use_s2(FALSE); on.exit(sf_use_s2(old_s2))

  bb <- st_bbox(zone)
  x0 <- floor(bb[["xmin"]] / cellsize_m) * cellsize_m
  y0 <- floor(bb[["ymin"]] / cellsize_m) * cellsize_m
  x1 <- ceiling(bb[["xmax"]] / cellsize_m) * cellsize_m
  y1 <- ceiling(bb[["ymax"]] / cellsize_m) * cellsize_m
  xs <- seq(x0, x1 - cellsize_m, by = cellsize_m)
  ys <- seq(y0, y1 - cellsize_m, by = cellsize_m)
  corners <- expand.grid(x = xs, y = ys)

  # Keep cells whose CENTROID is inside the zone (matches the clip rule and
  # avoids building polygons for the whole bounding box).
  cxy <- data.frame(cx = corners$x + cellsize_m / 2, cy = corners$y + cellsize_m / 2)
  pts <- st_as_sf(cxy, coords = c("cx", "cy"), crs = crs)
  inside <- lengths(st_intersects(pts, zone)) > 0
  if (!any(inside)) {
    cli_alert_warning("Marine {prefix}: no cells with centroid inside the zone")
    grid$marine <- FALSE
    return(grid)
  }

  llx <- corners$x[inside]
  lly <- corners$y[inside]
  # round(), not floor(): lattice corners are exact multiples of the cell size,
  # but stored geometry can carry sub-metre float noise that would flip floor().
  codes <- sprintf("%sE%dN%d", prefix,
                   as.integer(round(llx / 10000)), as.integer(round(lly / 10000)))
  polys <- st_sfc(mapply(function(x, y) st_polygon(list(matrix(
      c(x, y, x + cellsize_m, y, x + cellsize_m, y + cellsize_m, x, y + cellsize_m, x, y),
      ncol = 2, byrow = TRUE))),
      llx, lly, SIMPLIFY = FALSE), crs = crs)

  # Lattice-alignment self-check against the authoritative EEA grid: any code
  # shared with the land grid must be geometrically co-located.
  land_codes <- as.character(grid$eeacellcode)
  shared <- intersect(codes, land_codes)
  if (length(shared) > 0) {
    s   <- head(shared, 500L)
    fc  <- st_centroid(polys[match(s, codes)])
    gc0 <- st_centroid(st_geometry(grid)[match(s, land_codes)])
    dmax <- suppressWarnings(max(as.numeric(st_distance(fc, gc0, by_element = TRUE))))
    if (!is.finite(dmax) || dmax > 1) {
      cli_abort(c(
        "Marine fishnet misaligned with the EEA {prefix} grid.",
        "x" = "Max shared-cell centroid offset {round(dmax)} m (expected 0).",
        "i" = "Check the LAEA lattice offset / CRS ({crs})."
      ))
    }
    cli_alert_success("Marine {prefix}: lattice aligned ({length(shared)} shared cell(s))")
  }

  new_idx <- !(codes %in% land_codes)
  n_new <- sum(new_idx)
  grid$marine <- FALSE
  if (n_new > 0) {
    land_df <- st_drop_geometry(grid)
    new_df  <- data.frame(eeacellcode = codes[new_idx], stringsAsFactors = FALSE)
    for (col in setdiff(names(land_df), names(new_df))) new_df[[col]] <- NA
    new_df$marine <- TRUE
    new_df <- new_df[, names(land_df), drop = FALSE]
    grid <- st_sf(rbind(land_df, new_df),
                  geometry = c(st_geometry(grid), polys[new_idx]), crs = crs)
  }
  cli_alert_success("Marine {prefix}: added {scales::comma(n_new)} empty sea cell(s)")
  grid
}

# ============================================================================
# Process Grids
# ============================================================================

cli_h2("Processing EEA Grids for {COUNTRY_CODE}")

grids_processed <- list()

# T-D5: fetch (or load cached) the marine zone once, before the per-grid loop.
eez_zone <- NULL
if (marine_enabled) {
  cli_h2("Loading marine zone for {COUNTRY_CODE}")
  eez_zone <- load_marine_zone(zone = marine_zone, crs = target_crs)
  eez_zone <- sf::st_union(sf::st_geometry(eez_zone))
  cli_alert_success("Marine zone loaded ({marine_zone})")
}

for (grid_name in names(grid_configs)) {
  gc <- grid_configs[[grid_name]]
  cli_h3("{grid_name}")

  grid <- read_grid_safe(gc$file) |> standardize_grid()

  # Standardise cell code column name
  grid <- standardise_cellcode(grid)

  # T-D5: bring the country's empty marine (EEZ) cells into the grid universe
  # before the clip (config-gated; no-op unless marine.enabled).
  if (marine_enabled && !is.null(eez_zone)) {
    marine_cellsize <- if (grid_name == "grid10km") 10000L else 50000L
    grid <- add_marine_cells(grid, eez_zone, marine_cellsize,
                             sub("^grid", "", grid_name), target_crs)
  }

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
    # T-D5: also keep cells whose centroid is in the marine zone (EEZ), so the
    # empty sea cells added above survive the clip. No-op unless marine.enabled.
    if (marine_enabled && !is.null(eez_zone)) {
      in_eez <- st_intersects(st_centroid(st_geometry(grid)), eez_zone,
                              sparse = FALSE)[, 1]
      in_country <- in_country | in_eez
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
    # T-D5: also keep cells whose centroid is in the marine zone (EEZ), so the
    # empty sea cells added above survive the clip. No-op unless marine.enabled.
    if (marine_enabled && !is.null(eez_zone)) {
      in_eez <- st_intersects(st_centroid(st_geometry(grid)), eez_zone,
                              sparse = FALSE)[, 1]
      in_country <- in_country | in_eez
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
