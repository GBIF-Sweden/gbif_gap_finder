# scripts/01_ingest_grids.R
# Read EEA grids (10km + 50km) from config.yml, standardize CRS, write to data_proc/.

source("scripts/00_setup.R")

# --- Inputs from config.yml ---------------------------------------------------
grid10_dir  <- cfg_get("paths.grid_10km_dir")
grid50_dir  <- cfg_get("paths.grid_50km_dir")
grid10_file <- cfg_get("files.grids.grid10km")
grid50_file <- cfg_get("files.grids.grid50km")

stopifnot(!is.null(grid10_dir), !is.null(grid50_dir),
          !is.null(grid10_file), !is.null(grid50_file))

f_grid10 <- here::here(grid10_dir, grid10_file)
f_grid50 <- here::here(grid50_dir, grid50_file)

# --- Helpers ------------------------------------------------------------------
read_sf_safe <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  sf::st_read(path, quiet = TRUE)
}

standardize_grid <- function(x, target_crs = CRS_SWEREF99TM) {
  
  if (is.na(sf::st_crs(x))) stop("Input grid has no CRS set.")
  
  # Transform first
  x <- sf::st_transform(x, target_crs)
  
  old_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  
  gt <- unique(sf::st_geometry_type(x))
  log_msg("Geometry types before: ", paste(gt, collapse = ", "))
  
  # --- Handle MULTISURFACE safely --------------------------------------------
  if (any(gt == "MULTISURFACE")) {
    # Step 1: MULTISURFACE -> GEOMETRYCOLLECTION
    x <- sf::st_cast(x, "GEOMETRYCOLLECTION", warn = FALSE)
    
    # Step 2: extract polygon parts only
    x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
    
    # Step 3: ensure consistent polygon type
    x <- sf::st_cast(x, "MULTIPOLYGON", warn = FALSE)
  } else {
    # For normal POLYGON/MULTIPOLYGON grids (like your 10km)
    x <- sf::st_cast(x, "MULTIPOLYGON", warn = FALSE)
  }
  
  # Fix validity (GEOS)
  x <- tryCatch(
    sf::st_make_valid(x),
    error = function(e) sf::st_buffer(x, 0)
  )
  
  sf::sf_use_s2(old_s2)
  
  gt2 <- unique(sf::st_geometry_type(x))
  log_msg("Geometry types after: ", paste(gt2, collapse = ", "))
  
  x
}



# --- Read ---------------------------------------------------------------------
log_msg("Reading 10km grid: ", f_grid10)
g10 <- read_sf_safe(f_grid10) |> standardize_grid()

log_msg("Reading 50km grid: ", f_grid50)
g50 <- read_sf_safe(f_grid50) |> standardize_grid()

# --- Write --------------------------------------------------------------------
log_msg("Writing processed 10km grid -> ", out_grid_10km_gpkg)
sf::st_write(g10, out_grid_10km_gpkg, delete_dsn = TRUE, quiet = TRUE)

log_msg("Writing processed 50km grid -> ", out_grid_50km_gpkg)
sf::st_write(g50, out_grid_50km_gpkg, delete_dsn = TRUE, quiet = TRUE)

log_msg("Done: grids ingested.")

