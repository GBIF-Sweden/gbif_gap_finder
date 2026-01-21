# scripts/03_ingest_gbif_cubes.R
# Read all GBIF cube CSVs defined in config.yml:cube_files, write processed per-file outputs + manifest.

source("scripts/00_setup.R")

# --- Read cube file map from config -------------------------------------------
cube_map <- cfg_get("files.cube_files", NULL)
cube_dir <- cfg_get("paths.cube_dir", here::here("data_raw", "gbif_occurrence_cubes"))

if (is.null(cube_map) || length(cube_map) == 0) {
  stop("No cube_files found in config.yml. Please add cube_files:grid10km/grid50km entries.")
}

out_cube_dir <- here::here("data_proc", "cubes")
dir.create(out_cube_dir, showWarnings = FALSE, recursive = TRUE)

# --- Helpers ------------------------------------------------------------------
read_cube <- function(path) {
  if (!file.exists(path)) stop("Cube file not found: ", path)
  data.table::fread(path, showProgress = FALSE, encoding = "UTF-8")
}

standardize_names_dt <- function(dt) {
  nm <- names(dt)
  nm <- stringr::str_replace_all(nm, "\\s+", "_")
  nm <- stringr::str_replace_all(nm, "[^A-Za-z0-9_]", "")
  nm <- tolower(nm)
  data.table::setnames(dt, nm)
  dt
}

safe_write_rds <- function(obj, path) {
  saveRDS(obj, path, compress = "xz")
}

# --- Iterate ------------------------------------------------------------------
manifest <- list()

for (grid_name in names(cube_map)) {
  grid_list <- cube_map[[grid_name]]
  
  for (cube_type in names(grid_list)) {
    fname <- grid_list[[cube_type]]
    fpath <- file.path(cube_dir, fname)
    
    log_msg("Reading cube: ", grid_name, " / ", cube_type, " -> ", fname)
    dt <- read_cube(fpath) |> standardize_names_dt()
    
    # add metadata columns (as plain columns for downstream joins/filters)
    dt[, grid := grid_name]
    dt[, cube_type := cube_type]
    dt[, source_file := fname]
    
    # write per-file output
    out_file <- file.path(out_cube_dir, paste0("cube_", grid_name, "_", cube_type, ".rds"))
    safe_write_rds(dt, out_file)
    log_msg("Wrote: ", out_file)
    
    # manifest entry
    manifest[[length(manifest) + 1]] <- data.frame(
      grid = grid_name,
      cube_type = cube_type,
      source_file = fname,
      rows = nrow(dt),
      cols = ncol(dt),
      stringsAsFactors = FALSE
    )
  }
}

manifest_df <- do.call(rbind, manifest)
manifest_path <- here::here("data_proc", "cube_manifest.csv")
readr::write_csv(manifest_df, manifest_path)
log_msg("Wrote manifest: ", manifest_path)

log_msg("Done: cubes ingested.")
