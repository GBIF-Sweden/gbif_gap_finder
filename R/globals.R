## R/globals.R
# Project-wide configuration, paths, constants, and small utilities.
# Designed to be sourced early (e.g. from scripts/00_setup.R).

options(stringsAsFactors = FALSE)
Sys.setenv(TZ = "Europe/Stockholm")

# --- Paths (match your folder structure) --------------------------------------
p_root      <- here::here()
p_R         <- here::here("R")
p_scripts   <- here::here("scripts")
p_analysis  <- here::here("analysis")

p_data_raw  <- here::here("data_raw")
p_data_proc <- here::here("data_proc")
p_output    <- here::here("output")
p_docs      <- here::here("docs")
p_logs      <- here::here("logs")

# Create folders (idempotent)
dir.create(p_data_raw,  showWarnings = FALSE, recursive = TRUE)
dir.create(p_data_proc, showWarnings = FALSE, recursive = TRUE)
dir.create(p_output,    showWarnings = FALSE, recursive = TRUE)
dir.create(p_docs,      showWarnings = FALSE, recursive = TRUE)
dir.create(p_logs,      showWarnings = FALSE, recursive = TRUE)

# --- Config -------------------------------------------------------------------
config_path <- here::here("config.yml")

read_config <- function(path = config_path) {
  if (!file.exists(path)) {
    warning("config.yml not found at: ", path, "\nUsing defaults (you should create config.yml).")
    return(list())
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read config.yml. Install it and retry.")
  }
  yaml::read_yaml(path)
}

cfg <- read_config()

cfg_get <- function(name, default = NULL) {
  # name can be "a.b.c" for nested lists
  keys <- strsplit(name, "\\.", fixed = FALSE)[[1]]
  x <- cfg
  for (k in keys) {
    if (is.null(x) || is.null(x[[k]])) return(default)
    x <- x[[k]]
  }
  x
}

# --- CRS ----------------------------------------------------------------------
# Recommended analysis CRS for Sweden: SWEREF99 TM (EPSG:3006)
CRS_SWEREF99TM <- 3006

if (requireNamespace("sf", quietly = TRUE)) {
  # Keep explicit for reproducibility across machines
  sf::sf_use_s2(TRUE)
}

# --- Raw data directories (your exact names) ----------------------------------
raw_gbif_cube_dir     <- cfg_get("paths.gbif_cube_dir",     here::here("data_raw", "gbif_occurrence_cubes"))
raw_grid_10km_dir     <- cfg_get("paths.grid_10km_dir",     here::here("data_raw", "eea_grid_10km"))
raw_grid_50km_dir     <- cfg_get("paths.grid_50km_dir",     here::here("data_raw", "eea_grid_50km"))
raw_redlist_se_dir    <- cfg_get("paths.redlist_se_dir",    here::here("data_raw", "red_list_se"))
raw_redlist_iucn_dir  <- cfg_get("paths.redlist_iucn_dir",  here::here("data_raw", "red_list_iucn"))
raw_dyntaxa_dir       <- cfg_get("paths.dyntaxa_dir",       here::here("data_raw", "dyntaxa"))

# Optional: filenames (set in config.yml when known)
redlist_se_file       <- cfg_get("files.redlist_se", NULL)
redlist_iucn_file     <- cfg_get("files.redlist_iucn", NULL)
dyntaxa_file          <- cfg_get("files.dyntaxa", NULL)

# --- Derived outputs (stable, go to data_processed/) --------------------------
out_grid_10km_gpkg        <- here::here("data_proc", "grids_10km.gpkg")
out_grid_50km_gpkg        <- here::here("data_proc", "grids_50km.gpkg")

out_redlist_se_rds        <- here::here("data_proc", "red_list_se_current.rds")
out_redlist_iucn_rds      <- here::here("data_proc", "red_list_iucn_current.rds")

out_dyntaxa_rds           <- here::here("data_proc", "dyntaxa_current.rds")

# --- Logging helpers -----------------------------------------------------------
timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_msg <- function(...) message("[", timestamp(), "] ", paste0(..., collapse = ""))

# --- Light sanity notes (do not stop) -----------------------------------------
if (!dir.exists(raw_gbif_cube_dir))    log_msg("Note: GBIF cube dir not found yet: ", raw_gbif_cube_dir)
if (!dir.exists(raw_grid_10km_dir))    log_msg("Note: 10 km grid dir not found yet: ", raw_grid_10km_dir)
if (!dir.exists(raw_grid_50km_dir))    log_msg("Note: 50 km grid dir not found yet: ", raw_grid_50km_dir)
if (!dir.exists(raw_redlist_se_dir))   log_msg("Note: Red List SE dir not found yet: ", raw_redlist_se_dir)
if (!dir.exists(raw_redlist_iucn_dir)) log_msg("Note: Red List IUCN dir not found yet: ", raw_redlist_iucn_dir)
if (!dir.exists(raw_dyntaxa_dir))      log_msg("Note: Dyntaxa dir not found yet: ", raw_dyntaxa_dir)
