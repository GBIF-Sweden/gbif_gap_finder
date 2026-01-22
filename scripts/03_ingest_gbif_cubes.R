# scripts/03_ingest_gbif_cubes.R
# Robust ingestion of GBIF Occurrence Cube CSVs defined in config.yml under: files.cube_files
#
# YAML keys used:
#   - paths.gbif_cube_dir
#   - paths.data_proc
#   - files.cube_files.grid10km / grid50km
#
# What it does:
#   - ALWAYS computes totals via minimal read (fast, low memory) for plotting
#   - Optionally performs full ingest per file (saves full table per cube) unless it's "large"
#   - Writes:
#       data_proc/cubes/cube_<grid>_<basisOfRecord>.fst  (or .rds)
#       data_proc/cube_manifest.csv
#       data_proc/cube_totals_by_basisOfRecord.csv

source("scripts/00_setup.R")

# ---- Output format ------------------------------------------------------------
# Choose "fst" for fast I/O on large tabular data, or "rds" for base R portability.
OUTPUT_FORMAT <- "fst"  # "fst" or "rds"

# Make sure fst is installed if selected
if (OUTPUT_FORMAT == "fst" && !requireNamespace("fst", quietly = TRUE)) {
  stop(
    "OUTPUT_FORMAT is 'fst' but package 'fst' is not installed.\n",
    "Install it with: install.packages('fst')\n",
    "Then run: renv::snapshot()"
  )
}

# ---- Config (MATCH YOUR YAML) ------------------------------------------------
cube_dir_rel  <- cfg_get("paths.gbif_cube_dir")
cube_map      <- cfg_get("files.cube_files")
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")

if (is.null(cube_dir_rel) || is.null(cube_map) || length(cube_map) == 0) {
  stop(
    "Cube config missing. Expected keys in config.yml:\n",
    "  paths.gbif_cube_dir\n",
    "  paths.data_proc (optional; defaults to data_proc)\n",
    "  files.cube_files.grid10km / files.cube_files.grid50km\n"
  )
}

cube_dir <- here::here(cube_dir_rel)
if (!dir.exists(cube_dir)) stop("Cube directory not found: ", cube_dir)

out_base <- here::here(data_proc_rel)
dir.create(out_base, showWarnings = FALSE, recursive = TRUE)

out_cube_dir <- file.path(out_base, "cubes")
dir.create(out_cube_dir, showWarnings = FALSE, recursive = TRUE)

manifest_path <- file.path(out_base, "cube_manifest.csv")
totals_path   <- file.path(out_base, "cube_totals_by_basisOfRecord.csv")

# ---- Large-file strategy ------------------------------------------------------
# Robust defaults:
# - Totals always computed (minimal read)
# - Full ingest skipped for large files unless enabled
LARGE_FILE_BYTES <- 1.2 * 1024^3   # 1.2 GB threshold
FULL_INGEST_LARGE_FILES <- TRUE  # set TRUE if you want to fully ingest GB-sized files

# ---- Helpers -----------------------------------------------------------------
standardize_names_dt <- function(dt) {
  nm <- names(dt)
  nm <- stringr::str_replace_all(nm, "\\s+", "_")
  nm <- stringr::str_replace_all(nm, "[^A-Za-z0-9_]", "")
  nm <- tolower(nm)
  data.table::setnames(dt, nm)
  dt
}

file_size_bytes <- function(path) as.numeric(file.info(path)$size)

read_cube_minimal <- function(path) {
  
  # read only header to get exact column names
  header <- names(data.table::fread(path, nrows = 0, encoding = "UTF-8"))
  
  # match yearmonth / eeacellcode case-insensitively
  header_l <- tolower(header)
  
  col_occ <- header[header_l == "occurrences"][1]
  col_ym  <- header[header_l == "yearmonth"][1]
  col_cell <- header[header_l == "eeacellcode"][1]
  
  if (is.na(col_occ)) stop("Could not find 'occurrences' column in: ", path)
  
  cols <- c(col_occ, col_ym, col_cell)
  cols <- cols[!is.na(cols)]
  
  data.table::fread(
    path,
    select = cols,
    showProgress = FALSE,
    encoding = "UTF-8"
  )
}


read_cube_full <- function(path) {
  data.table::fread(path, showProgress = TRUE, encoding = "UTF-8")
}

safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

write_cube <- function(dt, out_file_base, format = OUTPUT_FORMAT) {
  if (format == "fst") {
    out_path <- paste0(out_file_base, ".fst")
    # fst expects a data.frame; data.table is fine but convert explicitly for safety
    fst::write_fst(as.data.frame(dt), out_path, compress = 50)
    return(out_path)
  }
  
  if (format == "rds") {
    out_path <- paste0(out_file_base, ".rds")
    saveRDS(dt, out_path, compress = "xz")
    return(out_path)
  }
  
  stop("Unknown OUTPUT_FORMAT: ", format)
}

# ---- Loop --------------------------------------------------------------------
manifest_rows <- list()
totals_rows   <- list()

for (grid_name in names(cube_map)) {
  grid_list <- cube_map[[grid_name]]
  
  for (basisOfRecord in names(grid_list)) {
    fname <- grid_list[[basisOfRecord]]
    fpath <- file.path(cube_dir, fname)
    
    if (!file.exists(fpath)) stop("Cube file not found: ", fpath)
    
    size_b  <- file_size_bytes(fpath)
    size_gb <- size_b / 1024^3
    
    log_msg("Cube: ", grid_name, " / ", basisOfRecord,
            " | file=", fname,
            " | size=", sprintf("%.2f", size_gb), " GB")
    
    # --- Always compute totals with minimal read ------------------------------
    mini <- read_cube_minimal(fpath)
    mini <- standardize_names_dt(mini)
    
    total_occ <- if ("occurrences" %in% names(mini)) safe_sum(mini$occurrences) else NA_real_
    # IMPORTANT: yearmonth may be character or numeric; do NOT use is.finite()
    min_ym <- if ("yearmonth" %in% names(mini)) suppressWarnings(min(mini$yearmonth, na.rm = TRUE)) else NA
    max_ym <- if ("yearmonth" %in% names(mini)) suppressWarnings(max(mini$yearmonth, na.rm = TRUE)) else NA
    
    
    totals_rows[[length(totals_rows) + 1]] <- data.frame(
      grid = grid_name,
      basisOfRecord = basisOfRecord,
      source_file = fname,
      file_size_gb = round(size_gb, 3),
      total_occurrences = total_occ,
      min_yearmonth = if (!is.na(min_ym)) as.character(min_ym) else NA_character_,
      max_yearmonth = if (!is.na(max_ym)) as.character(max_ym) else NA_character_,
      stringsAsFactors = FALSE
    )
    
    # --- Full ingest decision -------------------------------------------------
    do_full <- TRUE
    if (size_b >= LARGE_FILE_BYTES && !FULL_INGEST_LARGE_FILES) {
      do_full <- FALSE
      log_msg("SKIP full ingest (large file). Totals computed. ",
              "Set FULL_INGEST_LARGE_FILES=TRUE to ingest fully.")
    }
    
    out_written <- NA_character_
    n_rows <- NA_integer_
    n_cols <- NA_integer_
    
    if (do_full) {
      dt <- read_cube_full(fpath)
      dt <- standardize_names_dt(dt)
      
      # Provenance fields
      dt[, grid := grid_name]
      dt[, basisofrecord := basisOfRecord]  # standardized internal column name
      dt[, source_file := fname]
      
      n_rows <- nrow(dt)
      n_cols <- ncol(dt)
      
      out_file_base <- file.path(out_cube_dir, paste0("cube_", grid_name, "_", basisOfRecord))
      out_written <- write_cube(dt, out_file_base, format = OUTPUT_FORMAT)
      log_msg("Wrote processed cube: ", out_written)
    } else {
      # If full ingest skipped, record minimal shape only
      n_rows <- nrow(mini)
      n_cols <- ncol(mini)
    }
    
    manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
      grid = grid_name,
      basisOfRecord = basisOfRecord,
      source_file = fname,
      file_size_gb = round(size_gb, 3),
      full_ingest = do_full,
      output_format = OUTPUT_FORMAT,
      processed_file = out_written,
      rows = n_rows,
      cols = n_cols,
      stringsAsFactors = FALSE
    )
    
    # --- Cleanup --------------------------------------------------------------
    if (exists("dt", inherits = FALSE)) rm(dt)
    if (exists("mini", inherits = FALSE)) rm(mini)
    invisible(gc())
  }
}

manifest_df <- do.call(rbind, manifest_rows)
totals_df   <- do.call(rbind, totals_rows)

readr::write_csv(manifest_df, manifest_path)
readr::write_csv(totals_df, totals_path)

log_msg("Wrote manifest: ", manifest_path)
log_msg("Wrote totals:   ", totals_path)
log_msg("Done: cubes ingested.")
