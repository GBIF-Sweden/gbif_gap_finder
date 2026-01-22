# scripts/05_make_derived_summaries.R
# Create analysis-ready derived datasets from processed cube files (.fst)
#
# Inputs:
#   - data_proc/cubes/*.fst  (created by scripts/03_ingest_gbif_cubes.R)
#
# Outputs (written to data_proc/derived/):
#   - cell_summary_10km.csv, cell_summary_50km.csv
#   - time_summary_10km.csv, time_summary_50km.csv
#   - species_summary_10km.csv, species_summary_50km.csv
#   - cube_key_summary.csv
#
# Notes:
# - Reads only required columns from .fst (fast and memory-safe).
# - Aggregates per file first (small), then merges aggregates across files.

source("scripts/00_setup.R")

# ---- Paths (from your YAML) ---------------------------------------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
p_data_proc   <- here::here(data_proc_rel)

p_cubes   <- file.path(p_data_proc, "cubes")
p_derived <- file.path(p_data_proc, "derived")
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)

if (!dir.exists(p_cubes)) stop("Cubes folder not found: ", p_cubes)

# ---- Dependencies -------------------------------------------------------------
if (!requireNamespace("fst", quietly = TRUE)) {
  stop("Package 'fst' is required (processed cubes are .fst). Install with install.packages('fst') and renv::snapshot().")
}

# ---- Helpers -----------------------------------------------------------------
required_cols <- c(
  "grid", "basisofrecord", "source_file",
  "eeacellcode", "yearmonth",
  "specieskey", "species",
  "occurrences"
)

read_cube_cols <- function(path, cols = required_cols) {
  # fst::read_fst can select columns; this avoids loading huge objects unnecessarily.
  meta <- fst::metadata_fst(path)
  available <- meta$columnNames
  missing <- setdiff(cols, available)
  
  # We'll proceed if "occurrences" exists; other cols are needed for specific summaries.
  if (!("occurrences" %in% available)) stop("Missing 'occurrences' in: ", path)
  
  cols2 <- intersect(cols, available)
  df <- fst::read_fst(path, columns = cols2)
  
  # Ensure standard names (fst preserves names; ingestion uses lowercase already)
  data.table::setDT(df)
  df
}

safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

add_overall <- function(dt, group_cols) {
  # Adds an "all" basisOfRecord rollup alongside existing basisofrecord groups.
  # Input dt should have columns: grid, basisofrecord, plus group_cols and occurrences.
  overall <- dt[, .(occurrences = safe_sum(occurrences)),
                by = c("grid", group_cols)]
  overall[, basisofrecord := "all"]
  # Align columns and rbind
  dt2 <- data.table::rbindlist(list(dt, overall), use.names = TRUE, fill = TRUE)
  dt2
}

# ---- Locate cube files --------------------------------------------------------
cube_files <- list.files(p_cubes, pattern = "\\.fst$", full.names = TRUE)
if (length(cube_files) == 0) stop("No .fst cube files found in: ", p_cubes)

log_msg("Found ", length(cube_files), " processed cube files (.fst).")

# ---- Accumulators (store only aggregated outputs; keep memory low) ------------
cell_aggs   <- list()
time_aggs   <- list()
species_aggs <- list()
key_stats   <- list()

# ---- Process each cube file ---------------------------------------------------
for (f in cube_files) {
  log_msg("Summarizing: ", basename(f))
  
  dt <- read_cube_cols(f)
  
  # Ensure key columns exist (some cube variants might omit some; we handle gracefully)
  have_cell   <- "eeacellcode" %in% names(dt)
  have_time   <- "yearmonth"   %in% names(dt)
  have_specieskey <- "specieskey" %in% names(dt)
  have_species <- "species" %in% names(dt)
  
  # Standardize provenance columns if missing (should not happen if ingestion added them)
  if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
  if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
  if (!("source_file" %in% names(dt))) dt[, source_file := basename(f)]
  
  # ---- Key coverage stats (always) -------------------------------------------
  key_stats[[length(key_stats) + 1]] <- data.table::data.table(
    file = basename(f),
    grid = if (!all(is.na(dt$grid))) unique(dt$grid)[1] else NA_character_,
    basisOfRecord = if (!all(is.na(dt$basisofrecord))) unique(dt$basisofrecord)[1] else NA_character_,
    rows = nrow(dt),
    total_occurrences = safe_sum(dt$occurrences),
    n_cells = if (have_cell) data.table::uniqueN(dt$eeacellcode) else NA_integer_,
    n_months = if (have_time) data.table::uniqueN(dt$yearmonth) else NA_integer_,
    n_specieskeys = if (have_specieskey) data.table::uniqueN(dt$specieskey) else NA_integer_
  )
  
  # ---- Cell summary -----------------------------------------------------------
  if (have_cell) {
    cell_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, eeacellcode)]
    cell_dt <- add_overall(cell_dt, group_cols = "eeacellcode")
    cell_aggs[[length(cell_aggs) + 1]] <- cell_dt
  }
  
  # ---- Time summary -----------------------------------------------------------
  if (have_time) {
    time_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, yearmonth)]
    time_dt <- add_overall(time_dt, group_cols = "yearmonth")
    time_aggs[[length(time_aggs) + 1]] <- time_dt
  }
  
  # ---- Species summary --------------------------------------------------------
  if (have_specieskey) {
    # Keep species name if present (helps later matching/QA)
    if (have_species) {
      sp_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, specieskey, species)]
      # rollup across basisOfRecord while keeping species label
      sp_all <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, specieskey, species)]
      sp_all[, basisofrecord := "all"]
      sp_dt <- data.table::rbindlist(list(sp_dt, sp_all), use.names = TRUE, fill = TRUE)
    } else {
      sp_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, specieskey)]
      sp_dt <- add_overall(sp_dt, group_cols = "specieskey")
    }
    species_aggs[[length(species_aggs) + 1]] <- sp_dt
  }
  
  rm(dt)
  invisible(gc())
}

# ---- Combine aggregates across files (sum again to be safe) -------------------
log_msg("Combining per-file aggregates...")

# Cell
cell_all <- if (length(cell_aggs)) data.table::rbindlist(cell_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(cell_all)) {
  cell_all <- cell_all[, .(occurrences = safe_sum(occurrences)),
                       by = .(grid, basisofrecord, eeacellcode)]
}

# Time
time_all <- if (length(time_aggs)) data.table::rbindlist(time_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(time_all)) {
  time_all <- time_all[, .(occurrences = safe_sum(occurrences)),
                       by = .(grid, basisofrecord, yearmonth)]
}

# Species
species_all <- if (length(species_aggs)) data.table::rbindlist(species_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(species_all)) {
  if ("species" %in% names(species_all)) {
    species_all <- species_all[, .(occurrences = safe_sum(occurrences)),
                               by = .(grid, basisofrecord, specieskey, species)]
  } else {
    species_all <- species_all[, .(occurrences = safe_sum(occurrences)),
                               by = .(grid, basisofrecord, specieskey)]
  }
}

# Key stats
key_df <- data.table::rbindlist(key_stats, use.names = TRUE, fill = TRUE)

# ---- Write outputs ------------------------------------------------------------
log_msg("Writing derived outputs to: ", p_derived)

# Split by grid
write_split_by_grid <- function(dt, grid_value, filename) {
  out <- dt[grid == grid_value]
  out_path <- file.path(p_derived, filename)
  data.table::fwrite(out, out_path)
  log_msg("Wrote: ", out_path, " (rows=", nrow(out), ")")
}

# Determine grid labels used in your ingestion (typically "grid10km" and "grid50km")
grid_levels <- unique(key_df$grid)
log_msg("Detected grids: ", paste(grid_levels, collapse = ", "))

# Cell summaries
if (!is.null(cell_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(cell_all, "grid10km", "cell_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(cell_all, "grid50km", "cell_summary_50km.csv")
}

# Time summaries
if (!is.null(time_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(time_all, "grid10km", "time_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(time_all, "grid50km", "time_summary_50km.csv")
}

# Species summaries
if (!is.null(species_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(species_all, "grid10km", "species_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(species_all, "grid50km", "species_summary_50km.csv")
}

# Key coverage summary
key_out <- file.path(p_derived, "cube_key_summary.csv")
data.table::fwrite(key_df, key_out)
log_msg("Wrote: ", key_out, " (rows=", nrow(key_df), ")")

log_msg("Derived datasets created successfully.")
