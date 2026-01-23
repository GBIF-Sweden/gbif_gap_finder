# scripts/07_define_spatial_gaps.R
# Phase 3: Spatial gap metrics (per EEA cell)
#
# Inputs (Phase 2):
#   - data_proc/derived/cell_summary_10km.csv
#   - data_proc/derived/cell_summary_50km.csv
#
# Additional inputs (to build "all cells" universe):
#   - data_proc/grids_10km.gpkg
#   - data_proc/grids_50km.gpkg
#
# Outputs (Phase 3):
#   - data_proc/gaps/spatial_gaps_10km.csv
#   - data_proc/gaps/spatial_gaps_50km.csv
#   - data_proc/gaps/spatial_thresholds_by_basis.csv

source("scripts/00_setup.R")

# ---- Dependencies -------------------------------------------------------------
pkgs <- c("data.table", "readr", "dplyr", "stringr", "sf")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("Missing package: ", p)

# ---- Paths -------------------------------------------------------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
p_data_proc <- here::here(data_proc_rel)

p_derived <- file.path(p_data_proc, cfg_get("derived.dir", "derived"))
p_gaps <- file.path(p_data_proc, "gaps")
dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# ---- Parameters --------------------------------------------------------------
# Quantile thresholds computed among cells with occurrences > 0 (per grid + basisofrecord)
Q_LOW <- c(0.05, 0.10)  # currently used as q05/q10

# ---- Helpers -----------------------------------------------------------------

read_cell_summary <- function(fname) {
  path <- file.path(p_derived, fname)
  if (!file.exists(path)) stop("Missing derived file: ", path)
  dt <- data.table::fread(path)
  # expected: grid, basisofrecord, eeacellcode, occurrences
  need <- c("grid", "basisofrecord", "eeacellcode", "occurrences")
  miss <- setdiff(need, names(dt))
  if (length(miss)) stop("Missing columns in ", fname, ": ", paste(miss, collapse = ", "))
  dt
}

# Guess the cell-code field in a grid gpkg
guess_code_field <- function(nms) {
  n <- tolower(nms)
  cand <- nms[grepl("eea", n) & grepl("code", n)]
  if (length(cand) > 0) return(cand[1])
  cand <- nms[grepl("cell", n) & grepl("code", n)]
  if (length(cand) > 0) return(cand[1])
  cand <- nms[grepl("eea|cell|code|grid", n)]
  if (length(cand) > 0) return(cand[1])
  NA_character_
}

# Read all cell codes from a gpkg (single layer assumed)
get_all_cell_codes_from_gpkg <- function(gpkg_path) {
  stopifnot(file.exists(gpkg_path))
  g <- sf::st_read(gpkg_path, quiet = TRUE)
  code_field <- guess_code_field(names(g))
  if (is.na(code_field)) stop("Could not detect EEA cell code field in: ", gpkg_path)
  unique(as.character(g[[code_field]]))
}

# --- Build Sweden-domain 50km cell codes using the 10km Sweden grid as mask ----
get_sweden_codes_50km <- function(p_data_proc, grid50_file = "grids_50km.gpkg", grid10_file = "grids_10km.gpkg") {
  grid50_path <- file.path(p_data_proc, grid50_file)
  grid10_path <- file.path(p_data_proc, grid10_file)
  
  stopifnot(file.exists(grid50_path), file.exists(grid10_path))
  
  g50 <- sf::st_read(grid50_path, quiet = TRUE)
  g10 <- sf::st_read(grid10_path, quiet = TRUE)
  
  code_field_50 <- guess_code_field(names(g50))
  if (is.na(code_field_50)) stop("Could not detect EEA cell code field in grids_50km.gpkg")
  
  # Ensure same CRS
  if (sf::st_crs(g50) != sf::st_crs(g10)) {
    g10 <- sf::st_transform(g10, sf::st_crs(g50))
  }
  
  # Sweden mask = union of Sweden 10km grid
  sw_mask <- sf::st_union(sf::st_geometry(g10))
  
  # Keep only 50km cells that intersect Sweden mask
  hits <- sf::st_intersects(sf::st_geometry(g50), sw_mask, sparse = FALSE)[, 1]
  codes <- as.character(g50[[code_field_50]][hits])
  
  unique(codes)
}

# Build a "cell universe" table (grid x eeacellcode) for completing missing zeros
make_cell_universe <- function(dt, cell_codes) {
  data.table::setDT(dt)
  grids <- unique(dt$grid)
  # If dt has multiple grid labels, replicate cell_codes across them
  u <- data.table::CJ(grid = grids, eeacellcode = unique(cell_codes))
  u
}

# Core computation: complete grid x cell x BOR, compute BOR gaps + overall gaps + thresholds
compute_spatial_gaps <- function(dt, cell_universe, q_low = Q_LOW) {
  data.table::setDT(dt)
  data.table::setDT(cell_universe)
  
  # Ensure correct types
  dt[, occurrences := as.numeric(occurrences)]
  dt[is.na(occurrences), occurrences := 0]
  
  # BOR levels present in this dataset
  bor_levels <- sort(unique(dt$basisofrecord))
  
  # Complete: (grid x eeacellcode) x basisofrecord
  full <- cell_universe[
    , .(basisofrecord = bor_levels), by = .(grid, eeacellcode)
  ]
  
  # Join observed data; missing combos => 0
  full <- dt[full, on = .(grid, eeacellcode, basisofrecord)]
  full[is.na(occurrences), occurrences := 0]
  
  # BOR-specific zero coverage
  full[, gap_zero := (occurrences == 0)]
  
  # ---- Overall (cell-level) definitions across BORs --------------------------
  # any_occ: at least one BOR has occurrences > 0
  # all_zero: all BORs are zero
  # missing_some_bor: at least one BOR is zero AND at least one BOR is non-zero
  cell_overall <- full[, .(
    cell_any_occ = any(occurrences > 0),
    cell_all_zero = all(occurrences == 0),
    cell_missing_some_bor = any(occurrences == 0) & any(occurrences > 0)
  ), by = .(grid, eeacellcode)]
  
  full <- merge(full, cell_overall, by = c("grid", "eeacellcode"), all.x = TRUE)
  
  # ---- Quantile thresholds among non-zero cells, per grid + basisofrecord ----
  thresh <- full[occurrences > 0,
                 .(
                   q05 = stats::quantile(occurrences, probs = 0.05, na.rm = TRUE, names = FALSE, type = 7),
                   q10 = stats::quantile(occurrences, probs = 0.10, na.rm = TRUE, names = FALSE, type = 7),
                   n_nonzero = .N
                 ),
                 by = .(grid, basisofrecord)
  ]
  
  # Join thresholds back
  out <- merge(full, thresh, by = c("grid", "basisofrecord"), all.x = TRUE)
  
  # Low-coverage gaps (relative, only among non-zero)
  out[, gap_low_q05 := (occurrences > 0 & !is.na(q05) & occurrences <= q05)]
  out[, gap_low_q10 := (occurrences > 0 & !is.na(q10) & occurrences <= q10)]
  
  # Mapping helper
  out[, log_occ := log10(occurrences + 1)]
  
  list(out = out, thresholds = thresh)
}

# ---- Run ---------------------------------------------------------------------
cell10_name <- cfg_get("derived.outputs.cell_summary_10km", "cell_summary_10km.csv")
cell50_name <- cfg_get("derived.outputs.cell_summary_50km", "cell_summary_50km.csv")

cell10 <- read_cell_summary(cell10_name)
cell50 <- read_cell_summary(cell50_name)  # <-- FIX: you were missing this

# Build cell universes from grids
grid10_path <- file.path(p_data_proc, "grids_10km.gpkg")
grid50_path <- file.path(p_data_proc, "grids_50km.gpkg")

codes10 <- get_all_cell_codes_from_gpkg(grid10_path)
u10 <- make_cell_universe(cell10, codes10)

# Filter 50km universe to Sweden-domain BEFORE completing
sweden_codes_50 <- get_sweden_codes_50km(p_data_proc)
u50 <- make_cell_universe(cell50, sweden_codes_50)

# Also filter the observed 50km summaries to Sweden-domain cells (keeps things consistent)
cell50 <- data.table::as.data.table(cell50)
cell50 <- cell50[eeacellcode %in% sweden_codes_50]

res10 <- compute_spatial_gaps(cell10, cell_universe = u10)
res50 <- compute_spatial_gaps(cell50, cell_universe = u50)

out10 <- file.path(p_gaps, "spatial_gaps_10km.csv")
out50 <- file.path(p_gaps, "spatial_gaps_50km.csv")
thr_out <- file.path(p_gaps, "spatial_thresholds_by_basis.csv")

data.table::fwrite(res10$out, out10)
data.table::fwrite(res50$out, out50)

# Add a grid_size label so thresholds from 10km and 50km are distinguishable if grid labels overlap
thr10 <- data.table::copy(res10$thresholds)[, grid_size := "10km"]
thr50 <- data.table::copy(res50$thresholds)[, grid_size := "50km"]

thr_all <- data.table::rbindlist(list(thr10, thr50), use.names = TRUE, fill = TRUE)
data.table::fwrite(thr_all, thr_out)

log_msg("Wrote: ", out10)
log_msg("Wrote: ", out50)
log_msg("Wrote: ", thr_out)
log_msg("Phase 3 (spatial) complete.")
