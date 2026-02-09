# scripts/07_spatial_gaps.R
# ==============================================================================
# Spatial Gap Analysis
# ==============================================================================
# This script identifies spatial gaps in GBIF occurrence data:
#
# INPUTS:
#   - data_proc/derived/cell_summary_*.csv (from 06a)
#   - data_proc/grids_*.gpkg (from 02)
#
# OUTPUTS (in data_proc/gaps/):
#   - spatial_gaps_10km.csv         Cell-level gap metrics
#   - spatial_gaps_50km.csv
#   - spatial_thresholds_by_basis.csv   Quantile thresholds per basis
#   - spatial_summary_by_basis.csv      Summary stats per basis of record
#   - spatial_summary_by_grid.csv       Summary stats per grid resolution
#   - spatial_zero_coverage_cells.csv   Cells with no data
#   - spatial_low_coverage_cells_q10.csv  Bottom 10% cells
#
# GAP DEFINITIONS:
#   - Zero coverage: Cells with 0 occurrences
#   - Low coverage: Cells in bottom quantile (5%, 10%, 25%)
#   - Coverage is computed per basis of record AND for "all" combined

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(data.table)
library(sf)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# ===========================================================================
# CONFIGURATION
# ===========================================================================

p_derived <- here(p_data_proc, "derived")
p_gaps <- here(p_data_proc, "gaps")

dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# Quantile thresholds for "low coverage" definition
# Computed among cells with occurrences > 0
QUANTILE_THRESHOLDS <- cfg_get("parameters.spatial.quantile_thresholds", c(0.05, 0.10, 0.25))

cli_h1("Spatial Gap Analysis (Script 07)")
cli_alert_info("Quantile thresholds: {paste(QUANTILE_THRESHOLDS, collapse = ', ')}")

# ===========================================================================
# HELPER FUNCTIONS
# ===========================================================================

#' Read cell summary safely
read_cell_summary <- function(filename) {
  path <- here(p_derived, filename)
  
  if (!file.exists(path)) {
    cli_abort("Cell summary not found: {.path {path}}")
  }
  
  dt <- fread(path)
  
  required_cols <- c("basisofrecord", "eeacellcode", "occurrences")
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0) {
    cli_abort(c(
      "Missing required columns in {.path {filename}}",
      "x" = "Missing: {paste(missing_cols, collapse = ', ')}"
    ))
  }
  
  # Add grid column if missing
  if (!("grid" %in% names(dt))) {
    grid_suffix <- str_extract(filename, "\\d+km")
    dt[, grid := paste0("grid", grid_suffix)]
  }
  
  dt
}

#' Get all cell codes from grid file
get_all_cellcodes <- function(grid_path) {
  if (!file.exists(grid_path)) {
    cli_abort("Grid file not found: {.path {grid_path}}")
  }
  
  grid <- st_read(grid_path, quiet = TRUE)
  code_field <- guess_cellcode_field(names(grid))
  
  if (is.na(code_field)) {
    cli_abort(c(
      "Could not identify cell code field",
      "i" = "Available fields: {paste(names(grid), collapse = ', ')}"
    ))
  }
  
  unique(as.character(grid[[code_field]]))
}

#' Filter 50km grid to Sweden domain using 10km grid as mask
get_sweden_cellcodes_50km <- function() {
  grid50_path <- here(p_data_proc, "grids_50km.gpkg")
  grid10_path <- here(p_data_proc, "grids_10km.gpkg")
  
  if (!file.exists(grid50_path) || !file.exists(grid10_path)) {
    cli_abort("Grid files not found")
  }
  
  grid50 <- st_read(grid50_path, quiet = TRUE)
  grid10 <- st_read(grid10_path, quiet = TRUE)
  
  code_field <- guess_cellcode_field(names(grid50))
  
  # Ensure same CRS
  if (st_crs(grid50) != st_crs(grid10)) {
    grid10 <- st_transform(grid10, st_crs(grid50))
  }
  
  # Create Sweden mask from 10km grid
  sweden_mask <- st_union(st_geometry(grid10))
  
  # Find 50km cells that intersect Sweden
  intersects <- st_intersects(st_geometry(grid50), sweden_mask, sparse = FALSE)[, 1]
  
  unique(as.character(grid50[[code_field]][intersects]))
}

#' Create complete cell universe (all cells × all basis types)
make_cell_universe <- function(cell_data, all_cellcodes) {
  setDT(cell_data)
  
  grids <- unique(cell_data$grid)
  basis_levels <- unique(cell_data$basisofrecord)
  
  # Create all combinations
  universe <- CJ(
    grid = grids,
    eeacellcode = all_cellcodes,
    basisofrecord = basis_levels
  )
  
  universe
}

#' Compute comprehensive spatial gaps
compute_spatial_gaps <- function(cell_data, cell_universe, quantiles = QUANTILE_THRESHOLDS) {
  setDT(cell_data)
  setDT(cell_universe)
  
  # Ensure numeric occurrences
  cell_data[, occurrences := as.numeric(occurrences)]
  cell_data[is.na(occurrences), occurrences := 0]
  
  # Complete the data: left join universe with observed data
  complete_data <- merge(
    cell_universe,
    cell_data[, .(grid, eeacellcode, basisofrecord, occurrences, n_species)],
    by = c("grid", "eeacellcode", "basisofrecord"),
    all.x = TRUE
  )
  complete_data[is.na(occurrences), occurrences := 0]
  complete_data[is.na(n_species), n_species := 0]
  
  # Gap definitions per cell × basis
  complete_data[, gap_zero := (occurrences == 0)]
  complete_data[, has_data := (occurrences > 0)]
  
  # Compute quantile thresholds per grid × basis (among non-zero cells)
  thresholds <- complete_data[occurrences > 0, {
    quants <- quantile(occurrences, probs = quantiles, na.rm = TRUE, names = FALSE)
    names(quants) <- paste0("q", str_pad(as.integer(quantiles * 100), 2, pad = "0"))
    as.list(c(
      quants,
      list(
        n_nonzero = .N,
        mean_nonzero = round(mean(occurrences), 2),
        median_nonzero = median(occurrences),
        max_occurrences = max(occurrences)
      )
    ))
  }, by = .(grid, basisofrecord)]
  
  # Join thresholds back
  result <- merge(
    complete_data,
    thresholds,
    by = c("grid", "basisofrecord"),
    all.x = TRUE
  )
  
  # Define low-coverage gaps (for each quantile)
  for (q in quantiles) {
    q_col <- paste0("q", str_pad(as.integer(q * 100), 2, pad = "0"))
    gap_col <- paste0("gap_low_", q_col)
    
    result[, (gap_col) := (occurrences > 0 & !is.na(get(q_col)) & occurrences <= get(q_col))]
  }
  
  # Cell-level aggregates (across all basis of record)
  cell_aggregates <- result[, .(
    total_occurrences_cell = sum(occurrences),
    n_basis_with_data = sum(has_data),
    n_basis_zero = sum(gap_zero),
    cell_has_any_data = any(has_data),
    cell_all_zero = all(gap_zero)
  ), by = .(grid, eeacellcode)]
  
  # Join back
  result <- merge(result, cell_aggregates, by = c("grid", "eeacellcode"), all.x = TRUE)
  
  # Add log-transformed occurrence for mapping
  result[, log_occ := log10(occurrences + 1)]
  
  list(
    gaps = result,
    thresholds = thresholds
  )
}

#' Create basis-level summary
create_basis_summary <- function(gaps_data) {
  gaps_data[, .(
    n_cells_total = .N,
    n_cells_zero = sum(gap_zero),
    n_cells_with_data = sum(has_data),
    pct_zero = round(100 * mean(gap_zero), 2),
    pct_with_data = round(100 * mean(has_data), 2),
    total_occurrences = sum(occurrences),
    mean_occurrences = round(mean(occurrences[occurrences > 0]), 2),
    median_occurrences = median(occurrences[occurrences > 0])
  ), by = .(grid, basisofrecord)]
}

#' Create grid-level summary
create_grid_summary <- function(gaps_data) {
  gaps_data[basisofrecord == "all", .(
    n_cells_total = .N,
    n_cells_with_data = sum(has_data),
    n_cells_zero = sum(gap_zero),
    pct_coverage = round(100 * mean(has_data), 2),
    total_occurrences = sum(occurrences),
    mean_per_cell = round(mean(occurrences), 2),
    median_per_cell = median(occurrences)
  ), by = grid]
}

# ===========================================================================
# LOAD DATA
# ===========================================================================

cli_h2("Loading Cell Summaries")

cell10 <- read_cell_summary("cell_summary_10km.csv")
cell50 <- read_cell_summary("cell_summary_50km.csv")

cli_alert_success("Loaded 10km: {scales::comma(nrow(cell10))} rows")
cli_alert_success("Loaded 50km: {scales::comma(nrow(cell50))} rows")

# ===========================================================================
# BUILD CELL UNIVERSES
# ===========================================================================

cli_h2("Building Cell Universes")

grid10_path <- here(p_data_proc, "grids_10km.gpkg")
grid50_path <- here(p_data_proc, "grids_50km.gpkg")

cli_alert_info("Getting all 10km cell codes...")
codes10 <- get_all_cellcodes(grid10_path)
cli_alert_success("{scales::comma(length(codes10))} cells in 10km grid")

cli_alert_info("Getting Sweden-domain 50km cell codes...")
codes50_sweden <- get_sweden_cellcodes_50km()
cli_alert_success("{scales::comma(length(codes50_sweden))} cells in 50km grid (Sweden)")

# Filter 50km data to Sweden domain
cell50 <- as.data.table(cell50)
cell50 <- cell50[eeacellcode %in% codes50_sweden]
cli_alert_info("Filtered 50km data to Sweden: {scales::comma(nrow(cell50))} rows")

# Create universes
universe10 <- make_cell_universe(cell10, codes10)
universe50 <- make_cell_universe(cell50, codes50_sweden)

cli_alert_success("Universe 10km: {scales::comma(nrow(universe10))} combinations")
cli_alert_success("Universe 50km: {scales::comma(nrow(universe50))} combinations")

# ===========================================================================
# COMPUTE SPATIAL GAPS
# ===========================================================================

cli_h2("Computing Spatial Gaps")

cli_alert_info("Processing 10km grid...")
gaps10 <- compute_spatial_gaps(cell10, universe10)

cli_alert_info("Processing 50km grid...")
gaps50 <- compute_spatial_gaps(cell50, universe50)

cli_alert_success("Gap analysis complete")

# ===========================================================================
# CREATE SUMMARIES
# ===========================================================================

cli_h2("Creating Summaries")

# By basis of record
basis_summary_10 <- create_basis_summary(gaps10$gaps)
basis_summary_50 <- create_basis_summary(gaps50$gaps)
basis_summary_all <- rbindlist(list(basis_summary_10, basis_summary_50))

# By grid
grid_summary_10 <- create_grid_summary(gaps10$gaps)
grid_summary_50 <- create_grid_summary(gaps50$gaps)
grid_summary_all <- rbindlist(list(grid_summary_10, grid_summary_50))

# ===========================================================================
# WRITE OUTPUTS
# ===========================================================================

cli_h2("Writing Outputs")

# Main gap files (cell-level detail)
fwrite(gaps10$gaps, here(p_gaps, "spatial_gaps_10km.csv"))
cli_alert_success("spatial_gaps_10km.csv: {scales::comma(nrow(gaps10$gaps))} rows")

fwrite(gaps50$gaps, here(p_gaps, "spatial_gaps_50km.csv"))
cli_alert_success("spatial_gaps_50km.csv: {scales::comma(nrow(gaps50$gaps))} rows")

# Thresholds
thresholds_all <- rbindlist(list(
  gaps10$thresholds[, grid_size := "10km"],
  gaps50$thresholds[, grid_size := "50km"]
))
fwrite(thresholds_all, here(p_gaps, "spatial_thresholds_by_basis.csv"))
cli_alert_success("spatial_thresholds_by_basis.csv")

# Summary tables
fwrite(basis_summary_all, here(p_gaps, "spatial_summary_by_basis.csv"))
cli_alert_success("spatial_summary_by_basis.csv")

fwrite(grid_summary_all, here(p_gaps, "spatial_summary_by_grid.csv"))
cli_alert_success("spatial_summary_by_grid.csv")

# Zero-coverage cells (for mapping priorities)
zero_cells_10 <- gaps10$gaps[gap_zero == TRUE & basisofrecord == "all", 
                              .(grid, eeacellcode)]
zero_cells_50 <- gaps50$gaps[gap_zero == TRUE & basisofrecord == "all",
                              .(grid, eeacellcode)]
zero_cells_all <- rbindlist(list(zero_cells_10, zero_cells_50))
fwrite(zero_cells_all, here(p_gaps, "spatial_zero_coverage_cells.csv"))
cli_alert_success("spatial_zero_coverage_cells.csv: {scales::comma(nrow(zero_cells_all))} cells")

# Low-coverage cells (bottom 10% - for targeted sampling)
low_cells_10 <- gaps10$gaps[gap_low_q10 == TRUE & basisofrecord == "all",
                             .(grid, eeacellcode, occurrences, q10)]
low_cells_50 <- gaps50$gaps[gap_low_q10 == TRUE & basisofrecord == "all",
                             .(grid, eeacellcode, occurrences, q10)]
low_cells_all <- rbindlist(list(low_cells_10, low_cells_50))
fwrite(low_cells_all, here(p_gaps, "spatial_low_coverage_cells_q10.csv"))
cli_alert_success("spatial_low_coverage_cells_q10.csv: {scales::comma(nrow(low_cells_all))} cells")

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 07)")

summary_table <- data.table(
  Metric = c("Total cells analyzed", "Cells with data", "Coverage %", "Zero-coverage cells"),
  `10km` = c(
    scales::comma(nrow(gaps10$gaps[basisofrecord == "all"])),
    scales::comma(sum(gaps10$gaps[basisofrecord == "all"]$has_data)),
    paste0(round(100 * mean(gaps10$gaps[basisofrecord == "all"]$has_data), 1), "%"),
    scales::comma(nrow(zero_cells_10))
  ),
  `50km` = c(
    scales::comma(nrow(gaps50$gaps[basisofrecord == "all"])),
    scales::comma(sum(gaps50$gaps[basisofrecord == "all"]$has_data)),
    paste0(round(100 * mean(gaps50$gaps[basisofrecord == "all"]$has_data), 1), "%"),
    scales::comma(nrow(zero_cells_50))
  )
)

print(summary_table)

cli_alert_success("Spatial gap analysis complete!")
cli_alert_info("Output location: {.path {p_gaps}}")
cli_alert_info("Created 7 spatial gap files")
