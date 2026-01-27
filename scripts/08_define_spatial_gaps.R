# scripts/08_define_spatial_gaps.R
# ==============================================================================
# Spatial Gap Analysis - Comprehensive
# ==============================================================================
# This script creates detailed spatial gap metrics across multiple dimensions:
# - Per grid cell (10km, 50km)
# - Per basis of record
# - Per taxonomic rank (if available)
# - Combined metrics
#
# Outputs include:
# - Cell-level gaps (zero coverage, low coverage)
# - Basis-specific summaries
# - Aggregate summaries
# - Threshold definitions

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

# Configuration -----------------------------------------------------------
p_derived <- here(p_data_proc, "derived")
p_gaps <- here(p_data_proc, "gaps")

dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# Quantile thresholds for "low coverage" definition
# Computed among cells with occurrences > 0
QUANTILE_THRESHOLDS <- c(0.05, 0.10, 0.25)

# Helper functions --------------------------------------------------------

#' Read cell summary safely
read_cell_summary <- function(filename) {
  path <- here(p_derived, filename)
  
  if (!file.exists(path)) {
    cli_abort("Cell summary not found: {.path {path}}")
  }
  
  dt <- fread(path)
  
  required_cols <- c("grid", "basisofrecord", "eeacellcode", "occurrences")
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0) {
    cli_abort(c(
      "Missing required columns in {.path {filename}}",
      "x" = "Missing: {paste(missing_cols, collapse = ', ')}"
    ))
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

#' Filter 50km grid to Sweden domain using 10km mask
get_sweden_cellcodes_50km <- function() {
  grid50_path <- here(p_data_proc, "grids_50km.gpkg")
  grid10_path <- here(p_data_proc, "grids_10km.gpkg")
  
  grid50 <- st_read(grid50_path, quiet = TRUE)
  grid10 <- st_read(grid10_path, quiet = TRUE)
  
  code_field <- guess_cellcode_field(names(grid50))
  
  # Ensure same CRS
  if (st_crs(grid50) != st_crs(grid10)) {
    grid10 <- st_transform(grid10, st_crs(grid50))
  }
  
  # Sweden mask
  sweden_mask <- st_union(st_geometry(grid10))
  
  # Intersect
  intersects <- st_intersects(st_geometry(grid50), sweden_mask, sparse = FALSE)[, 1]
  
  unique(as.character(grid50[[code_field]][intersects]))
}

#' Create complete cell universe (grid × cell × basis)
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
  complete_data <- cell_universe[cell_data, on = .(grid, eeacellcode, basisofrecord)]
  complete_data[is.na(occurrences), occurrences := 0]
  
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
        mean_nonzero = mean(occurrences),
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
  
  # Add mapping helper
  result[, log_occ := log10(occurrences + 1)]
  
  list(
    gaps = result,
    thresholds = thresholds
  )
}

# Load data ---------------------------------------------------------------
cli_h2("Loading Cell Summaries")

cell10 <- read_cell_summary("cell_summary_10km.csv")
cell50 <- read_cell_summary("cell_summary_50km.csv")

cli_alert_success("Loaded 10km: {scales::comma(nrow(cell10))} rows")
cli_alert_success("Loaded 50km: {scales::comma(nrow(cell50))} rows")

# Build cell universes ----------------------------------------------------
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

# Compute gaps ------------------------------------------------------------
cli_h2("Computing Spatial Gaps")

cli_alert_info("Processing 10km grid...")
gaps10 <- compute_spatial_gaps(cell10, universe10)

cli_alert_info("Processing 50km grid...")
gaps50 <- compute_spatial_gaps(cell50, universe50)

cli_alert_success("Gap analysis complete")

# Create additional summaries ---------------------------------------------
cli_h2("Creating Additional Summaries")

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

# Summaries for 10km
basis_summary_10 <- create_basis_summary(gaps10$gaps)
grid_summary_10 <- create_grid_summary(gaps10$gaps)

# Summaries for 50km  
basis_summary_50 <- create_basis_summary(gaps50$gaps)
grid_summary_50 <- create_grid_summary(gaps50$gaps)

# Combined summaries
basis_summary_all <- rbindlist(list(basis_summary_10, basis_summary_50))
grid_summary_all <- rbindlist(list(grid_summary_10, grid_summary_50))

# Write outputs -----------------------------------------------------------
cli_h2("Writing Outputs")

# Main gap files (cell-level detail)
fwrite(gaps10$gaps, here(p_gaps, "spatial_gaps_10km.csv"))
cli_alert_success("spatial_gaps_10km.csv")

fwrite(gaps50$gaps, here(p_gaps, "spatial_gaps_50km.csv"))
cli_alert_success("spatial_gaps_50km.csv")

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

# Zero-coverage cells only (for mapping priorities)
zero_cells_10 <- gaps10$gaps[gap_zero == TRUE & basisofrecord == "all", 
                               .(grid, eeacellcode, basisofrecord)]
zero_cells_50 <- gaps50$gaps[gap_zero == TRUE & basisofrecord == "all",
                               .(grid, eeacellcode, basisofrecord)]

zero_cells_all <- rbindlist(list(zero_cells_10, zero_cells_50))
fwrite(zero_cells_all, here(p_gaps, "spatial_zero_coverage_cells.csv"))
cli_alert_success("spatial_zero_coverage_cells.csv")

# Low-coverage cells (for targeted sampling)
low_cells_10 <- gaps10$gaps[gap_low_q10 == TRUE & basisofrecord == "all",
                              .(grid, eeacellcode, occurrences, q10)]
low_cells_50 <- gaps50$gaps[gap_low_q10 == TRUE & basisofrecord == "all",
                              .(grid, eeacellcode, occurrences, q10)]

low_cells_all <- rbindlist(list(low_cells_10, low_cells_50))
fwrite(low_cells_all, here(p_gaps, "spatial_low_coverage_cells_q10.csv"))
cli_alert_success("spatial_low_coverage_cells_q10.csv")

# Summary statistics ------------------------------------------------------
cli_h2("Summary Statistics")

summary_table <- tibble::tribble(
  ~metric, ~value_10km, ~value_50km,
  "Total cells analyzed", scales::comma(nrow(gaps10$gaps[basisofrecord == "all"])),
  scales::comma(nrow(gaps50$gaps[basisofrecord == "all"])),
  "Cells with data", scales::comma(sum(gaps10$gaps[basisofrecord == "all"]$has_data)),
  scales::comma(sum(gaps50$gaps[basisofrecord == "all"]$has_data)),
  "Coverage %", paste0(round(100 * mean(gaps10$gaps[basisofrecord == "all"]$has_data), 1), "%"),
  paste0(round(100 * mean(gaps50$gaps[basisofrecord == "all"]$has_data), 1), "%"),
  "Basis of record types", as.character(length(unique(gaps10$gaps$basisofrecord))),
  as.character(length(unique(gaps50$gaps$basisofrecord)))
)

print(summary_table)

cli_alert_success("Spatial gap analysis complete!")
cli_alert_info("Output location: {.path {p_gaps}}")
