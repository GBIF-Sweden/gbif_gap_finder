# scripts/07_spatial_gaps.R
# ============================================================================
# Spatial Gap Analysis
# ============================================================================
# Purpose:
#   Identify spatial gaps in GBIF occurrence data by comparing observed
#   cell coverage against the full grid universe. Computes zero-coverage
#   and low-coverage metrics at configurable quantile thresholds.
#
# Inputs:
#   - data/{CC}/proc/derived/cell_summary_*.csv  (from 06a)
#   - data/{CC}/proc/grids_*.gpkg               (from 02)
#
# Outputs (in data/{CC}/proc/gaps/):
#   - spatial_gaps_10km.csv              Cell-level gap metrics
#   - spatial_gaps_50km.csv
#   - spatial_thresholds_by_basis.csv    Quantile thresholds per basis
#   - spatial_summary_by_basis.csv       Summary per basisOfRecord
#   - spatial_summary_by_grid.csv        Summary per grid resolution
#   - spatial_zero_coverage_cells.csv    Cells with no data
#   - spatial_low_coverage_cells_q10.csv Bottom 10% cells
#
# Gap definitions:
#   Zero coverage: cells with 0 occurrences
#   Low coverage:  cells in bottom quantile (5%, 10%, 25%)
#   Coverage is computed per basis of record AND for "all" combined
#
# Dependencies: scripts/00_setup.R, data.table, sf
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# ============================================================================
# Configuration
# ============================================================================

# p_derived and p_gaps are defined in R/globals.R
# Directories created by ensure_dirs() in 00_setup.R

QUANTILE_THRESHOLDS <- cfg_get(
  "parameters.spatial.quantile_thresholds", c(0.05, 0.10, 0.25)
)

cli_h1("Spatial Gap Analysis (Script 07)")
cli_alert_info(
  "Quantile thresholds: {paste(QUANTILE_THRESHOLDS, collapse = ', ')}"
)

# ============================================================================
# Helper Functions
# ============================================================================

# read_derived_summary(), guess_cellcode_field() are defined in R/globals.R

#' Read cell summary (wrapper around read_derived_summary)
read_cell_summary <- function(filename) {
  read_derived_summary(filename,
    required_cols = c("basisofrecord", "eeacellcode", "occurrences"))
}

#' Get all cell codes from a grid file
get_all_cellcodes <- function(grid_path) {
  if (!file.exists(grid_path)) {
    cli_abort("Grid file not found: {.path {grid_path}}")
  }

  grid       <- st_read(grid_path, quiet = TRUE)
  code_field <- guess_cellcode_field(names(grid))

  if (is.na(code_field)) {
    cli_abort(c(
      "Could not identify cell code field",
      "i" = "Available: {paste(names(grid), collapse = ', ')}"
    ))
  }

  unique(as.character(grid[[code_field]]))
}

#' Filter coarse grid to country domain using fine grid as mask
#'
#' Uses the fine grid (e.g., 10km) as a spatial mask to determine
#' which coarse grid (e.g., 50km) cells intersect the study area.
#' This is country-agnostic — works for any pair of grids.
#'
#' @param coarse_path Path to coarse grid GeoPackage
#' @param fine_path   Path to fine grid GeoPackage (defines study area)
#' @return Character vector of cell codes within the study area
filter_coarse_grid_to_country <- function(
    coarse_path = here(p_data_proc, "grids_50km.gpkg"),
    fine_path   = here(p_data_proc, "grids_10km.gpkg")) {

  if (!file.exists(coarse_path) || !file.exists(fine_path)) {
    cli_abort("Grid files not found")
  }

  coarse_grid <- st_read(coarse_path, quiet = TRUE)
  fine_grid   <- st_read(fine_path, quiet = TRUE)

  code_field <- guess_cellcode_field(names(coarse_grid))

  # Ensure same CRS
  if (st_crs(coarse_grid) != st_crs(fine_grid)) {
    fine_grid <- st_transform(fine_grid, st_crs(coarse_grid))
  }

  # Create country mask from fine grid extent
  country_mask <- st_union(st_geometry(fine_grid))

  # Find coarse cells that intersect the country
  intersects <- st_intersects(
    st_geometry(coarse_grid), country_mask, sparse = FALSE
  )[, 1]

  unique(as.character(coarse_grid[[code_field]][intersects]))
}

#' Compute comprehensive spatial gaps
#'
#' Completion to the full grid (every cell x basis, zeros filled, with
#' `has_data` / `gap_zero` flags) is delegated to `complete_to_grid()` in
#' R/globals.R — the same helper 09c uses, so the two zero-fills cannot drift.
#' `all_cellcodes` is the full set of grid cells from the grid file;
#' `complete_to_grid()` aborts if it is empty rather than silently zero-filling
#' to data-only cells (which would report ~100% coverage and hide empty cells).
compute_spatial_gaps <- function(cell_data,
                                 all_cellcodes,
                                 quantiles = QUANTILE_THRESHOLDS) {
  setDT(cell_data)

  cell_data[, occurrences := as.numeric(occurrences)]
  cell_data[is.na(occurrences), occurrences := 0]

  complete_data <- complete_to_grid(
    cell_data[, .(grid, eeacellcode, basisofrecord, occurrences, n_species)],
    all_cellcodes,
    facet_cols = c("grid", "basisofrecord")
  )

  # Quantile thresholds per grid × basis (non-zero cells only)
  thresholds <- complete_data[occurrences > 0, {
    quants <- quantile(
      occurrences,
      probs = quantiles, na.rm = TRUE, names = FALSE
    )
    names(quants) <- paste0(
      "q", str_pad(as.integer(quantiles * 100), 2, pad = "0")
    )
    as.list(c(
      quants,
      list(
        n_nonzero        = .N,
        mean_nonzero     = round(mean(occurrences), 2),
        median_nonzero   = median(occurrences),
        max_occurrences  = max(occurrences)
      )
    ))
  }, by = .(grid, basisofrecord)]

  result <- merge(
    complete_data, thresholds,
    by = c("grid", "basisofrecord"), all.x = TRUE
  )

  # Low-coverage gap flags
  for (q in quantiles) {
    q_col   <- paste0("q", str_pad(as.integer(q * 100), 2, pad = "0"))
    gap_col <- paste0("gap_low_", q_col)
    result[, (gap_col) := (
      occurrences > 0 &
        !is.na(get(q_col)) &
        occurrences <= get(q_col)
    )]
  }

  # Cell-level aggregates (across REAL basis types only).
  # Exclude the synthetic basisofrecord == "all" row: it already equals the sum
  # over the real bases, so including it double-counted total_occurrences_cell
  # (~2x) and added a phantom basis to n_basis_with_data / n_basis_zero.
  cell_agg <- result[basisofrecord != "all", .(
    total_occurrences_cell = sum(occurrences),
    n_basis_with_data      = sum(has_data),
    n_basis_zero           = sum(gap_zero),
    cell_has_any_data      = any(has_data),
    cell_all_zero          = all(gap_zero)
  ), by = .(grid, eeacellcode)]

  result <- merge(
    result, cell_agg,
    by = c("grid", "eeacellcode"), all.x = TRUE
  )

  result[, log_occ := log10(occurrences + 1)]

  list(gaps = result, thresholds = thresholds)
}

#' Basis-level summary statistics
create_basis_summary <- function(gaps_data) {
  gaps_data[, .(
    n_cells_total      = .N,
    n_cells_zero       = sum(gap_zero),
    n_cells_with_data  = sum(has_data),
    pct_zero           = round(100 * mean(gap_zero), 2),
    pct_with_data      = round(100 * mean(has_data), 2),
    total_occurrences  = sum(occurrences),
    mean_occurrences   = round(mean(occurrences[occurrences > 0]), 2),
    median_occurrences = median(occurrences[occurrences > 0])
  ), by = .(grid, basisofrecord)]
}

#' Grid-level summary statistics
create_grid_summary <- function(gaps_data) {
  gaps_data[basisofrecord == "all", .(
    n_cells_total     = .N,
    n_cells_with_data = sum(has_data),
    n_cells_zero      = sum(gap_zero),
    pct_coverage      = round(100 * mean(has_data), 2),
    total_occurrences = sum(occurrences),
    mean_per_cell     = round(mean(occurrences), 2),
    median_per_cell   = median(occurrences)
  ), by = grid]
}

# ============================================================================
# Load Data
# ============================================================================

cli_h2("Loading Cell Summaries")

cell10 <- read_cell_summary("cell_summary_10km.csv")
cell50 <- read_cell_summary("cell_summary_50km.csv")

cli_alert_success(
  "10km: {scales::comma(nrow(cell10))} rows"
)
cli_alert_success(
  "50km: {scales::comma(nrow(cell50))} rows"
)

# ============================================================================
# Build Cell Universes
# ============================================================================

cli_h2("Building Cell Universes")

grid10_path <- here(p_data_proc, "grids_10km.gpkg")
grid50_path <- here(p_data_proc, "grids_50km.gpkg")

country_name <- cfg_get("country.name", "study area")

cli_alert_info("Getting all 10km cell codes...")
codes10 <- get_all_cellcodes(grid10_path)
cli_alert_success(
  "{scales::comma(length(codes10))} cells in 10km grid"
)

cli_alert_info(
  "Getting {country_name}-domain 50km cell codes..."
)
codes50_country <- filter_coarse_grid_to_country()
cli_alert_success(
  "{scales::comma(length(codes50_country))} cells in 50km grid ({country_name})"
)

# Filter 50km data to country domain
cell50 <- as.data.table(cell50)
cell50 <- cell50[eeacellcode %in% codes50_country]
cli_alert_info(
  "Filtered 50km to {country_name}: {scales::comma(nrow(cell50))} rows"
)

# ============================================================================
# Compute Spatial Gaps
# ============================================================================

cli_h2("Computing Spatial Gaps")

cli_alert_info("Processing 10km grid...")
gaps10 <- compute_spatial_gaps(cell10, codes10)

cli_alert_info("Processing 50km grid...")
gaps50 <- compute_spatial_gaps(cell50, codes50_country)

cli_alert_success("Gap analysis complete")

# ============================================================================
# Create Summaries
# ============================================================================

cli_h2("Creating Summaries")

basis_summary_10  <- create_basis_summary(gaps10$gaps)
basis_summary_50  <- create_basis_summary(gaps50$gaps)
basis_summary_all <- rbindlist(
  list(basis_summary_10, basis_summary_50)
)

grid_summary_10  <- create_grid_summary(gaps10$gaps)
grid_summary_50  <- create_grid_summary(gaps50$gaps)
grid_summary_all <- rbindlist(
  list(grid_summary_10, grid_summary_50)
)

# ============================================================================
# Write Outputs
# ============================================================================

cli_h2("Writing Outputs")

fwrite(gaps10$gaps, here(p_gaps, "spatial_gaps_10km.csv"))
cli_alert_success(
  "spatial_gaps_10km.csv: {scales::comma(nrow(gaps10$gaps))} rows"
)

fwrite(gaps50$gaps, here(p_gaps, "spatial_gaps_50km.csv"))
cli_alert_success(
  "spatial_gaps_50km.csv: {scales::comma(nrow(gaps50$gaps))} rows"
)

thresholds_all <- rbindlist(list(
  gaps10$thresholds[, grid_size := "10km"],
  gaps50$thresholds[, grid_size := "50km"]
))
fwrite(thresholds_all, here(p_gaps, "spatial_thresholds_by_basis.csv"))
cli_alert_success("spatial_thresholds_by_basis.csv")

fwrite(basis_summary_all, here(p_gaps, "spatial_summary_by_basis.csv"))
cli_alert_success("spatial_summary_by_basis.csv")

fwrite(grid_summary_all, here(p_gaps, "spatial_summary_by_grid.csv"))
cli_alert_success("spatial_summary_by_grid.csv")

# Zero-coverage cells
zero_cells_10 <- gaps10$gaps[
  gap_zero == TRUE & basisofrecord == "all", .(grid, eeacellcode)
]
zero_cells_50 <- gaps50$gaps[
  gap_zero == TRUE & basisofrecord == "all", .(grid, eeacellcode)
]
zero_cells_all <- rbindlist(list(zero_cells_10, zero_cells_50))
fwrite(zero_cells_all, here(p_gaps, "spatial_zero_coverage_cells.csv"))
cli_alert_success(
  "spatial_zero_coverage_cells.csv: {scales::comma(nrow(zero_cells_all))} cells"
)

# Low-coverage cells (bottom 10%)
low_cells_10 <- gaps10$gaps[
  gap_low_q10 == TRUE & basisofrecord == "all",
  .(grid, eeacellcode, occurrences, q10)
]
low_cells_50 <- gaps50$gaps[
  gap_low_q10 == TRUE & basisofrecord == "all",
  .(grid, eeacellcode, occurrences, q10)
]
low_cells_all <- rbindlist(list(low_cells_10, low_cells_50))
fwrite(low_cells_all, here(p_gaps, "spatial_low_coverage_cells_q10.csv"))
cli_alert_success(
  "spatial_low_coverage_cells_q10.csv: {scales::comma(nrow(low_cells_all))} cells"
)

# ============================================================================
# Summary
# ============================================================================

cli_h1("Summary (Script 07)")

summary_table <- data.table(
  Metric = c(
    "Total cells analyzed", "Cells with data",
    "Coverage %", "Zero-coverage cells"
  ),
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
cli_alert_info("Output: {.path {p_gaps}}")
cli_alert_info("Next: source('scripts/08_temporal_gaps.R')")
