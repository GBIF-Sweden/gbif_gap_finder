# scripts/08_temporal_gaps.R
# ==============================================================================
# Temporal Gap Analysis
# ==============================================================================
# This script identifies temporal gaps in GBIF occurrence data:
#
# INPUTS:
#   - data/{CC}/proc/derived/time_summary_*.csv (from 06a)
#   - data/{CC}/proc/derived/family_time_summary_*.csv (from 06a)
#   - data/{CC}/proc/cubes/*.parquet (for cell-level recency)
#
# OUTPUTS (in data/{CC}/proc/gaps/):
#   National trends:
#     - temporal_overview_year_*.csv      Annual totals
#     - temporal_overview_month_*.csv     Monthly totals (seasonal patterns)
#     - temporal_year_by_basis_*.csv      Annual totals by basis of record
#     - temporal_month_by_basis_*.csv     Monthly totals by basis of record
#     - temporal_year_month_*.csv         Year × month matrix (for heatmaps)
#     - temporal_decade_summary_*.csv     Decadal summaries
#
#   Completeness:
#     - temporal_year_completeness_*.csv  Months per year coverage
#     - temporal_month_completeness_*.csv Years per month coverage
#
#   Gaps:
#     - temporal_gap_years_detail.csv     All year × basis gaps
#     - temporal_gap_years_summary.csv    Summary of gap years
#
#   Cell-level:
#     - cell_recency_*.csv                Last observation per cell
#     - temporal_sampling_frequency_summary.csv
#
#   Taxonomic × temporal:
#     - temporal_year_by_family_*.csv     Family trends over time
#
# GAP DEFINITIONS:
#   - Gap year: No observations in a year within the overall range
#   - Stale cell: No observations in last 12 months or 5 years
#   - Incomplete year: <12 months with data

# Dependencies: scripts/00_setup.R, data.table, arrow, lubridate

source(here::here("scripts", "00_setup.R"))

# Script-specific package
library(arrow)

# ===========================================================================
# CONFIGURATION
# ===========================================================================

# p_derived, p_cubes, p_gaps are defined in R/globals.R
# Directories created by ensure_dirs() in 00_setup.R

# Staleness thresholds from config (or defaults)
STALE_12M <- cfg_get("parameters.temporal.stale_months_12", 12)
STALE_60M <- cfg_get("parameters.temporal.stale_months_60", 60)

cli_h1("Temporal Gap Analysis (Script 08)")
cli_alert_info("Staleness thresholds: {STALE_12M} months, {STALE_60M} months")

# ===========================================================================
# HELPER FUNCTIONS
# ===========================================================================

# parse_yearmonth, safe_sum, read_cube, read_derived_summary are in R/globals.R

#' Read time summary (wrapper around read_derived_summary)
read_time_summary <- function(filename) {
  read_derived_summary(filename,
    required_cols = c("basisofrecord", "yearmonth", "occurrences"))
}

#' Enrich temporal data with derived fields
enrich_temporal_data <- function(dt) {
  setDT(dt)
  
  dt[, ym := parse_yearmonth(yearmonth)]
  dt <- dt[!is.na(ym)]
  
  dt[, year := year(ym)]
  dt[, month := month(ym)]
  dt[, quarter := quarter(ym)]
  dt[, decade := floor(year / 10) * 10]
  
  dt
}

# ===========================================================================
# LOAD DATA
# ===========================================================================

cli_h2("Loading Temporal Summaries")

time10 <- read_time_summary("time_summary_10km.csv")
time50 <- read_time_summary("time_summary_50km.csv")

cli_alert_success("Loaded 10km: {scales::comma(nrow(time10))} rows")
cli_alert_success("Loaded 50km: {scales::comma(nrow(time50))} rows")

# Enrich with temporal fields
time10 <- enrich_temporal_data(time10)
time50 <- enrich_temporal_data(time50)

cli_alert_info("Year range 10km: {min(time10$year)}-{max(time10$year)}")
cli_alert_info("Year range 50km: {min(time50$year)}-{max(time50$year)}")

# ===========================================================================
# NATIONAL-LEVEL SUMMARIES
# ===========================================================================

cli_h2("Creating National-Level Summaries")

create_national_summaries <- function(time_data, grid_label) {
  
  # By year (all basis)
  year_all <- time_data[basisofrecord == "all", .(
    total_occurrences = safe_sum(occurrences),
    n_months = uniqueN(ym)
  ), by = .(grid, year)]
  
  # By year × basis
  year_by_basis <- time_data[, .(
    total_occurrences = safe_sum(occurrences),
    n_months = uniqueN(ym)
  ), by = .(grid, year, basisofrecord)]
  
  # By month (all basis) - seasonal patterns
  month_all <- time_data[basisofrecord == "all", .(
    total_occurrences = safe_sum(occurrences),
    n_years = uniqueN(year)
  ), by = .(grid, month)]
  
  # By month × basis
  month_by_basis <- time_data[, .(
    total_occurrences = safe_sum(occurrences),
    n_years = uniqueN(year)
  ), by = .(grid, month, basisofrecord)]
  
  # By year × month (for heatmaps)
  year_month <- time_data[basisofrecord == "all", .(
    total_occurrences = safe_sum(occurrences)
  ), by = .(grid, year, month)]
  
  # By decade
  decade_summary <- time_data[basisofrecord == "all", .(
    total_occurrences = safe_sum(occurrences),
    n_years = uniqueN(year),
    n_months = uniqueN(ym)
  ), by = .(grid, decade)]
  
  list(
    year_all = year_all,
    year_by_basis = year_by_basis,
    month_all = month_all,
    month_by_basis = month_by_basis,
    year_month = year_month,
    decade = decade_summary
  )
}

cli_alert_info("Processing 10km summaries...")
summaries_10 <- create_national_summaries(time10, "grid10km")

cli_alert_info("Processing 50km summaries...")
summaries_50 <- create_national_summaries(time50, "grid50km")

cli_alert_success("National summaries complete")

# ===========================================================================
# TEMPORAL COMPLETENESS
# ===========================================================================

cli_h2("Analyzing Temporal Completeness")

compute_completeness <- function(time_data) {
  
  # Year completeness by basis
  year_completeness <- time_data[, .(
    n_months_observed = uniqueN(month),
    total_occurrences = safe_sum(occurrences),
    complete_year = (uniqueN(month) == 12)
  ), by = .(grid, year, basisofrecord)]
  
  # Month completeness (how many years has each month been sampled?)
  month_completeness <- time_data[, .(
    n_years_observed = uniqueN(year),
    total_occurrences = safe_sum(occurrences)
  ), by = .(grid, month, basisofrecord)]
  
  # Seasonal completeness (all 4 quarters represented?)
  quarter_completeness <- time_data[, .(
    n_quarters_observed = uniqueN(quarter),
    complete_seasons = (uniqueN(quarter) == 4)
  ), by = .(grid, year, basisofrecord)]
  
  list(
    year = year_completeness,
    month = month_completeness,
    quarter = quarter_completeness
  )
}

completeness_10 <- compute_completeness(time10)
completeness_50 <- compute_completeness(time50)

# ===========================================================================
# GAP YEARS
# ===========================================================================

cli_h2("Identifying Temporal Gaps")

identify_gap_years <- function(time_data) {
  
  # Get full year range
  year_range <- time_data[basisofrecord == "all", .(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  ), by = grid]
  
  # For each grid, create all years in range
  gap_years <- rbindlist(lapply(1:nrow(year_range), function(i) {
    grid_val <- year_range$grid[i]
    all_years <- seq(year_range$min_year[i], year_range$max_year[i])
    
    # Get observed years per basis
    observed <- time_data[grid == grid_val, .(
      observed = TRUE
    ), by = .(year, basisofrecord)]
    
    # Create all combinations
    all_combos <- CJ(
      grid = grid_val,
      year = all_years,
      basisofrecord = unique(time_data$basisofrecord)
    )
    
    # Mark gaps
    result <- merge(all_combos, observed, 
                   by = c("year", "basisofrecord"), 
                   all.x = TRUE)
    result[is.na(observed), observed := FALSE]
    result[, is_gap := !observed]
    
    result[, .(grid, year, basisofrecord, is_gap)]
  }))
  
  gap_years
}

gap_years_10 <- identify_gap_years(time10)
gap_years_50 <- identify_gap_years(time50)

# ===========================================================================
# CELL-LEVEL RECENCY
# ===========================================================================

cli_h2("Computing Cell-Level Recency")

compute_cell_recency <- function(grid_label, cubes_dir) {
  
  cli_alert_info("Processing {grid_label} cubes...")
  
  # Find parquet file for this grid
  grid_suffix <- str_extract(grid_label, "\\d+km")
  parquet_file <- list.files(cubes_dir, pattern = glue(".*{grid_suffix}.*\\.parquet$"), full.names = TRUE)
  
  if (length(parquet_file) == 0) {
    cli_alert_warning("No parquet file found for {grid_label} in {.path {cubes_dir}}")
    return(NULL)
  }
  
  parquet_file <- parquet_file[1]
  cli_alert_info("Reading: {basename(parquet_file)}")
  
  cols <- c("basisofrecord", "eeacellcode", "year", "month", "occurrences")
  dt <- read_cube(parquet_file, cols = cols, recode_taxonomy = FALSE)
  
  dt <- dt[as.numeric(occurrences) > 0]
  dt[, ym := parse_yearmonth(yearmonth)]
  dt <- dt[!is.na(ym)]
  
  if (nrow(dt) == 0) {
    cli_alert_warning("No recency data for {grid_label}")
    return(NULL)
  }
  
  cli_alert_info("{scales::comma(nrow(dt))} rows with valid dates")
  
  # Per basis
  tmp_basis <- dt[, .(
    last_ym = max(ym, na.rm = TRUE),
    first_ym = min(ym, na.rm = TRUE),
    n_observations = .N,
    total_occurrences = safe_sum(occurrences),
    n_unique_months = uniqueN(ym)
  ), by = .(basisofrecord, eeacellcode)]
  tmp_basis[, grid := grid_label]
  
  # All basis combined
  tmp_all <- dt[, .(
    last_ym = max(ym, na.rm = TRUE),
    first_ym = min(ym, na.rm = TRUE),
    n_observations = .N,
    total_occurrences = safe_sum(occurrences),
    n_unique_months = uniqueN(ym)
  ), by = .(eeacellcode)]
  tmp_all[, basisofrecord := "all"]
  tmp_all[, grid := grid_label]
  
  out <- rbindlist(list(tmp_basis, tmp_all), use.names = TRUE, fill = TRUE)
  
  rm(dt, tmp_basis, tmp_all); gc()
  
  # Calculate derived metrics
  today <- Sys.Date()
  
  out[, staleness_months := as.integer(
    interval(last_ym, today) / months(1)
  )]
  
  out[, observation_span_months := as.integer(
    interval(first_ym, last_ym) / months(1)
  )]
  
  out[, gap_stale_12m := (staleness_months > STALE_12M)]
  out[, gap_stale_5y := (staleness_months > STALE_60M)]
  
  out[, sampling_frequency := n_observations / pmax(observation_span_months, 1)]
  
  out
}

cli_alert_info("Computing recency for 10km...")
recency_10 <- compute_cell_recency("grid10km", p_cubes)

cli_alert_info("Computing recency for 50km...")
recency_50 <- compute_cell_recency("grid50km", p_cubes)

cli_alert_success("Cell recency complete")

# ===========================================================================
# SAMPLING FREQUENCY SUMMARY
# ===========================================================================

cli_h2("Analyzing Sampling Frequency")

create_frequency_summary <- function(recency_data) {
  if (is.null(recency_data) || nrow(recency_data) == 0) {
    return(NULL)
  }
  
  recency_data[, .(
    n_cells = .N,
    mean_observations = as.numeric(round(mean(n_observations, na.rm = TRUE), 1)),
    median_observations = as.numeric(median(n_observations, na.rm = TRUE)),
    mean_unique_months = as.numeric(round(mean(n_unique_months, na.rm = TRUE), 1)),
    median_span_months = as.numeric(median(observation_span_months, na.rm = TRUE)),
    mean_frequency = as.numeric(round(mean(sampling_frequency, na.rm = TRUE), 3))
  ), by = .(grid, basisofrecord)]
}

frequency_summary_10 <- create_frequency_summary(recency_10)
frequency_summary_50 <- create_frequency_summary(recency_50)
frequency_summary_all <- rbindlist(list(frequency_summary_10, frequency_summary_50), fill = TRUE)

# ===========================================================================
# TAXONOMIC × TEMPORAL (Family trends)
# ===========================================================================

cli_h2("Creating Taxonomic x Temporal Summaries")

family_year_10 <- NULL
family_year_50 <- NULL

family_10_path <- here(p_derived, "family_time_summary_10km.csv")
family_50_path <- here(p_derived, "family_time_summary_50km.csv")

if (file.exists(family_10_path)) {
  family_data <- fread(family_10_path)
  family_data <- enrich_temporal_data(family_data)
  
  family_year_10 <- family_data[basisofrecord == "all", .(
    total_occurrences = safe_sum(occurrences),
    n_families = uniqueN(family)
  ), by = .(grid, year)]
  
  cli_alert_success("Created family x year summary (10km)")
} else {
  cli_alert_info("Family data not available for 10km")
}

if (file.exists(family_50_path)) {
  family_data <- fread(family_50_path)
  family_data <- enrich_temporal_data(family_data)
  
  family_year_50 <- family_data[basisofrecord == "all", .(
    total_occurrences = safe_sum(occurrences),
    n_families = uniqueN(family)
  ), by = .(grid, year)]
  
  cli_alert_success("Created family x year summary (50km)")
} else {
  cli_alert_info("Family data not available for 50km")
}

# ===========================================================================
# WRITE OUTPUTS
# ===========================================================================

cli_h2("Writing Temporal Gap Outputs")

# National trends
fwrite(summaries_10$year_all, here(p_gaps, "temporal_overview_year_10km.csv"))
fwrite(summaries_50$year_all, here(p_gaps, "temporal_overview_year_50km.csv"))
cli_alert_success("temporal_overview_year_*.csv")

fwrite(summaries_10$month_all, here(p_gaps, "temporal_overview_month_10km.csv"))
fwrite(summaries_50$month_all, here(p_gaps, "temporal_overview_month_50km.csv"))
cli_alert_success("temporal_overview_month_*.csv")

# By basis of record
fwrite(summaries_10$year_by_basis, here(p_gaps, "temporal_year_by_basis_10km.csv"))
fwrite(summaries_50$year_by_basis, here(p_gaps, "temporal_year_by_basis_50km.csv"))
cli_alert_success("temporal_year_by_basis_*.csv")

fwrite(summaries_10$month_by_basis, here(p_gaps, "temporal_month_by_basis_10km.csv"))
fwrite(summaries_50$month_by_basis, here(p_gaps, "temporal_month_by_basis_50km.csv"))
cli_alert_success("temporal_month_by_basis_*.csv")

# Year × month heatmap data
fwrite(summaries_10$year_month, here(p_gaps, "temporal_year_month_10km.csv"))
fwrite(summaries_50$year_month, here(p_gaps, "temporal_year_month_50km.csv"))
cli_alert_success("temporal_year_month_*.csv")

# Decade summaries
fwrite(summaries_10$decade, here(p_gaps, "temporal_decade_summary_10km.csv"))
fwrite(summaries_50$decade, here(p_gaps, "temporal_decade_summary_50km.csv"))
cli_alert_success("temporal_decade_summary_*.csv")

# Completeness
fwrite(completeness_10$year, here(p_gaps, "temporal_year_completeness_10km.csv"))
fwrite(completeness_50$year, here(p_gaps, "temporal_year_completeness_50km.csv"))
cli_alert_success("temporal_year_completeness_*.csv")

fwrite(completeness_10$month, here(p_gaps, "temporal_month_completeness_10km.csv"))
fwrite(completeness_50$month, here(p_gaps, "temporal_month_completeness_50km.csv"))
cli_alert_success("temporal_month_completeness_*.csv")

# Gap years
gap_years_all <- rbindlist(list(gap_years_10, gap_years_50))
gap_summary <- gap_years_all[, .(
  n_gaps = sum(is_gap),
  pct_gaps = round(100 * mean(is_gap), 2)
), by = .(grid, basisofrecord)]

fwrite(gap_years_all, here(p_gaps, "temporal_gap_years_detail.csv"))
fwrite(gap_summary, here(p_gaps, "temporal_gap_years_summary.csv"))
cli_alert_success("temporal_gap_years_*.csv")

# Cell recency
if (!is.null(recency_10)) {
  fwrite(recency_10, here(p_gaps, "cell_recency_10km.csv"))
  cli_alert_success("cell_recency_10km.csv: {scales::comma(nrow(recency_10))} rows")
}

if (!is.null(recency_50)) {
  fwrite(recency_50, here(p_gaps, "cell_recency_50km.csv"))
  cli_alert_success("cell_recency_50km.csv: {scales::comma(nrow(recency_50))} rows")
}

# Sampling frequency
if (!is.null(frequency_summary_all) && nrow(frequency_summary_all) > 0) {
  fwrite(frequency_summary_all, here(p_gaps, "temporal_sampling_frequency_summary.csv"))
  cli_alert_success("temporal_sampling_frequency_summary.csv")
}

# Taxonomic × temporal
if (!is.null(family_year_10)) {
  fwrite(family_year_10, here(p_gaps, "temporal_year_by_family_10km.csv"))
  cli_alert_success("temporal_year_by_family_10km.csv")
}

if (!is.null(family_year_50)) {
  fwrite(family_year_50, here(p_gaps, "temporal_year_by_family_50km.csv"))
  cli_alert_success("temporal_year_by_family_50km.csv")
}

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 08)")

summary_table <- data.table(
  Metric = c("Year range", "Total records", "Cells with recency", "Stale cells (5y)"),
  `10km` = c(
    glue("{min(time10$year)}-{max(time10$year)}"),
    scales::comma(sum(time10[basisofrecord == "all"]$occurrences)),
    if (!is.null(recency_10)) scales::comma(nrow(recency_10[basisofrecord == "all"])) else "N/A",
    if (!is.null(recency_10)) scales::comma(sum(recency_10[basisofrecord == "all"]$gap_stale_5y, na.rm = TRUE)) else "N/A"
  ),
  `50km` = c(
    glue("{min(time50$year)}-{max(time50$year)}"),
    scales::comma(sum(time50[basisofrecord == "all"]$occurrences)),
    if (!is.null(recency_50)) scales::comma(nrow(recency_50[basisofrecord == "all"])) else "N/A",
    if (!is.null(recency_50)) scales::comma(sum(recency_50[basisofrecord == "all"]$gap_stale_5y, na.rm = TRUE)) else "N/A"
  )
)

print(summary_table)

cli_alert_success("Temporal gap analysis complete!")
cli_alert_info("Output location: {.path {p_gaps}}")

# Count output files
n_outputs <- length(list.files(p_gaps, pattern = "^temporal_|^cell_recency"))
cli_alert_info("Created {n_outputs} temporal gap files")
cli_alert_info("Next: source('scripts/09a_reconcile_taxonomy.R')")
