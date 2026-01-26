# scripts/08_define_temporal_gaps.R
# ==============================================================================
# Temporal Gap Analysis - Comprehensive
# ==============================================================================
# This script creates detailed temporal gap metrics across multiple dimensions:
# - National trends (year, month)
# - Trends by basis of record
# - Trends by taxonomic rank (family, order)
# - Cell-level recency and sampling frequency
# - Temporal completeness and gaps
#
# Outputs organized by type for flexible plotting

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(data.table)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------
p_derived <- here(p_data_proc, "derived")
p_cubes <- here(p_data_proc, "cubes")
p_gaps <- here(p_data_proc, "gaps")

dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# Staleness thresholds (months)
STALE_12M <- 12
STALE_60M <- 60  # 5 years

# Helper functions --------------------------------------------------------

#' Parse yearmonth to Date
parse_yearmonth <- function(x) {
  x_chr <- str_trim(as.character(x))
  valid <- str_detect(x_chr, "^[0-9]{4}-[0-9]{2}$")
  
  out <- rep(as.Date(NA), length(x_chr))
  out[valid] <- as.Date(paste0(x_chr[valid], "-01"))
  out
}

#' Safe sum
safe_sum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

#' Read time summary safely
read_time_summary <- function(filename) {
  path <- here(p_derived, filename)
  
  if (!file.exists(path)) {
    cli_abort("Time summary not found: {.path {path}}")
  }
  
  dt <- fread(path)
  
  required_cols <- c("grid", "basisofrecord", "yearmonth", "occurrences")
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0) {
    cli_abort(c(
      "Missing required columns in {.path {filename}}",
      "x" = "Missing: {paste(missing_cols, collapse = ', ')}"
    ))
  }
  
  dt
}

#' Read fst columns
read_fst_cols <- function(path, cols) {
  meta <- fst::metadata_fst(path)
  available <- meta$columnNames
  cols_to_read <- intersect(cols, available)
  
  if (!("occurrences" %in% available)) {
    cli_abort("Column 'occurrences' missing in: {.path {basename(path)}}")
  }
  
  df <- fst::read_fst(path, columns = cols_to_read)
  setDT(df)
  df
}

# Parse and enrich temporal data -----------------------------------------

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

# Load data ---------------------------------------------------------------
cli_h2("Loading Temporal Summaries")

time10 <- read_time_summary("time_summary_10km.csv")
time50 <- read_time_summary("time_summary_50km.csv")

cli_alert_success("Loaded 10km: {scales::comma(nrow(time10))} rows")
cli_alert_success("Loaded 50km: {scales::comma(nrow(time50))} rows")

# Enrich with temporal fields
time10 <- enrich_temporal_data(time10)
time50 <- enrich_temporal_data(time50)

# National-level summaries ------------------------------------------------
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

# Temporal completeness ---------------------------------------------------
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

# Gap years (years with zero observations) --------------------------------
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

# Cell-level recency ------------------------------------------------------
cli_h2("Computing Cell-Level Recency")

compute_cell_recency <- function(grid_label, cubes_dir) {
  
  cli_alert_info("Processing {grid_label} cubes...")
  
  cube_files <- list.files(cubes_dir, pattern = "\\.fst$", full.names = TRUE)
  
  if (length(cube_files) == 0) {
    cli_abort("No cube files found in: {.path {cubes_dir}}")
  }
  
  cols <- c("grid", "basisofrecord", "eeacellcode", "yearmonth", "occurrences")
  parts <- list()
  
  cli_progress_bar("Reading cubes", total = length(cube_files), clear = FALSE)
  
  for (f in cube_files) {
    cli_progress_update()
    
    dt <- read_fst_cols(f, cols)
    
    if (!("grid" %in% names(dt))) {
      rm(dt)
      next
    }
    
    dt <- dt[grid == grid_label]
    
    if (nrow(dt) == 0) {
      rm(dt)
      next
    }
    
    required <- c("eeacellcode", "yearmonth", "occurrences", "basisofrecord")
    if (!all(required %in% names(dt))) {
      rm(dt)
      next
    }
    
    dt <- dt[as.numeric(occurrences) > 0]
    
    if (nrow(dt) == 0) {
      rm(dt)
      next
    }
    
    dt[, ym := parse_yearmonth(yearmonth)]
    dt <- dt[!is.na(ym)]
    
    if (nrow(dt) == 0) {
      rm(dt)
      next
    }
    
    # Per basis
    tmp_basis <- dt[, .(
      last_ym = max(ym, na.rm = TRUE),
      first_ym = min(ym, na.rm = TRUE),
      n_observations = .N,
      total_occurrences = safe_sum(occurrences),
      n_unique_months = uniqueN(ym)
    ), by = .(grid, basisofrecord, eeacellcode)]
    
    # All basis combined
    tmp_all <- dt[, .(
      last_ym = max(ym, na.rm = TRUE),
      first_ym = min(ym, na.rm = TRUE),
      n_observations = .N,
      total_occurrences = safe_sum(occurrences),
      n_unique_months = uniqueN(ym)
    ), by = .(grid, eeacellcode)]
    tmp_all[, basisofrecord := "all"]
    
    parts[[length(parts) + 1]] <- rbindlist(
      list(tmp_basis, tmp_all), 
      use.names = TRUE, 
      fill = TRUE
    )
    
    rm(dt)
    invisible(gc())
  }
  
  cli_progress_done()
  
  if (length(parts) == 0) {
    cli_abort("No recency data produced for {grid_label}")
  }
  
  # Combine all parts
  out <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  
  # Aggregate across files
  out <- out[, .(
    last_ym = max(last_ym, na.rm = TRUE),
    first_ym = min(first_ym, na.rm = TRUE),
    n_observations = sum(n_observations),
    total_occurrences = safe_sum(total_occurrences),
    n_unique_months = sum(n_unique_months)
  ), by = .(grid, basisofrecord, eeacellcode)]
  
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

# Sampling frequency summary ----------------------------------------------
cli_h2("Analyzing Sampling Frequency")

create_frequency_summary <- function(recency_data) {
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
frequency_summary_all <- rbindlist(list(frequency_summary_10, frequency_summary_50))

# Load and process taxonomic time series ----------------------------------
cli_h2("Creating Taxonomic × Temporal Summaries")

# Family × year (if available)
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
  
  cli_alert_success("Created family × year summary (10km)")
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
  
  cli_alert_success("Created family × year summary (50km)")
} else {
  cli_alert_info("Family data not available for 50km")
}

# Write outputs -----------------------------------------------------------
cli_h2("Writing Temporal Gap Outputs")

# National trends
fwrite(summaries_10$year_all, here(p_gaps, "temporal_overview_year_10km.csv"))
fwrite(summaries_50$year_all, here(p_gaps, "temporal_overview_year_50km.csv"))
cli_alert_success("temporal_overview_year_*.csv")

fwrite(summaries_10$month_all, here(p_gaps, "temporal_overview_month_10km.csv"))
fwrite(summaries_50$month_all, here(p_gaps, "temporal_overview_month_50km.csv"))
cli_alert_success("temporal_overview_month_*.csv")

# By basis of record
year_by_basis_all <- rbindlist(list(summaries_10$year_by_basis, summaries_50$year_by_basis))
fwrite(summaries_10$year_by_basis, here(p_gaps, "temporal_year_by_basis_10km.csv"))
fwrite(summaries_50$year_by_basis, here(p_gaps, "temporal_year_by_basis_50km.csv"))
cli_alert_success("temporal_year_by_basis_*.csv")

month_by_basis_all <- rbindlist(list(summaries_10$month_by_basis, summaries_50$month_by_basis))
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
fwrite(recency_10, here(p_gaps, "cell_recency_10km.csv"))
fwrite(recency_50, here(p_gaps, "cell_recency_50km.csv"))
cli_alert_success("cell_recency_*.csv")

# Sampling frequency
fwrite(frequency_summary_all, here(p_gaps, "temporal_sampling_frequency_summary.csv"))
cli_alert_success("temporal_sampling_frequency_summary.csv")

# Taxonomic × temporal (if available)
if (!is.null(family_year_10)) {
  fwrite(family_year_10, here(p_gaps, "temporal_year_by_family_10km.csv"))
  cli_alert_success("temporal_year_by_family_10km.csv")
}

if (!is.null(family_year_50)) {
  fwrite(family_year_50, here(p_gaps, "temporal_year_by_family_50km.csv"))
  cli_alert_success("temporal_year_by_family_50km.csv")
}

# Summary statistics ------------------------------------------------------
cli_h2("Summary Statistics")

summary_table <- tibble::tribble(
  ~metric, ~value_10km, ~value_50km,
  "Year range", glue("{min(time10$year)}-{max(time10$year)}"),
            glue("{min(time50$year)}-{max(time50$year)}"),
  "Total observations", scales::comma(sum(time10[basisofrecord == "all"]$occurrences)),
                        scales::comma(sum(time50[basisofrecord == "all"]$occurrences)),
  "Cells with recency data", scales::comma(nrow(recency_10[basisofrecord == "all"])),
                             scales::comma(nrow(recency_50[basisofrecord == "all"])),
  "Basis of record types", as.character(length(unique(time10$basisofrecord))),
                          as.character(length(unique(time50$basisofrecord)))
)

print(summary_table)

cli_alert_success("Temporal gap analysis complete!")
cli_alert_info("Output location: {.path {p_gaps}}")
cli_alert_info("Created {21 + (if(!is.null(family_year_10)) 1 else 0) + (if(!is.null(family_year_50)) 1 else 0)} temporal gap files")
