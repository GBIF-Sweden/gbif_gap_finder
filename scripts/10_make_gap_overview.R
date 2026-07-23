# scripts/10_make_gap_overview.R
# ==============================================================================
# Integrated Gap Overview - Multi-Dimensional Summaries
# ==============================================================================
# Creates comprehensive, analysis-ready integrated tables combining:
# - Spatial gaps (coverage, intensity)
# - Temporal gaps (trends, seasonality, recency, completeness)
# - Taxonomic gaps (coverage by rank, threat status, families, orders)
# - Species-level summaries (richness, endemism, threatened species)
# - Priority lists for conservation action
#
# INPUTS (from scripts 07-09):
#   Spatial:  spatial_gaps_*.csv, spatial_summary_*.csv, spatial_*_cells.csv
#   Temporal: temporal_overview_*.csv, temporal_*_completeness_*.csv,
#             temporal_gap_years_*.csv, cell_recency_*.csv
#   Taxonomic: taxonomic_coverage_*.csv, taxonomic_gaps_by_*.csv,
#              taxonomic_*_taxa.csv, taxonomic_spatial_coverage.csv
#
# OUTPUTS:
#   output/tables/               - Standard summary tables
#   output/tables/integrated/    - Multi-dimensional joined tables

source(here::here("scripts", "00_setup.R"))

# ===========================================================================
# CONFIGURATION
# ===========================================================================

cli_h1("Integrated Gap Overview (Script 10)")

# p_gaps, p_derived, p_tables, p_integrated are defined in R/globals.R
# Directories created by ensure_dirs() in 00_setup.R

# ===========================================================================
# HELPER FUNCTIONS
# ===========================================================================

# safe_read() and parse_year() are defined in R/globals.R

#' Safely read gap file
safe_read_gap <- function(filename) safe_read(here(p_gaps, filename))

#' Safely read derived file
safe_read_derived <- function(filename, subdir = NULL) {
  if (!is.null(subdir)) safe_read(here(p_derived, subdir, filename))
  else safe_read(here(p_derived, filename))
}

#' Write to integrated folder
write_integrated <- function(dt, filename) {
  if (is.null(dt) || nrow(dt) == 0) {
    cli_alert_warning("{filename}: No data")
    return(invisible(NULL))
  }
  path <- here(p_integrated, filename)
  fwrite(dt, path)
  cli_alert_success("{filename}: {scales::comma(nrow(dt))} rows")
}

#' Write to tables folder
write_table <- function(dt, filename) {
  if (is.null(dt) || nrow(dt) == 0) {
    cli_alert_warning("{filename}: No data")
    return(invisible(NULL))
  }
  path <- here(p_tables, filename)
  fwrite(dt, path)
  cli_alert_success("{filename}: {scales::comma(nrow(dt))} rows")
}

# ===========================================================================
# LOAD ALL GAP OUTPUTS
# ===========================================================================

cli_h2("Loading Gap Analysis Outputs")

# --- Spatial ---
spatial_10 <- safe_read_gap("spatial_gaps_10km.csv")
spatial_50 <- safe_read_gap("spatial_gaps_50km.csv")
spatial_summary_grid <- safe_read_gap("spatial_summary_by_grid.csv")
spatial_thresholds <- safe_read_gap("spatial_thresholds_by_basis.csv")
spatial_zero_cells <- safe_read_gap("spatial_zero_coverage_cells.csv")
spatial_low_cells <- safe_read_gap("spatial_low_coverage_cells_q10.csv")
# 07 writes these two files for BOTH grid resolutions (10km + 50km rbind'd).
# The dashboard fields and the integrated priority lists below feed the 10km
# app, so drop the 50km rows here — otherwise the zero/low counts mix
# resolutions and disagree with cells_10km_zero and the 10km app tabs. (The
# 50km empties remain in spatial_gaps_50km for the resolution comparison; they
# just must not leak into the 10km outputs.)
if (!is.null(spatial_zero_cells) && "grid" %in% names(spatial_zero_cells)) {
  spatial_zero_cells <- spatial_zero_cells[grid == "grid10km"]
}
if (!is.null(spatial_low_cells) && "grid" %in% names(spatial_low_cells)) {
  spatial_low_cells <- spatial_low_cells[grid == "grid10km"]
}

# --- Temporal ---
temporal_year_10 <- safe_read_gap("temporal_overview_year_10km.csv")
temporal_year_50 <- safe_read_gap("temporal_overview_year_50km.csv")
temporal_month_10 <- safe_read_gap("temporal_overview_month_10km.csv")
temporal_month_50 <- safe_read_gap("temporal_overview_month_50km.csv")
temporal_year_month_10 <- safe_read_gap("temporal_year_month_10km.csv")
temporal_year_month_50 <- safe_read_gap("temporal_year_month_50km.csv")
temporal_decade_10 <- safe_read_gap("temporal_decade_summary_10km.csv")
temporal_decade_50 <- safe_read_gap("temporal_decade_summary_50km.csv")
temporal_year_complete_10 <- safe_read_gap("temporal_year_completeness_10km.csv")
temporal_year_complete_50 <- safe_read_gap("temporal_year_completeness_50km.csv")
temporal_gap_years_summary <- safe_read_gap("temporal_gap_years_summary.csv")
temporal_sampling_freq <- safe_read_gap("temporal_sampling_frequency_summary.csv")
recency_10 <- safe_read_gap("cell_recency_10km.csv")
recency_50 <- safe_read_gap("cell_recency_50km.csv")
temporal_family_10 <- safe_read_gap("temporal_year_by_family_10km.csv")

# --- Taxonomic ---
tax_coverage_rank <- safe_read_gap("taxonomic_coverage_by_rank.csv")
tax_coverage_threat <- safe_read_gap("taxonomic_coverage_by_threat.csv")
tax_coverage_basis <- safe_read_gap("taxonomic_coverage_by_basis.csv")
tax_gap_summary <- safe_read_gap("taxonomic_gap_summary.csv")
tax_gaps_family <- safe_read_gap("taxonomic_gaps_by_family.csv")
tax_gaps_order <- safe_read_gap("taxonomic_gaps_by_order.csv")
tax_spatial <- safe_read_gap("taxonomic_spatial_coverage.csv")
tax_threatened_spatial <- safe_read_gap("taxonomic_threatened_spatial_coverage.csv")
tax_priority <- safe_read_gap("taxonomic_priority_taxa.csv")
tax_missing <- safe_read_gap("taxonomic_missing_taxa.csv")
tax_missing_threatened <- safe_read_gap("taxonomic_missing_threatened.csv")

cli_alert_success("Gap files loaded")

# --- Derived summaries (from 06a/06b) ---
cli_h2("Loading Derived Summaries")

order_cell_10 <- safe_read_derived("order_cell_summary_10km.csv")
order_time_10 <- safe_read_derived("order_time_summary_10km.csv")
family_time_10 <- safe_read_derived("family_time_summary_10km.csv")

cli_alert_success("Derived summaries loaded")

# ===========================================================================
# 1. MASTER DASHBOARD SUMMARY
# ===========================================================================

cli_h2("Creating Master Dashboard Summary")

# Calculate key metrics
# On failure, log which expression failed (and why) before falling back to the
# default, so malformed/missing upstream files surface as a named warning rather
# than a silent NA in the dashboard. Capture the expression text first, before
# the promise is forced.
calc_metric <- function(expr, default = NA) {
  expr_txt <- paste(deparse(substitute(expr)), collapse = " ")
  tryCatch(expr, error = function(e) {
    cli_alert_warning("calc_metric failed [{expr_txt}]: {conditionMessage(e)} \u2014 using default")
    default
  })
}

dashboard <- data.table(
  # --- Spatial metrics ---
  cells_10km_total = calc_metric(nrow(spatial_10[basisofrecord == "all"])),
  cells_10km_with_data = calc_metric(sum(spatial_10[basisofrecord == "all"]$has_data)),
  cells_10km_zero = calc_metric(sum(spatial_10[basisofrecord == "all"]$gap_zero)),
  cells_10km_pct_coverage = calc_metric(round(100 * mean(spatial_10[basisofrecord == "all"]$has_data), 1)),
  
  cells_50km_total = calc_metric(nrow(spatial_50[basisofrecord == "all"])),
  cells_50km_with_data = calc_metric(sum(spatial_50[basisofrecord == "all"]$has_data)),
  cells_50km_zero = calc_metric(sum(spatial_50[basisofrecord == "all"]$gap_zero)),
  cells_50km_pct_coverage = calc_metric(round(100 * mean(spatial_50[basisofrecord == "all"]$has_data), 1)),
  
  # --- Temporal metrics ---
  year_min = calc_metric(min(temporal_year_10$year, na.rm = TRUE)),
  year_max = calc_metric(max(temporal_year_10$year, na.rm = TRUE)),
  year_span = calc_metric(max(temporal_year_10$year) - min(temporal_year_10$year) + 1),
  total_occurrences = calc_metric(sum(temporal_year_10$total_occurrences, na.rm = TRUE)),
  
  median_staleness_months_10km = calc_metric(
    as.numeric(median(recency_10[basisofrecord == "all"]$staleness_months, na.rm = TRUE))
  ),
  pct_stale_1y_10km = calc_metric(
    round(100 * mean(recency_10[basisofrecord == "all"]$gap_stale_12m, na.rm = TRUE), 1)
  ),
  pct_stale_5y_10km = calc_metric(
    round(100 * mean(recency_10[basisofrecord == "all"]$gap_stale_5y, na.rm = TRUE), 1)
  ),
  
  # --- Taxonomic metrics ---
  taxa_in_reference = calc_metric(sum(tax_coverage_rank$n_ref_total, na.rm = TRUE)),
  taxa_in_gbif = calc_metric(sum(tax_coverage_rank$n_in_gbif, na.rm = TRUE)),
  taxa_missing = calc_metric(sum(tax_coverage_rank$n_missing, na.rm = TRUE)),
  taxa_pct_coverage = calc_metric(
    round(100 * sum(tax_coverage_rank$n_in_gbif) / sum(tax_coverage_rank$n_ref_total), 1)
  ),
  
  # --- Threatened species ---
  # tax_coverage_threat has ONE row per threat status, so nrow() here returned
  # the number of categories (<=4), not the number of threatened reference taxa.
  # Sum n_ref_total instead (mirrors threatened_in_gbif on the next line).
  threatened_in_reference = calc_metric(sum(tax_coverage_threat[threatStatus %in% c("CR", "EN", "VU", "NT")]$n_ref_total, na.rm = TRUE)),
  threatened_in_gbif = calc_metric(
    sum(tax_coverage_threat[threatStatus %in% c("CR", "EN", "VU", "NT")]$n_in_gbif, na.rm = TRUE)
  ),
  threatened_missing = calc_metric(nrow(tax_missing_threatened)),
  
  # --- Priority counts ---
  n_priority_taxa = calc_metric(nrow(tax_priority)),
  n_zero_coverage_cells = calc_metric(nrow(spatial_zero_cells)),
  n_low_coverage_cells = calc_metric(nrow(spatial_low_cells)),
  
  # --- Metadata ---
  analysis_date = as.character(Sys.Date()),
  n_basis_types = calc_metric(uniqueN(spatial_10$basisofrecord) - 1)  # exclude "all"
)

write_table(dashboard, "dashboard_summary.csv")

# Transposed version for display
dashboard_char <- dashboard[, lapply(.SD, as.character)]
dashboard_long <- melt(dashboard_char, measure.vars = names(dashboard_char), 
                       variable.name = "metric", value.name = "value")
write_table(dashboard_long, "dashboard_summary_long.csv")

# ===========================================================================
# 2. SPATIAL OVERVIEWS
# ===========================================================================

cli_h2("Creating Spatial Overviews")

# Combined spatial overview by grid and basis
if (!is.null(spatial_10) && !is.null(spatial_50)) {
  spatial_all <- rbindlist(list(spatial_10, spatial_50), fill = TRUE)
  
  spatial_overview <- spatial_all[, .(
    n_cells = .N,
    n_with_data = sum(has_data, na.rm = TRUE),
    n_zero = sum(gap_zero, na.rm = TRUE),
    pct_coverage = round(100 * mean(has_data, na.rm = TRUE), 2),
    pct_zero = round(100 * mean(gap_zero, na.rm = TRUE), 2),
    n_low_q05 = sum(gap_low_q05, na.rm = TRUE),
    n_low_q10 = sum(gap_low_q10, na.rm = TRUE),
    n_low_q25 = sum(gap_low_q25, na.rm = TRUE),
    total_occurrences = sum(as.numeric(occurrences), na.rm = TRUE),
    mean_occ_nonzero = round(mean(occurrences[occurrences > 0], na.rm = TRUE), 1),
    median_occ_nonzero = as.numeric(median(occurrences[occurrences > 0], na.rm = TRUE)),
    max_occurrences = max(occurrences, na.rm = TRUE)
  ), by = .(grid, basisofrecord)]
  
  setorder(spatial_overview, grid, basisofrecord)
  write_table(spatial_overview, "overview_spatial_by_basis.csv")
}

# Spatial summary by grid only (all basis)
if (!is.null(spatial_summary_grid)) {
  write_table(spatial_summary_grid, "overview_spatial_by_grid.csv")
}

# Quantile thresholds
if (!is.null(spatial_thresholds)) {
  write_table(spatial_thresholds, "overview_spatial_thresholds.csv")
}

# ===========================================================================
# 3. TEMPORAL OVERVIEWS
# ===========================================================================

cli_h2("Creating Temporal Overviews")

# Annual trends
if (!is.null(temporal_year_10) && !is.null(temporal_year_50)) {
  temporal_year_all <- rbindlist(list(temporal_year_10, temporal_year_50), fill = TRUE)
  write_table(temporal_year_all, "overview_temporal_year.csv")
}

# Monthly (seasonal) patterns
if (!is.null(temporal_month_10) && !is.null(temporal_month_50)) {
  temporal_month_all <- rbindlist(list(temporal_month_10, temporal_month_50), fill = TRUE)
  write_table(temporal_month_all, "overview_temporal_month_seasonal.csv")
}

# Decadal summary
if (!is.null(temporal_decade_10) && !is.null(temporal_decade_50)) {
  temporal_decade_all <- rbindlist(list(temporal_decade_10, temporal_decade_50), fill = TRUE)
  write_table(temporal_decade_all, "overview_temporal_decade.csv")
}

# Year × month heatmap data
if (!is.null(temporal_year_month_10)) {
  write_table(temporal_year_month_10, "overview_temporal_heatmap_10km.csv")
}
if (!is.null(temporal_year_month_50)) {
  write_table(temporal_year_month_50, "overview_temporal_heatmap_50km.csv")
}

# Completeness - year
if (!is.null(temporal_year_complete_10) && !is.null(temporal_year_complete_50)) {
  year_complete_all <- rbindlist(list(temporal_year_complete_10, temporal_year_complete_50), fill = TRUE)
  
  completeness_summary <- year_complete_all[basisofrecord == "all", .(
    n_years = .N,
    n_complete_years = sum(complete_year, na.rm = TRUE),
    pct_complete = round(100 * mean(complete_year, na.rm = TRUE), 1),
    mean_months_per_year = round(mean(n_months_observed, na.rm = TRUE), 1)
  ), by = grid]
  
  write_table(completeness_summary, "overview_temporal_completeness.csv")
}

# Gap years summary
if (!is.null(temporal_gap_years_summary)) {
  write_table(temporal_gap_years_summary, "overview_temporal_gap_years.csv")
}

# Sampling frequency
if (!is.null(temporal_sampling_freq)) {
  write_table(temporal_sampling_freq, "overview_temporal_sampling_frequency.csv")
}

# Recency overview
if (!is.null(recency_10) && !is.null(recency_50)) {
  recency_all <- rbindlist(list(recency_10, recency_50), fill = TRUE)
  
  recency_overview <- recency_all[, .(
    n_cells = .N,
    mean_staleness_months = round(mean(staleness_months, na.rm = TRUE), 1),
    median_staleness_months = as.numeric(median(staleness_months, na.rm = TRUE)),
    pct_stale_1y = round(100 * mean(gap_stale_12m, na.rm = TRUE), 2),
    pct_stale_5y = round(100 * mean(gap_stale_5y, na.rm = TRUE), 2),
    mean_span_months = round(mean(observation_span_months, na.rm = TRUE), 1)
  ), by = .(grid, basisofrecord)]
  
  setorder(recency_overview, grid, basisofrecord)
  write_table(recency_overview, "overview_temporal_recency.csv")
}

# ===========================================================================
# 4. TAXONOMIC OVERVIEWS
# ===========================================================================

cli_h2("Creating Taxonomic Overviews")

# Coverage by rank
if (!is.null(tax_coverage_rank)) {
  setorder(tax_coverage_rank, -n_ref_total)
  write_table(tax_coverage_rank, "overview_taxonomic_by_rank.csv")
}

# Coverage by threat status
if (!is.null(tax_coverage_threat)) {
  # Add threat level order
  threat_order <- c("CR", "EN", "VU", "NT", "LC", "DD", "NE", NA)
  tax_coverage_threat[, threat_order := match(threatStatus, threat_order)]
  setorder(tax_coverage_threat, threat_order)
  tax_coverage_threat[, threat_order := NULL]
  write_table(tax_coverage_threat, "overview_taxonomic_by_threat.csv")
}

# Coverage by basis of record
if (!is.null(tax_coverage_basis)) {
  setorder(tax_coverage_basis, grid, -n_species)
  write_table(tax_coverage_basis, "overview_taxonomic_by_basis.csv")
}

# Rank × threat matrix
if (!is.null(tax_gap_summary)) {
  write_table(tax_gap_summary, "overview_taxonomic_rank_threat_matrix.csv")
}

# Gaps by family
if (!is.null(tax_gaps_family)) {
  setorder(tax_gaps_family, -n_taxa)
  write_table(tax_gaps_family, "overview_taxonomic_gaps_by_family.csv")
}

# Gaps by order
if (!is.null(tax_gaps_order)) {
  setorder(tax_gaps_order, -n_taxa)
  write_table(tax_gaps_order, "overview_taxonomic_gaps_by_order.csv")
}

# Missing taxa summary
if (!is.null(tax_missing)) {
  # Summary by rank
  missing_by_rank <- tax_missing[, .(
    n_missing = .N
  ), by = taxonRank]
  setorder(missing_by_rank, -n_missing)
  write_table(missing_by_rank, "overview_taxonomic_missing_by_rank.csv")
}

# ===========================================================================
# 5. ORDER-LEVEL ANALYSIS
# ===========================================================================

cli_h2("Creating Order-Level Analysis")

# Order × cell coverage
if (!is.null(order_cell_10)) {
  order_spatial <- order_cell_10[basisofrecord == "all", .(
    n_cells = uniqueN(eeacellcode),
    total_occurrences = sum(as.numeric(occurrences), na.rm = TRUE),
    n_species = sum(n_species, na.rm = TRUE),
    mean_occ_per_cell = round(mean(occurrences, na.rm = TRUE), 1)
  ), by = .(grid, order)]
  
  setorder(order_spatial, -total_occurrences)
  write_integrated(order_spatial, "order_spatial_coverage.csv")
}

# Order × time trends
if (!is.null(order_time_10)) {
  # Extract year
  order_time_copy <- copy(order_time_10)
  order_time_copy[, year := parse_year(yearmonth)]
  
  order_trends <- order_time_copy[basisofrecord == "all", .(
    total_occurrences = sum(as.numeric(occurrences), na.rm = TRUE),
    n_cells = sum(n_cells, na.rm = TRUE)
  ), by = .(grid, order, year)]
  
  write_integrated(order_trends, "order_temporal_trends.csv")
  
  # Order summary across all time
  order_summary <- order_time_copy[basisofrecord == "all" & !is.na(year), .(
    total_occurrences = sum(as.numeric(occurrences), na.rm = TRUE),
    n_years = as.double(uniqueN(year)),
    first_year = as.double(min(year, na.rm = TRUE)),
    last_year = as.double(max(year, na.rm = TRUE))
  ), by = .(grid, order)]
  
  setorder(order_summary, -total_occurrences)
  write_integrated(order_summary, "order_summary.csv")
}

# ===========================================================================
# 6. FAMILY-LEVEL ANALYSIS
# ===========================================================================

cli_h2("Creating Family-Level Analysis")

# Family × time trends
if (!is.null(family_time_10)) {
  family_time_copy <- copy(family_time_10)
  family_time_copy[, year := parse_year(yearmonth)]
  
  family_summary <- family_time_copy[basisofrecord == "all" & !is.na(year), .(
    total_occurrences = sum(as.numeric(occurrences), na.rm = TRUE),
    n_years = as.double(uniqueN(year)),
    first_year = as.double(min(year, na.rm = TRUE)),
    last_year = as.double(max(year, na.rm = TRUE))
  ), by = .(grid, order, family)]
  
  setorder(family_summary, order, -total_occurrences)
  write_integrated(family_summary, "family_summary.csv")
  
  # Top families
  top_families <- family_summary[, .(
    total_occurrences = sum(total_occurrences)
  ), by = family][order(-total_occurrences)][1:50]
  
  write_integrated(top_families, "family_top50.csv")
}

# ===========================================================================
# 7. SPECIES-LEVEL ANALYSIS
# ===========================================================================

cli_h2("Creating Species-Level Analysis")

# Spatial coverage per species (from taxonomic analysis)
if (!is.null(tax_spatial)) {
  # Species richness summary
  species_spatial_summary <- tax_spatial[, .(
    n_species = .N,
    mean_cells_10km = round(mean(n_cells_10km, na.rm = TRUE), 1),
    median_cells_10km = as.numeric(median(n_cells_10km, na.rm = TRUE)),
    mean_occurrences = round(mean(total_occ_10km, na.rm = TRUE), 1),
    n_widespread = sum(n_cells_10km >= 100, na.rm = TRUE),
    n_restricted = sum(n_cells_10km <= 5, na.rm = TRUE),
    n_poorly_sampled = sum(poorly_sampled_spatial | poorly_sampled_abundance, na.rm = TRUE)
  ), by = taxonRank]
  
  write_integrated(species_spatial_summary, "species_spatial_summary_by_rank.csv")
}

# Threatened species spatial coverage
if (!is.null(tax_threatened_spatial)) {
  threatened_summary <- tax_threatened_spatial[, .(
    n_species = .N,
    mean_cells_10km = round(mean(n_cells_10km, na.rm = TRUE), 1),
    mean_occurrences = round(mean(total_occ_10km, na.rm = TRUE), 1),
    n_poorly_sampled = sum(poorly_sampled_spatial | poorly_sampled_abundance, na.rm = TRUE)
  ), by = threatStatus]
  
  # Order by threat level
  threat_order <- c("CR", "EN", "VU", "NT")
  threatened_summary[, threat_order := match(threatStatus, threat_order)]
  setorder(threatened_summary, threat_order)
  threatened_summary[, threat_order := NULL]
  
  write_integrated(threatened_summary, "species_threatened_summary.csv")
  
  # Full threatened species list with spatial data
  write_integrated(tax_threatened_spatial, "species_threatened_spatial_detail.csv")
}

# ===========================================================================
# 8. INTEGRATED MULTI-DIMENSIONAL TABLES
# ===========================================================================

cli_h2("Creating Integrated Multi-Dimensional Tables")

# 8.1 Cell-level: Spatial × Temporal (recency per cell)
if (!is.null(spatial_10) && !is.null(recency_10)) {
  
  spatial_cells <- spatial_10[basisofrecord == "all", 
                               .(eeacellcode, occurrences, n_species, gap_zero, gap_low_q10)]
  
  recency_cells <- recency_10[basisofrecord == "all",
                               .(eeacellcode, last_ym, staleness_months, gap_stale_12m, gap_stale_5y,
                                 n_observations, observation_span_months)]
  
  cell_integrated <- merge(spatial_cells, recency_cells, by = "eeacellcode", all = TRUE)
  cell_integrated[, grid := "grid10km"]
  
  # Add priority score (higher = more urgent)
  cell_integrated[, priority_score := 
                    (gap_zero * 3) + 
                    (gap_low_q10 * 1) + 
                    (gap_stale_5y * 2) + 
                    (gap_stale_12m * 1)]
  
  setorder(cell_integrated, -priority_score, -staleness_months)
  write_integrated(cell_integrated, "cell_integrated_10km.csv")
}

# 8.2 Year-level: Temporal × Taxonomic (families per year)
if (!is.null(temporal_year_10) && !is.null(temporal_family_10)) {
  
  family_year <- temporal_family_10[, .(
    n_families = n_families,
    family_occurrences = total_occurrences
  ), by = .(grid, year)]
  
  year_integrated <- merge(
    temporal_year_10,
    family_year,
    by = c("grid", "year"),
    all.x = TRUE
  )
  
  write_integrated(year_integrated, "year_integrated_10km.csv")
}

# 8.3 Basis-level: All dimensions by basis of record
if (!is.null(spatial_overview) && !is.null(recency_overview) && !is.null(tax_coverage_basis)) {
  
  basis_spatial <- spatial_overview[grid == "grid10km", 
                                     .(basisofrecord, spatial_cells = n_cells, 
                                       spatial_coverage_pct = pct_coverage,
                                       spatial_occurrences = total_occurrences)]
  
  basis_temporal <- recency_overview[grid == "grid10km",
                                      .(basisofrecord, temporal_cells = n_cells,
                                        temporal_staleness_median = median_staleness_months,
                                        temporal_pct_stale_5y = pct_stale_5y)]
  
  basis_taxonomic <- tax_coverage_basis[grid == "grid10km",
                                         .(basisofrecord, taxonomic_n_species = n_species)]
  
  basis_integrated <- Reduce(function(x, y) merge(x, y, by = "basisofrecord", all = TRUE),
                              list(basis_spatial, basis_temporal, basis_taxonomic))
  
  write_integrated(basis_integrated, "basis_integrated_10km.csv")
}

# ===========================================================================
# 9. PRIORITY LISTS FOR ACTION
# ===========================================================================

cli_h2("Creating Priority Lists")

# 9.1 Priority cells: Zero coverage
if (!is.null(spatial_zero_cells)) {
  priority_zero <- copy(spatial_zero_cells)
  priority_zero[, priority_reason := "Zero coverage - never sampled"]
  priority_zero[, priority_level := "HIGH"]
  write_integrated(priority_zero, "priority_cells_zero_coverage.csv")
}

# 9.2 Priority cells: Low coverage
if (!is.null(spatial_low_cells)) {
  priority_low <- copy(spatial_low_cells)
  priority_low[, priority_reason := "Low coverage - bottom 10%"]
  priority_low[, priority_level := "MEDIUM"]
  write_integrated(priority_low, "priority_cells_low_coverage.csv")
}

# 9.3 Priority cells: Stale (not sampled in 5+ years)
if (!is.null(recency_10)) {
  stale_cells <- recency_10[basisofrecord == "all" & gap_stale_5y == TRUE]
  
  if (nrow(stale_cells) > 0) {
    priority_stale <- stale_cells[, .(
      grid, eeacellcode, last_ym, staleness_months, total_occurrences, n_observations
    )]
    priority_stale[, years_since_sampled := round(staleness_months / 12, 1)]
    priority_stale[, priority_reason := paste0("Stale - not sampled in ", years_since_sampled, " years")]
    priority_stale[, priority_level := ifelse(staleness_months > 120, "HIGH", "MEDIUM")]
    
    setorder(priority_stale, -staleness_months)
    write_integrated(priority_stale, "priority_cells_stale.csv")
  }
}

# 9.4 Priority taxa: Missing threatened species
if (!is.null(tax_missing_threatened) && nrow(tax_missing_threatened) > 0) {
  priority_threatened_missing <- copy(tax_missing_threatened)
  
  # Select key columns
  keep_cols <- intersect(
    c("taxonID", "scientificName", "taxonRank", "threatStatus", "family", "order"),
    names(priority_threatened_missing)
  )
  priority_threatened_missing <- priority_threatened_missing[, ..keep_cols]
  
  priority_threatened_missing[, priority_reason := "Threatened species - not in GBIF"]
  priority_threatened_missing[, priority_level := ifelse(threatStatus %in% c("CR", "EN"), "CRITICAL", "HIGH")]
  
  # Order by threat level
  threat_order <- c("CR", "EN", "VU", "NT")
  priority_threatened_missing[, threat_order := match(threatStatus, threat_order)]
  setorder(priority_threatened_missing, threat_order, scientificName)
  priority_threatened_missing[, threat_order := NULL]
  
  write_integrated(priority_threatened_missing, "priority_taxa_threatened_missing.csv")
}

# 9.5 Priority taxa: Poorly sampled threatened species
if (!is.null(tax_priority) && nrow(tax_priority) > 0) {
  priority_taxa <- copy(tax_priority)
  
  # Add priority level
  priority_taxa[, priority_level := ifelse(
    threatStatus %in% c("CR", "EN"), "CRITICAL",
    ifelse(threatStatus %in% c("VU", "NT"), "HIGH", "MEDIUM")
  )]
  
  write_integrated(priority_taxa, "priority_taxa_all.csv")
}

# 9.6 Combined priority summary
priority_summary <- data.table(
  priority_type = c(
    "Cells - Zero coverage",
    "Cells - Low coverage (Q10)",
    "Cells - Stale (5+ years)",
    "Taxa - Threatened missing",
    "Taxa - Poorly sampled"
  ),
  count = c(
    calc_metric(nrow(spatial_zero_cells)),
    calc_metric(nrow(spatial_low_cells)),
    calc_metric(nrow(recency_10[basisofrecord == "all" & gap_stale_5y == TRUE])),
    calc_metric(nrow(tax_missing_threatened)),
    calc_metric(nrow(tax_priority))
  )
)

write_table(priority_summary, "priority_summary.csv")

# ===========================================================================
# 10. COMPARISON TABLES
# ===========================================================================

cli_h2("Creating Comparison Tables")

# Grid resolution comparison
if (!is.null(spatial_summary_grid)) {
  write_table(spatial_summary_grid, "comparison_grid_resolutions.csv")
}

# Basis type comparison
if (!is.null(spatial_overview)) {
  basis_comparison <- spatial_overview[basisofrecord != "all", .(
    grid, basisofrecord, n_cells = n_with_data, pct_coverage, total_occurrences
  )]
  setorder(basis_comparison, grid, -total_occurrences)
  write_table(basis_comparison, "comparison_basis_types.csv")
}

# Decade comparison
if (!is.null(temporal_decade_10)) {
  write_table(temporal_decade_10, "comparison_decades.csv")
}

# ===========================================================================
# OVERVIEW-DERIVED TABLES  (T-R3)
# ===========================================================================
# Order-trend views, overview last-year stats, and Troudet-style sampling bias
# were previously computed inside script 11. They live here now so that 11 is a
# pure loader, and every temporal window is anchored on the data snapshot /
# recent_cutoff rather than the wall clock (reproducible across reruns).

cli_h2("Creating Overview-Derived Tables")

# Snapshot year (cube download date) — wall-clock-free window reference.
snapshot_year <- year(get_snapshot_date())

# Recent-period cutoff (rolling 12 months from the snapshot), produced by 09c.
.recent_cutoff   <- safe_read(here(p_data_proc, "recent_cutoff.rds"), type = "rds")
recent_cutoff_ym <- if (!is.null(.recent_cutoff)) .recent_cutoff$cutoff_ym else
                    as.integer(paste0(snapshot_year - 1L, "01"))
recent_label     <- if (!is.null(.recent_cutoff)) .recent_cutoff$label else
                    as.character(snapshot_year - 1L)

# --- Order-trend views (from order_temporal_trends.csv, written above) ---
ov_order_temporal <- safe_read(here(p_integrated, "order_temporal_trends.csv"))
if (!is.null(ov_order_temporal)) {
  ov_order_temporal <- as_tibble(ov_order_temporal)
  current_year <- snapshot_year

  order_5yr <- ov_order_temporal |>
    filter(year >= 1970, year <= current_year) |>
    mutate(period_start = floor(year / 5) * 5,
           period = paste0(period_start, "-", period_start + 4)) |>
    group_by(grid, order, period, period_start) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE),
              n_cells = sum(n_cells, na.rm = TRUE), .groups = "drop")
  write_integrated(order_5yr, "order_5yr.csv")

  top_orders <- ov_order_temporal |>
    group_by(order) |>
    summarise(total = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |> slice_head(n = 25)
  write_integrated(top_orders, "order_top25.csv")

  recent_cutoff_year <- current_year - 10
  historical_cutoff  <- current_year - 20
  order_change <- ov_order_temporal |>
    filter(order %in% top_orders$order[1:12]) |>
    mutate(era = case_when(year >= recent_cutoff_year ~ "Recent",
                           year >= historical_cutoff ~ "Historical",
                           TRUE ~ NA_character_)) |>
    filter(!is.na(era)) |>
    group_by(order, era) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occurrences, values_fill = 0) |>
    filter(Historical > 0) |>
    mutate(pct_change = round(100 * (Recent - Historical) / Historical, 1),
           direction = ifelse(pct_change >= 0, "Increased", "Decreased")) |>
    arrange(desc(pct_change))
  write_integrated(order_change, "order_change.csv")
}

# --- Overview last-year stats (09c "all"-scope time_summary + cell_last_year) ---
ov_ts   <- safe_read_derived("time_summary_all_10km.csv")
ov_cly  <- safe_read_derived("cell_last_year_all_10km.csv")
ov_zero <- safe_read(here(p_integrated, "priority_cells_zero_coverage.csv"))
if (!is.null(ov_ts)) {
  ov_ts_all <- as_tibble(ov_ts) |> filter(basisofrecord == "all")

  yearly_totals <- ov_ts_all |>
    group_by(year) |>
    summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
              n_cells = sum(as.numeric(n_cells), na.rm = TRUE), .groups = "drop")
  write_integrated(yearly_totals, "yearly_totals.csv")

  ov_recent <- ov_ts_all |> filter(yearmonth >= recent_cutoff_ym) |>
    summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
              n_cells = sum(as.numeric(n_cells), na.rm = TRUE))
  ov_prior <- ov_ts_all |> filter(yearmonth < recent_cutoff_ym) |>
    summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE))
  ov_cly_tb <- if (!is.null(ov_cly)) as_tibble(ov_cly) else NULL

  # cells_active_last_year: distinct cells with any recent record from the
  # per-cell layer (NOT sum(n_cells), which over-counts a cell once per month).
  overview_last_year <- data.frame(
    label     = recent_label,
    cutoff_ym = recent_cutoff_ym,
    occ_last_year = ov_recent$total_occ[1],
    occ_prior     = ov_prior$total_occ[1],
    cells_active_last_year = if (!is.null(ov_cly_tb)) sum(ov_cly_tb$occ_last_year > 0, na.rm = TRUE) else NA_integer_,
    cells_newly_covered    = if (!is.null(ov_cly_tb)) sum(ov_cly_tb$newly_covered, na.rm = TRUE) else NA_integer_,
    cells_resolved = if (!is.null(ov_cly_tb) && !is.null(ov_zero))
      nrow(ov_cly_tb |> filter(eeacellcode %in% ov_zero$eeacellcode, occ_last_year > 0)) else NA_integer_,
    stringsAsFactors = FALSE
  )
  write_integrated(overview_last_year, "overview_last_year.csv")

  if (!is.null(ov_cly_tb) && !is.null(ov_zero)) {
    priority_resolved_last_year <- ov_cly_tb |>
      filter(eeacellcode %in% ov_zero$eeacellcode, occ_last_year > 0)
    write_integrated(priority_resolved_last_year, "priority_resolved_last_year.csv")
  }
}

# --- Troudet-style sampling bias (class / order / family) ---
# match_summary (09b coverage table) is a T-R3 moved-in dependency: the Troudet
# block below was relocated here from script 11, which loaded it separately.
match_summary <- safe_read_gap("taxonomic_match_summary.csv")
ov_fts <- safe_read_derived("family_time_summary_all_10km.csv")
if (!is.null(match_summary) && !is.null(ov_order_temporal)) {
  year_cutoff <- as.integer(substr(as.character(recent_cutoff_ym), 1, 4))

  known_by_class <- match_summary |> as_tibble() |>
    filter(!is.na(class), class != "") |>
    group_by(kingdom, phylum, class) |>
    summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop")

  order_to_class <- match_summary |> as_tibble() |>
    filter(!is.na(order), order != "", !is.na(class), class != "") |>
    distinct(kingdom, phylum, class, order)

  occ_by_class <- ov_order_temporal |> as_tibble() |>
    inner_join(order_to_class, by = "order") |>
    mutate(era = ifelse(year >= year_cutoff, "last_year", "prior")) |>
    group_by(kingdom, phylum, class, era) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)
  if (!"prior" %in% names(occ_by_class)) occ_by_class$prior <- 0
  if (!"last_year" %in% names(occ_by_class)) occ_by_class$last_year <- 0

  troudet_bias <- known_by_class |>
    left_join(occ_by_class, by = c("kingdom", "phylum", "class")) |>
    mutate(prior = replace_na(prior, 0), last_year = replace_na(last_year, 0),
           total_occ = prior + last_year, total_known = sum(n_known_species),
           total_occ_all = sum(total_occ), pct_known = n_known_species / total_known,
           ideal_occ = pct_known * total_occ_all, bias = total_occ - ideal_occ) |>
    select(kingdom, phylum, class, n_known_species, n_in_gbif,
           occ_prior = prior, occ_last_year = last_year, total_occ, ideal_occ, bias) |>
    arrange(desc(abs(bias)))
  write_integrated(troudet_bias, "troudet_bias_class.csv")

  known_by_order <- match_summary |> as_tibble() |>
    filter(!is.na(order), order != "") |>
    group_by(kingdom, phylum, class, order) |>
    summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop")

  occ_by_order <- ov_order_temporal |> as_tibble() |>
    inner_join(order_to_class, by = "order") |>
    mutate(era = ifelse(year >= year_cutoff, "last_year", "prior")) |>
    group_by(kingdom, phylum, class, order, era) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)
  if (!"prior" %in% names(occ_by_order)) occ_by_order$prior <- 0
  if (!"last_year" %in% names(occ_by_order)) occ_by_order$last_year <- 0

  troudet_bias_order <- known_by_order |>
    left_join(occ_by_order, by = c("kingdom", "phylum", "class", "order")) |>
    mutate(prior = replace_na(prior, 0), last_year = replace_na(last_year, 0),
           total_occ = prior + last_year, total_known = sum(n_known_species),
           total_occ_all = sum(total_occ), pct_known = n_known_species / total_known,
           ideal_occ = pct_known * total_occ_all, bias = total_occ - ideal_occ) |>
    select(kingdom, phylum, class, order, n_known_species, n_in_gbif,
           occ_prior = prior, occ_last_year = last_year, total_occ, ideal_occ, bias) |>
    arrange(desc(abs(bias)))
  write_integrated(troudet_bias_order, "troudet_bias_order.csv")

  if (!is.null(ov_fts) && "family" %in% names(match_summary) && "order" %in% names(match_summary)) {
    order_to_family <- match_summary |> as_tibble() |>
      filter(!is.na(family), family != "", !is.na(order), order != "", !is.na(class), class != "") |>
      distinct(kingdom, phylum, class, order, family)

    occ_by_family <- as_tibble(ov_fts) |>
      filter(basisofrecord == "all") |>
      inner_join(order_to_family, by = c("order", "family")) |>
      mutate(era = ifelse(year >= year_cutoff, "last_year", "prior")) |>
      group_by(kingdom, phylum, class, order, family, era) |>
      summarise(occurrences = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)
    if (!"prior" %in% names(occ_by_family)) occ_by_family$prior <- 0
    if (!"last_year" %in% names(occ_by_family)) occ_by_family$last_year <- 0

    known_by_family <- match_summary |> as_tibble() |>
      filter(!is.na(family), family != "") |>
      group_by(kingdom, phylum, class, order, family) |>
      summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop")

    troudet_bias_family <- known_by_family |>
      left_join(occ_by_family, by = c("kingdom", "phylum", "class", "order", "family")) |>
      mutate(prior = replace_na(prior, 0), last_year = replace_na(last_year, 0),
             total_occ = prior + last_year, total_known = sum(n_known_species),
             total_occ_all = sum(total_occ), pct_known = n_known_species / total_known,
             ideal_occ = pct_known * total_occ_all, bias = total_occ - ideal_occ) |>
      select(kingdom, phylum, class, order, family, n_known_species, n_in_gbif,
             occ_prior = prior, occ_last_year = last_year, total_occ, ideal_occ, bias) |>
      arrange(desc(abs(bias)))
    write_integrated(troudet_bias_family, "troudet_bias_family.csv")
  }
}


# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 10)")

n_integrated <- length(list.files(p_integrated, pattern = "\\.csv$"))
n_tables <- length(list.files(p_tables, pattern = "\\.csv$"))

summary_dt <- data.table(
  Output = c("Standard tables", "Integrated tables", "Total files"),
  Count = c(n_tables, n_integrated, n_tables + n_integrated),
  Location = c(p_tables, p_integrated, "-")
)

print(summary_dt)

cli_alert_success("Gap overview complete!")
cli_alert_info("Standard tables: {.path {p_tables}}")
cli_alert_info("Integrated tables: {.path {p_integrated}}")
cli_alert_info("Next: source('scripts/11_prepare_gap_finder_data.R')")
