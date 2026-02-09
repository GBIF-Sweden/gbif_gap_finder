# scripts/11_prepare_shiny_data.R
# ==============================================================================
# Prepare Data for Shiny App / Interactive Visualization
# ==============================================================================
# This script:
# - Reads all outputs from the gap analysis pipeline (scripts 07-10)
# - Combines and optimizes data for fast Shiny app loading
# - Pre-aggregates data for common visualizations
# - Saves everything as a single .rds file bundle
#
# Run this after: Scripts 01-10 (full pipeline)
# Output: data_proc/shiny_data.rds
#
# The output bundle contains:
# - Grid geometries (simplified for web rendering)
# - Dashboard summary metrics
# - Spatial gap data (cell-level and summaries)
# - Temporal data (trends, seasonality, recency)
# - Taxonomic data (coverage, gaps, priorities)
# - Order/family summaries
# - Priority lists
# - Pre-aggregated data for common charts

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(sf)
library(data.table)
library(cli)
library(glue)
library(lubridate)

source(here("scripts", "00_setup.R"))

cli_h1("Preparing Data for Shiny App (Script 11)")

# ===========================================================================
# CONFIGURATION
# ===========================================================================

p_gaps <- here(p_data_proc, "gaps")
p_tables <- here(p_output, "tables")
p_integrated <- here(p_output, "tables", "integrated")
p_derived <- here(p_data_proc, "derived")

# Output path
shiny_data_path <- here(p_data_proc, "shiny_data.rds")

# Initialize data list
shiny_data <- list()

# Helper function for safe file reading
safe_read <- function(path, type = "csv") {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch({
    if (type == "csv") {
      fread(path)
    } else if (type == "rds") {
      readRDS(path)
    }
  }, error = function(e) {
    cli_alert_warning("Error reading {basename(path)}: {e$message}")
    NULL
  })
}

# ===========================================================================
# 1. GRID GEOMETRIES
# ===========================================================================

cli_h2("Loading Grid Geometries")

# 10km grid (primary)
grid_10km_path <- here(p_data_proc, "grids_10km.gpkg")
if (file.exists(grid_10km_path)) {
  grid_10km <- st_read(grid_10km_path, quiet = TRUE)
  
  # Standardize cellcode column
  cellcode_field <- guess_cellcode_field(names(grid_10km))
  if (!is.na(cellcode_field)) {
    grid_10km$eeacellcode <- as.character(grid_10km[[cellcode_field]])
  }
  
  # Simplify geometry for faster web rendering
  grid_10km_simple <- grid_10km |>
    st_simplify(dTolerance = 500) |>
    select(eeacellcode)
  
  # Transform to WGS84 for Leaflet
  shiny_data$grid_10km <- st_transform(grid_10km_simple, 4326)
  cli_alert_success("10km grid: {nrow(shiny_data$grid_10km)} cells (simplified)")
  
  rm(grid_10km, grid_10km_simple)
  invisible(gc())
}

# 50km grid
grid_50km_path <- here(p_data_proc, "grids_50km.gpkg")
if (file.exists(grid_50km_path)) {
  grid_50km <- st_read(grid_50km_path, quiet = TRUE)
  
  cellcode_field <- guess_cellcode_field(names(grid_50km))
  if (!is.na(cellcode_field)) {
    grid_50km$eeacellcode <- as.character(grid_50km[[cellcode_field]])
  }
  
  grid_50km_simple <- grid_50km |>
    st_simplify(dTolerance = 1000) |>
    select(eeacellcode)
  
  shiny_data$grid_50km <- st_transform(grid_50km_simple, 4326)
  cli_alert_success("50km grid: {nrow(shiny_data$grid_50km)} cells (simplified)")
  
  rm(grid_50km, grid_50km_simple)
  invisible(gc())
}

# ===========================================================================
# 2. DASHBOARD SUMMARY
# ===========================================================================

cli_h2("Loading Dashboard Summary")

dashboard <- safe_read(here(p_tables, "dashboard_summary.csv"))
if (!is.null(dashboard)) {
  shiny_data$dashboard <- as_tibble(dashboard)
  cli_alert_success("Dashboard summary loaded")
}

dashboard_long <- safe_read(here(p_tables, "dashboard_summary_long.csv"))
if (!is.null(dashboard_long)) {
  shiny_data$dashboard_long <- as_tibble(dashboard_long)
  cli_alert_success("Dashboard summary (long format) loaded")
}

# ===========================================================================
# 3. SPATIAL DATA
# ===========================================================================

cli_h2("Loading Spatial Data")

# Cell-level spatial gaps (10km)
spatial_gaps_10 <- safe_read(here(p_gaps, "spatial_gaps_10km.csv"))
if (!is.null(spatial_gaps_10)) {
  # Filter to "all" basis for mapping
  shiny_data$spatial_gaps_10km <- spatial_gaps_10[basisofrecord == "all"] |> as_tibble()
  cli_alert_success("Spatial gaps 10km: {nrow(shiny_data$spatial_gaps_10km)} cells")
}

# Spatial overview by basis
spatial_overview <- safe_read(here(p_tables, "overview_spatial_by_basis.csv"))
if (!is.null(spatial_overview)) {
  shiny_data$spatial_overview <- as_tibble(spatial_overview)
  cli_alert_success("Spatial overview loaded")
}

# Spatial summary by grid
spatial_by_grid <- safe_read(here(p_tables, "overview_spatial_by_grid.csv"))
if (!is.null(spatial_by_grid)) {
  shiny_data$spatial_by_grid <- as_tibble(spatial_by_grid)
  cli_alert_success("Spatial by grid loaded")
}

# Spatial thresholds
spatial_thresholds <- safe_read(here(p_tables, "overview_spatial_thresholds.csv"))
if (!is.null(spatial_thresholds)) {
  shiny_data$spatial_thresholds <- as_tibble(spatial_thresholds)
  cli_alert_success("Spatial thresholds loaded")
}

# Zero coverage cells
zero_cells <- safe_read(here(p_integrated, "priority_cells_zero_coverage.csv"))
if (!is.null(zero_cells)) {
  shiny_data$priority_zero_cells <- as_tibble(zero_cells)
  cli_alert_success("Zero coverage cells: {nrow(shiny_data$priority_zero_cells)}")
}

# Low coverage cells
low_cells <- safe_read(here(p_integrated, "priority_cells_low_coverage.csv"))
if (!is.null(low_cells)) {
  shiny_data$priority_low_cells <- as_tibble(low_cells)
  cli_alert_success("Low coverage cells: {nrow(shiny_data$priority_low_cells)}")
}

# ===========================================================================
# 4. TEMPORAL DATA
# ===========================================================================

cli_h2("Loading Temporal Data")

# Annual trends
temporal_year <- safe_read(here(p_tables, "overview_temporal_year.csv"))
if (!is.null(temporal_year)) {
  shiny_data$temporal_year <- as_tibble(temporal_year)
  cli_alert_success("Temporal year overview loaded")
}

# Monthly/seasonal patterns
temporal_month <- safe_read(here(p_tables, "overview_temporal_month_seasonal.csv"))
if (!is.null(temporal_month)) {
  shiny_data$temporal_month <- as_tibble(temporal_month)
  cli_alert_success("Temporal month overview loaded")
}

# Decadal summary
temporal_decade <- safe_read(here(p_tables, "overview_temporal_decade.csv"))
if (!is.null(temporal_decade)) {
  shiny_data$temporal_decade <- as_tibble(temporal_decade)
  cli_alert_success("Temporal decade overview loaded")
}

# Year × month heatmap data
temporal_heatmap <- safe_read(here(p_tables, "overview_temporal_heatmap_10km.csv"))
if (!is.null(temporal_heatmap)) {
  shiny_data$temporal_heatmap <- as_tibble(temporal_heatmap)
  cli_alert_success("Temporal heatmap data loaded")
}

# Completeness
temporal_completeness <- safe_read(here(p_tables, "overview_temporal_completeness.csv"))
if (!is.null(temporal_completeness)) {
  shiny_data$temporal_completeness <- as_tibble(temporal_completeness)
  cli_alert_success("Temporal completeness loaded")
}

# Gap years summary
gap_years <- safe_read(here(p_tables, "overview_temporal_gap_years.csv"))
if (!is.null(gap_years)) {
  shiny_data$temporal_gap_years <- as_tibble(gap_years)
  cli_alert_success("Temporal gap years loaded")
}

# Recency overview
recency_overview <- safe_read(here(p_tables, "overview_temporal_recency.csv"))
if (!is.null(recency_overview)) {
  shiny_data$recency_overview <- as_tibble(recency_overview)
  cli_alert_success("Recency overview loaded")
}

# Cell recency (10km)
cell_recency <- safe_read(here(p_gaps, "cell_recency_10km.csv"))
if (!is.null(cell_recency)) {
  # Filter to "all" basis for mapping
  shiny_data$cell_recency_10km <- cell_recency[basisofrecord == "all"] |> as_tibble()
  cli_alert_success("Cell recency 10km: {nrow(shiny_data$cell_recency_10km)} cells")
}

# Stale cells priority
stale_cells <- safe_read(here(p_integrated, "priority_cells_stale.csv"))
if (!is.null(stale_cells)) {
  shiny_data$priority_stale_cells <- as_tibble(stale_cells)
  cli_alert_success("Stale cells: {nrow(shiny_data$priority_stale_cells)}")
}

# ===========================================================================
# 5. TAXONOMIC DATA
# ===========================================================================

cli_h2("Loading Taxonomic Data")

# Coverage by rank
tax_by_rank <- safe_read(here(p_tables, "overview_taxonomic_by_rank.csv"))
if (!is.null(tax_by_rank)) {
  shiny_data$tax_by_rank <- as_tibble(tax_by_rank)
  cli_alert_success("Taxonomic by rank loaded")
}

# Coverage by threat status
tax_by_threat <- safe_read(here(p_tables, "overview_taxonomic_by_threat.csv"))
if (!is.null(tax_by_threat)) {
  shiny_data$tax_by_threat <- as_tibble(tax_by_threat)
  cli_alert_success("Taxonomic by threat loaded")
}

# Coverage by basis
tax_by_basis <- safe_read(here(p_tables, "overview_taxonomic_by_basis.csv"))
if (!is.null(tax_by_basis)) {
  shiny_data$tax_by_basis <- as_tibble(tax_by_basis)
  cli_alert_success("Taxonomic by basis loaded")
}

# Rank × threat matrix
tax_rank_threat <- safe_read(here(p_tables, "overview_taxonomic_rank_threat_matrix.csv"))
if (!is.null(tax_rank_threat)) {
  shiny_data$tax_rank_threat <- as_tibble(tax_rank_threat)
  cli_alert_success("Taxonomic rank × threat matrix loaded")
}

# Gaps by family
tax_by_family <- safe_read(here(p_tables, "overview_taxonomic_gaps_by_family.csv"))
if (!is.null(tax_by_family)) {
  shiny_data$tax_by_family <- as_tibble(tax_by_family)
  cli_alert_success("Taxonomic by family: {nrow(shiny_data$tax_by_family)} families")
}

# Gaps by order
tax_by_order <- safe_read(here(p_tables, "overview_taxonomic_gaps_by_order.csv"))
if (!is.null(tax_by_order)) {
  shiny_data$tax_by_order <- as_tibble(tax_by_order)
  cli_alert_success("Taxonomic by order: {nrow(shiny_data$tax_by_order)} orders")
}

# Missing taxa by rank
missing_by_rank <- safe_read(here(p_tables, "overview_taxonomic_missing_by_rank.csv"))
if (!is.null(missing_by_rank)) {
  shiny_data$missing_by_rank <- as_tibble(missing_by_rank)
  cli_alert_success("Missing taxa by rank loaded")
}

# Threatened species summary
threatened_summary <- safe_read(here(p_integrated, "species_threatened_summary.csv"))
if (!is.null(threatened_summary)) {
  shiny_data$threatened_summary <- as_tibble(threatened_summary)
  cli_alert_success("Threatened species summary loaded")
}

# Threatened species detail
threatened_detail <- safe_read(here(p_integrated, "species_threatened_spatial_detail.csv"))
if (!is.null(threatened_detail)) {
  shiny_data$threatened_detail <- as_tibble(threatened_detail)
  cli_alert_success("Threatened species detail: {nrow(shiny_data$threatened_detail)} species")
}

# ===========================================================================
# 6. ORDER/FAMILY SUMMARIES
# ===========================================================================

cli_h2("Loading Order/Family Summaries")

# Order summary
order_summary <- safe_read(here(p_integrated, "order_summary.csv"))
if (!is.null(order_summary)) {
  shiny_data$order_summary <- as_tibble(order_summary)
  cli_alert_success("Order summary: {nrow(shiny_data$order_summary)} orders")
}

# Order spatial coverage
order_spatial <- safe_read(here(p_integrated, "order_spatial_coverage.csv"))
if (!is.null(order_spatial)) {
  shiny_data$order_spatial <- as_tibble(order_spatial)
  cli_alert_success("Order spatial coverage loaded")
}

# Order temporal trends
order_temporal <- safe_read(here(p_integrated, "order_temporal_trends.csv"))
if (!is.null(order_temporal)) {
  shiny_data$order_temporal <- as_tibble(order_temporal)
  cli_alert_success("Order temporal trends loaded")
  
  # Pre-aggregate into 5-year bins for visualizations
  current_year <- year(Sys.Date())
  shiny_data$order_5yr <- shiny_data$order_temporal |>
    filter(year >= 1970, year <= current_year) |>
    mutate(
      period_start = floor(year / 5) * 5,
      period = paste0(period_start, "-", period_start + 4)
    ) |>
    group_by(grid, order, period, period_start) |>
    summarise(
      occurrences = sum(total_occurrences, na.rm = TRUE),
      n_cells = sum(n_cells, na.rm = TRUE),
      .groups = "drop"
    )
  cli_alert_success("Order 5-year aggregates created")
  
  # Top orders by total occurrences
  shiny_data$top_orders <- shiny_data$order_temporal |>
    group_by(order) |>
    summarise(total = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    slice_head(n = 25)
  cli_alert_success("Top 25 orders identified")
}

# Family summary
family_summary <- safe_read(here(p_integrated, "family_summary.csv"))
if (!is.null(family_summary)) {
  shiny_data$family_summary <- as_tibble(family_summary)
  cli_alert_success("Family summary: {nrow(shiny_data$family_summary)} families")
}

# Top families
family_top50 <- safe_read(here(p_integrated, "family_top50.csv"))
if (!is.null(family_top50)) {
  shiny_data$family_top50 <- as_tibble(family_top50)
  cli_alert_success("Top 50 families loaded")
}

# ===========================================================================
# 7. PRIORITY LISTS
# ===========================================================================

cli_h2("Loading Priority Lists")

# Priority summary
priority_summary <- safe_read(here(p_tables, "priority_summary.csv"))
if (!is.null(priority_summary)) {
  shiny_data$priority_summary <- as_tibble(priority_summary)
  cli_alert_success("Priority summary loaded")
}

# Priority taxa - threatened missing
priority_threatened_missing <- safe_read(here(p_integrated, "priority_taxa_threatened_missing.csv"))
if (!is.null(priority_threatened_missing)) {
  shiny_data$priority_taxa_missing <- as_tibble(priority_threatened_missing)
  cli_alert_success("Priority taxa (missing): {nrow(shiny_data$priority_taxa_missing)}")
}

# Priority taxa - all
priority_taxa_all <- safe_read(here(p_integrated, "priority_taxa_all.csv"))
if (!is.null(priority_taxa_all)) {
  shiny_data$priority_taxa_all <- as_tibble(priority_taxa_all)
  cli_alert_success("Priority taxa (all): {nrow(shiny_data$priority_taxa_all)}")
}

# ===========================================================================
# 8. INTEGRATED TABLES
# ===========================================================================

cli_h2("Loading Integrated Tables")

# Cell integrated (spatial + temporal)
cell_integrated <- safe_read(here(p_integrated, "cell_integrated_10km.csv"))
if (!is.null(cell_integrated)) {
  shiny_data$cell_integrated <- as_tibble(cell_integrated)
  cli_alert_success("Cell integrated: {nrow(shiny_data$cell_integrated)} cells")
}

# Basis integrated
basis_integrated <- safe_read(here(p_integrated, "basis_integrated_10km.csv"))
if (!is.null(basis_integrated)) {
  shiny_data$basis_integrated <- as_tibble(basis_integrated)
  cli_alert_success("Basis integrated loaded")
}

# ===========================================================================
# 9. COMPARISON TABLES
# ===========================================================================

cli_h2("Loading Comparison Tables")

# Grid comparison
grid_comparison <- safe_read(here(p_tables, "comparison_grid_resolutions.csv"))
if (!is.null(grid_comparison)) {
  shiny_data$comparison_grids <- as_tibble(grid_comparison)
  cli_alert_success("Grid comparison loaded")
}

# Basis comparison
basis_comparison <- safe_read(here(p_tables, "comparison_basis_types.csv"))
if (!is.null(basis_comparison)) {
  shiny_data$comparison_basis <- as_tibble(basis_comparison)
  cli_alert_success("Basis comparison loaded")
}

# Decade comparison
decade_comparison <- safe_read(here(p_tables, "comparison_decades.csv"))
if (!is.null(decade_comparison)) {
  shiny_data$comparison_decades <- as_tibble(decade_comparison)
  cli_alert_success("Decade comparison loaded")
}

# ===========================================================================
# 10. DERIVED SUMMARIES (for detailed exploration)
# ===========================================================================

cli_h2("Loading Derived Summaries")

# Cell summary (for custom aggregations)
cell_summary <- safe_read(here(p_derived, "cell_summary_10km.csv"))
if (!is.null(cell_summary)) {
  shiny_data$cell_summary_10km <- cell_summary[basisofrecord == "all"] |> as_tibble()
  cli_alert_success("Cell summary 10km: {nrow(shiny_data$cell_summary_10km)} cells")
}

# Time summary (for custom charts)
time_summary <- safe_read(here(p_derived, "time_summary_10km.csv"))
if (!is.null(time_summary)) {
  shiny_data$time_summary_10km <- time_summary[basisofrecord == "all"] |> 
    as_tibble() |>
    mutate(
      yearmonth_chr = str_trim(as.character(yearmonth)),
      year = as.integer(str_sub(yearmonth_chr, 1, 4)),
      month = as.integer(str_sub(yearmonth_chr, 6, 7))
    )
  cli_alert_success("Time summary 10km: {nrow(shiny_data$time_summary_10km)} rows")
}

# ===========================================================================
# 11. METADATA
# ===========================================================================

cli_h2("Adding Metadata")

# Get dataset names (excluding metadata)
dataset_names <- names(shiny_data)

shiny_data$metadata <- list(
  created_at = Sys.time(),
  created_by = "scripts/11_prepare_shiny_data.R",
  r_version = R.version.string,
  n_datasets = length(dataset_names),
  datasets = dataset_names,
  
  # Summary counts
  n_cells_10km = if (!is.null(shiny_data$grid_10km)) nrow(shiny_data$grid_10km) else NA,
  n_cells_50km = if (!is.null(shiny_data$grid_50km)) nrow(shiny_data$grid_50km) else NA,
  
  # Data availability flags
  has_spatial = !is.null(shiny_data$spatial_gaps_10km),
  has_temporal = !is.null(shiny_data$temporal_year),
  has_taxonomic = !is.null(shiny_data$tax_by_rank),
  has_orders = !is.null(shiny_data$order_summary),
  has_priorities = !is.null(shiny_data$priority_summary)
)

# ===========================================================================
# 12. SAVE BUNDLE
# ===========================================================================

cli_h2("Saving Shiny Data Bundle")

saveRDS(shiny_data, shiny_data_path, compress = "xz")

file_size_mb <- file.size(shiny_data_path) / 1024^2
cli_alert_success("Saved: {.path {shiny_data_path}} ({round(file_size_mb, 2)} MB)")

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 11)")

# Count by category
n_spatial <- sum(str_detect(dataset_names, "spatial|grid|cell"))
n_temporal <- sum(str_detect(dataset_names, "temporal|recency|time"))
n_taxonomic <- sum(str_detect(dataset_names, "tax|order|family|threatened|missing"))
n_priority <- sum(str_detect(dataset_names, "priority"))
n_other <- length(dataset_names) - n_spatial - n_temporal - n_taxonomic - n_priority

summary_dt <- data.table(
  Category = c("Spatial", "Temporal", "Taxonomic", "Priority", "Other", "TOTAL"),
  Datasets = c(n_spatial, n_temporal, n_taxonomic, n_priority, n_other, length(dataset_names))
)

print(summary_dt)

cli_alert_info("")
cli_alert_info("Dataset details:")
for (ds in dataset_names) {
  obj <- shiny_data[[ds]]
  if (is.data.frame(obj)) {
    cli_alert_info("  {ds}: {scales::comma(nrow(obj))} rows x {ncol(obj)} cols")
  } else if (inherits(obj, "sf")) {
    cli_alert_info("  {ds}: {scales::comma(nrow(obj))} features (sf)")
  } else if (is.list(obj)) {
    cli_alert_info("  {ds}: list with {length(obj)} elements")
  }
}

cli_alert_success("")
cli_alert_success("Shiny data preparation complete!")
cli_alert_info("Output: {.path {shiny_data_path}}")
cli_alert_info("Size: {round(file_size_mb, 2)} MB")
