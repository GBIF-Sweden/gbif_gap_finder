# scripts/12_prepare_shiny_data.R
# ==============================================================================
# Prepare Data for Shiny App
# ==============================================================================
# This script:
# - Reads all outputs from the gap analysis pipeline
# - Combines and optimizes data for fast Shiny app loading
# - Saves everything as a single .rds file bundle
#
# Run this after: Scripts 01-11 (full pipeline)
# Output: data_proc/shiny_data.rds

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

cli_h1("Preparing Data for Shiny App")

# Paths
p_gaps <- here("data_proc", "gaps")
p_tables <- here("output", "tables")
p_integrated <- here("output", "tables", "integrated")
p_derived <- here("data_proc", "derived")

# Output path
shiny_data_path <- here("data_proc", "shiny_data.rds")

# Initialize data list
shiny_data <- list()

# =============================================================================
# 1. Dashboard Summary
# =============================================================================
cli_h2("Loading Dashboard Summary")

dashboard_path <- here(p_tables, "dashboard_summary.csv")
if (file.exists(dashboard_path)) {
  shiny_data$dashboard <- read_csv(dashboard_path, show_col_types = FALSE)
  cli_alert_success("Dashboard summary loaded")
} else {
  cli_alert_warning("Dashboard summary not found")
}

# =============================================================================
# 2. Spatial Data
# =============================================================================
cli_h2("Loading Spatial Data")

# Grid geometries (simplified for faster rendering)
grid_10km_path <- here(p_data_proc, "grids_10km.gpkg")
if (file.exists(grid_10km_path)) {
  grid_10km <- st_read(grid_10km_path, quiet = TRUE)
  
 # Find cellcode field
  cellcode_candidates <- names(grid_10km)[grepl("cellcode|eeacell", tolower(names(grid_10km)))]
  cellcode_field <- if(length(cellcode_candidates) > 0) {
    cellcode_candidates[1]
  } else {
    names(grid_10km)[grepl("code", tolower(names(grid_10km)))][1]
  }
  
  # Standardize cellcode column name
  grid_10km$eeacellcode <- as.character(grid_10km[[cellcode_field]])
  
  # Simplify geometry for faster web rendering
  grid_10km_simple <- grid_10km |>
    st_simplify(dTolerance = 500) |>
    select(eeacellcode)
  
  # Transform to WGS84 for Leaflet
  shiny_data$grid_10km <- st_transform(grid_10km_simple, 4326)
  cli_alert_success("10km grid loaded and simplified ({nrow(shiny_data$grid_10km)} cells)")
}

# Spatial gaps
spatial_gaps_path <- here(p_gaps, "spatial_gaps_10km.csv")
if (file.exists(spatial_gaps_path)) {
  shiny_data$spatial_gaps <- fread(spatial_gaps_path) |> as_tibble()
  cli_alert_success("Spatial gaps loaded ({nrow(shiny_data$spatial_gaps)} rows)")
}

# Spatial overview
spatial_overview_path <- here(p_tables, "overview_spatial_gap_rates.csv")
if (file.exists(spatial_overview_path)) {
  shiny_data$spatial_overview <- read_csv(spatial_overview_path, show_col_types = FALSE)
  cli_alert_success("Spatial overview loaded")
}

# Grid comparison
grid_comparison_path <- here(p_tables, "comparison_grid_resolutions.csv")
if (file.exists(grid_comparison_path)) {
  shiny_data$grid_comparison <- read_csv(grid_comparison_path, show_col_types = FALSE)
  cli_alert_success("Grid comparison loaded")
}

# Priority zero coverage cells
priority_zero_path <- here(p_integrated, "priority_zero_coverage_cells.csv")
if (file.exists(priority_zero_path)) {
  shiny_data$priority_zero <- fread(priority_zero_path) |> as_tibble()
  cli_alert_success("Priority zero cells loaded ({nrow(shiny_data$priority_zero)} cells)")
}

# =============================================================================
# 3. Temporal Data
# =============================================================================
cli_h2("Loading Temporal Data")

# Time summary
time_summary_path <- here(p_derived, "time_summary_10km.csv")
if (file.exists(time_summary_path)) {
  shiny_data$time_summary <- read_csv(time_summary_path, show_col_types = FALSE) |>
    mutate(
      yearmonth_chr = str_trim(as.character(yearmonth)),
      year = as.integer(str_sub(yearmonth_chr, 1, 4)),
      month = as.integer(str_sub(yearmonth_chr, 6, 7))
    )
  cli_alert_success("Time summary loaded ({nrow(shiny_data$time_summary)} rows)")
}

# Cell recency
cell_recency_path <- here(p_gaps, "temporal_cell_recency_10km.csv")
if (file.exists(cell_recency_path)) {
  shiny_data$cell_recency <- fread(cell_recency_path) |> as_tibble()
  cli_alert_success("Cell recency loaded ({nrow(shiny_data$cell_recency)} rows)")
}

# Temporal overview
temporal_year_path <- here(p_tables, "overview_temporal_year.csv")
if (file.exists(temporal_year_path)) {
  shiny_data$temporal_year <- read_csv(temporal_year_path, show_col_types = FALSE)
  cli_alert_success("Temporal year overview loaded")
}

temporal_month_path <- here(p_tables, "overview_temporal_month.csv")
if (file.exists(temporal_month_path)) {
  shiny_data$temporal_month <- read_csv(temporal_month_path, show_col_types = FALSE)
  cli_alert_success("Temporal month overview loaded")
}

# Recency rates
recency_rates_path <- here(p_tables, "overview_temporal_recency_rates.csv")
if (file.exists(recency_rates_path)) {
  shiny_data$recency_rates <- read_csv(recency_rates_path, show_col_types = FALSE)
  cli_alert_success("Recency rates loaded")
}

# Priority stale cells
priority_stale_path <- here(p_integrated, "priority_stale_cells.csv")
if (file.exists(priority_stale_path)) {
  shiny_data$priority_stale <- fread(priority_stale_path) |> as_tibble()
  cli_alert_success("Priority stale cells loaded ({nrow(shiny_data$priority_stale)} cells)")
}

# =============================================================================
# 4. Taxonomic Data
# =============================================================================
cli_h2("Loading Taxonomic Data")

# Order time summary (for 5-year mobilization charts)
order_time_path <- here(p_derived, "order_time_summary_10km.csv")
if (file.exists(order_time_path)) {
  current_year <- year(Sys.Date())
  
  order_time_raw <- fread(order_time_path) |> as_tibble()
  
  # Pre-compute 5-year bins
  shiny_data$order_time <- order_time_raw |>
    filter(basisofrecord == "all") |>
    mutate(
      yearmonth_chr = str_trim(as.character(yearmonth)),
      year = as.integer(str_sub(yearmonth_chr, 1, 4))
    ) |>
    filter(year >= 1970, year <= current_year, !is.na(order), order != "")
  
  # Pre-aggregated 5-year summary
  shiny_data$order_5yr <- shiny_data$order_time |>
    mutate(
      period_start = floor(year / 5) * 5,
      period = paste0(period_start, "-", period_start + 4)
    ) |>
    group_by(order, period, period_start) |>
    summarise(occurrences = sum(occurrences, na.rm = TRUE), .groups = "drop")
  
  # Top orders list
  shiny_data$top_orders <- shiny_data$order_time |>
    group_by(order) |>
    summarise(total = sum(occurrences, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    slice_head(n = 20)
  
  cli_alert_success("Order time data loaded and pre-aggregated")
}

# Taxonomic coverage by rank
tax_rank_path <- here(p_gaps, "taxonomic_coverage_by_rank.csv")
if (file.exists(tax_rank_path)) {
  shiny_data$tax_by_rank <- read_csv(tax_rank_path, show_col_types = FALSE)
  cli_alert_success("Taxonomic coverage by rank loaded")
} else {
  # Try alternative path
  tax_summary_path <- here(p_tables, "overview_taxonomic_summary.csv")
  if (file.exists(tax_summary_path)) {
    shiny_data$tax_by_rank <- read_csv(tax_summary_path, show_col_types = FALSE)
    cli_alert_success("Taxonomic summary loaded")
  }
}

# Taxonomic coverage by threat
tax_threat_path <- here(p_gaps, "taxonomic_coverage_by_threat.csv")
if (file.exists(tax_threat_path)) {
  shiny_data$tax_by_threat <- read_csv(tax_threat_path, show_col_types = FALSE)
  cli_alert_success("Taxonomic coverage by threat loaded")
}

# Taxonomic coverage by order
tax_order_path <- here(p_gaps, "taxonomic_coverage_by_order.csv")
if (file.exists(tax_order_path)) {
  shiny_data$tax_by_order <- read_csv(tax_order_path, show_col_types = FALSE)
  cli_alert_success("Taxonomic coverage by order loaded")
}

# Taxonomic coverage by family
tax_family_path <- here(p_gaps, "taxonomic_coverage_by_family.csv")
if (file.exists(tax_family_path)) {
  shiny_data$tax_by_family <- read_csv(tax_family_path, show_col_types = FALSE)
  cli_alert_success("Taxonomic coverage by family loaded")
}

# Missing taxa
missing_taxa_path <- here(p_gaps, "taxonomic_missing_taxa.csv")
if (file.exists(missing_taxa_path)) {
  shiny_data$missing_taxa <- fread(missing_taxa_path) |> as_tibble()
  cli_alert_success("Missing taxa loaded ({nrow(shiny_data$missing_taxa)} taxa)")
}

# Priority undersampled taxa
priority_taxa_path <- here(p_integrated, "priority_undersampled_taxa.csv")
if (file.exists(priority_taxa_path)) {
  shiny_data$priority_taxa <- fread(priority_taxa_path) |> as_tibble()
  cli_alert_success("Priority taxa loaded ({nrow(shiny_data$priority_taxa)} taxa)")
}

# =============================================================================
# 5. Cube/Basis of Record Data
# =============================================================================
cli_h2("Loading Basis of Record Data")

cube_totals_path <- here(p_data_proc, "cube_totals_by_basisOfRecord.csv")
if (file.exists(cube_totals_path)) {
  shiny_data$cube_totals <- read_csv(cube_totals_path, show_col_types = FALSE)
  cli_alert_success("Cube totals loaded")
}

basis_comparison_path <- here(p_tables, "comparison_basis_types.csv")
if (file.exists(basis_comparison_path)) {
  shiny_data$basis_comparison <- read_csv(basis_comparison_path, show_col_types = FALSE)
  cli_alert_success("Basis comparison loaded")
}

# =============================================================================
# 6. Metadata
# =============================================================================
cli_h2("Adding Metadata")

shiny_data$metadata <- list(
  created_at = Sys.time(),
  created_by = "scripts/12_prepare_shiny_data.R",
  n_datasets = length(shiny_data) - 1,  # Exclude metadata itself
  datasets = names(shiny_data)[names(shiny_data) != "metadata"]
)

# =============================================================================
# 7. Save Bundle
# =============================================================================
cli_h2("Saving Shiny Data Bundle")

saveRDS(shiny_data, shiny_data_path, compress = "xz")

file_size_mb <- file.size(shiny_data_path) / 1024^2
cli_alert_success("Saved: {.path {shiny_data_path}} ({round(file_size_mb, 2)} MB)")

# Summary
cli_h2("Summary")
cli_alert_info("Datasets included: {length(shiny_data$metadata$datasets)}")
for (ds in shiny_data$metadata$datasets) {
  if (is.data.frame(shiny_data[[ds]])) {
    cli_alert_info("  - {ds}: {nrow(shiny_data[[ds]])} rows")
  } else if (inherits(shiny_data[[ds]], "sf")) {
    cli_alert_info("  - {ds}: {nrow(shiny_data[[ds]])} features (sf)")
  }
}

cli_alert_success("Shiny data preparation complete!")
