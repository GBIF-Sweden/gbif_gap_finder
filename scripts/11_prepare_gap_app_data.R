# scripts/11_prepare_gap_app_data.R
# ==============================================================================
# Prepare Data for Gap Analysis App
# ==============================================================================
# This script:
# - Reads all outputs from the gap analysis pipeline (scripts 06-10)
# - Combines and optimizes data for fast Shiny app loading
# - Pre-aggregates data for common visualizations
# - Includes threat status data from taxonomic analysis
# - Saves everything as a single .rds file bundle
#
# Run this after: Scripts 01-10 (full pipeline)
# Output: shiny_app/data/shiny_data.rds
#
# The output bundle contains:
# - Grid geometries (simplified for web rendering)
# - Dashboard summary metrics
# - Spatial gap data (cell-level and summaries)
# - Temporal data (trends, seasonality, recency)
# - Taxonomic data (coverage, gaps, threat status, priorities)
# - Order/family summaries
# - Priority lists (including CR/EN species)
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

# Output path - gap analysis app data folder
shiny_output_dir <- here("shiny_app", "gap_app", "data")
if (!dir.exists(shiny_output_dir)) {
  dir.create(shiny_output_dir, recursive = TRUE)
  cli_alert_success("Created directory: {.path {shiny_output_dir}}")
}
shiny_data_path <- here(shiny_output_dir, "shiny_data.rds")

# Initialize data list
shiny_data <- list()

# Helper function for safe file reading
safe_read <- function(path, type = "csv") {
  if (!file.exists(path)) {
    cli_alert_warning("File not found: {.path {basename(path)}}")
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
} else {
  cli_alert_warning("10km grid not found")
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
} else {
  cli_alert_warning("50km grid not found")
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
  shiny_data$spatial_gaps_10km <- as_tibble(spatial_gaps_10)
  cli_alert_success("Spatial gaps 10km: {nrow(shiny_data$spatial_gaps_10km)} rows")
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

# Grid comparison
grid_comparison <- safe_read(here(p_tables, "comparison_grid_resolutions.csv"))
if (!is.null(grid_comparison)) {
  shiny_data$comparison_grids <- as_tibble(grid_comparison)
  cli_alert_success("Grid comparison loaded")
}

# Zero coverage cells
zero_cells <- safe_read(here(p_integrated, "priority_cells_zero_coverage.csv"))
if (is.null(zero_cells) || nrow(zero_cells) == 0) {
  zero_cells <- safe_read(here(p_integrated, "priority_zero_coverage_cells.csv"))
}
if (!is.null(zero_cells) && nrow(zero_cells) > 0) {
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

# Cell recency (10km)
cell_recency <- safe_read(here(p_gaps, "cell_recency_10km.csv"))
if (is.null(cell_recency)) {
  cell_recency <- safe_read(here(p_gaps, "temporal_cell_recency_10km.csv"))
}
if (!is.null(cell_recency)) {
  shiny_data$cell_recency_10km <- as_tibble(cell_recency)
  cli_alert_success("Cell recency 10km: {nrow(shiny_data$cell_recency_10km)} rows")
}

# Stale cells priority
stale_cells <- safe_read(here(p_integrated, "priority_stale_cells.csv"))
if (is.null(stale_cells) || nrow(stale_cells) == 0) {
  stale_cells <- safe_read(here(p_integrated, "priority_cells_stale.csv"))
}
if (!is.null(stale_cells) && nrow(stale_cells) > 0) {
  shiny_data$priority_stale_cells <- as_tibble(stale_cells)
  cli_alert_success("Stale cells: {nrow(shiny_data$priority_stale_cells)}")
}

# ===========================================================================
# 5. TAXONOMIC DATA (including threat status)
# ===========================================================================

cli_h2("Loading Taxonomic Data")

# Taxonomic match summary (main source for coverage analysis)
match_summary <- safe_read(here(p_gaps, "taxonomic_match_summary.csv"))
if (!is.null(match_summary)) {
  shiny_data$taxonomic_match_summary <- as_tibble(match_summary)
  cli_alert_success("Taxonomic match summary: {nrow(shiny_data$taxonomic_match_summary)} taxa")
  
  # Derive threat status coverage if threat columns exist
  threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), 
                          names(match_summary))[1]
  
  if (!is.na(threat_col)) {
    cli_alert_info("Found threat status column: {threat_col}")
    
    # Create threat coverage summary
    threat_coverage <- match_summary |>
      as_tibble() |>
      mutate(threatStatus = .data[[threat_col]]) |>
      filter(!is.na(threatStatus), threatStatus != "", 
             threatStatus %in% c("CR", "EN", "VU", "NT", "LC", "DD")) |>
      group_by(threatStatus) |>
      summarise(
        n_ref_total = n(),
        n_in_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = n_ref_total - n_in_gbif,
        pct_coverage = round(100 * n_in_gbif / n_ref_total, 1),
        .groups = "drop"
      )
    
    shiny_data$tax_by_threat <- threat_coverage
    cli_alert_success("Threat coverage: {nrow(threat_coverage)} categories")
    
    # Extract missing threatened species (CR/EN priority)
    missing_threatened <- match_summary |>
      as_tibble() |>
      mutate(threatStatus = .data[[threat_col]]) |>
      filter(threatStatus %in% c("CR", "EN", "VU", "NT"),
             !matched_any) |>
      select(any_of(c("scientificName", "taxonRank", "threatStatus", 
                      "kingdom", "phylum", "class", "order", "family")))
    
    if (nrow(missing_threatened) > 0) {
      shiny_data$priority_taxa_missing <- missing_threatened
      cli_alert_success("Missing threatened taxa: {nrow(missing_threatened)}")
    }
  }
}

# Try loading pre-computed threat tables if match_summary didn't have threat data
if (is.null(shiny_data$tax_by_threat)) {
  tax_by_threat <- safe_read(here(p_tables, "overview_taxonomic_by_threat.csv"))
  if (!is.null(tax_by_threat)) {
    shiny_data$tax_by_threat <- as_tibble(tax_by_threat)
    cli_alert_success("Taxonomic by threat loaded from pre-computed table")
  }
}

# Coverage by rank
tax_by_rank <- safe_read(here(p_tables, "overview_taxonomic_by_rank.csv"))
if (!is.null(tax_by_rank)) {
  shiny_data$tax_by_rank <- as_tibble(tax_by_rank)
  cli_alert_success("Taxonomic by rank loaded")
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

# Coverage by kingdom (derive from match_summary if available)
if (!is.null(match_summary) && "kingdom" %in% names(match_summary)) {
  kingdom_coverage <- match_summary |>
    as_tibble() |>
    filter(!is.na(kingdom), kingdom != "") |>
    group_by(kingdom) |>
    summarise(
      n_ref_total = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_ref_total - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_ref_total, 1),
      .groups = "drop"
    ) |>
    arrange(desc(n_ref_total))
  
  shiny_data$tax_by_kingdom <- kingdom_coverage
  cli_alert_success("Taxonomic by kingdom: {nrow(kingdom_coverage)} kingdoms")
}

# Coverage by phylum (derive from match_summary)
if (!is.null(match_summary) && "phylum" %in% names(match_summary)) {
  phylum_coverage <- match_summary |>
    as_tibble() |>
    filter(!is.na(phylum), phylum != "") |>
    group_by(kingdom, phylum) |>
    summarise(
      n_ref_total = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_ref_total - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_ref_total, 1),
      .groups = "drop"
    ) |>
    arrange(desc(n_ref_total))
  
  shiny_data$tax_by_phylum <- phylum_coverage
  cli_alert_success("Taxonomic by phylum: {nrow(phylum_coverage)} phyla")
}

# Coverage by class (derive from match_summary)
if (!is.null(match_summary) && "class" %in% names(match_summary)) {
  class_coverage <- match_summary |>
    as_tibble() |>
    filter(!is.na(class), class != "") |>
    group_by(kingdom, phylum, class) |>
    summarise(
      n_ref_total = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_ref_total - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_ref_total, 1),
      .groups = "drop"
    ) |>
    arrange(desc(n_ref_total))
  
  shiny_data$tax_by_class <- class_coverage
  cli_alert_success("Taxonomic by class: {nrow(class_coverage)} classes")
}

# ===========================================================================
# 6. ORDER/FAMILY TEMPORAL TRENDS
# ===========================================================================

cli_h2("Loading Order/Family Summaries")

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
  
  # Recent vs historical change by order
  recent_cutoff <- current_year - 10
  historical_cutoff <- current_year - 20
  
  order_change <- shiny_data$order_temporal |>
    filter(order %in% shiny_data$top_orders$order[1:12]) |>
    mutate(
      era = case_when(
        year >= recent_cutoff ~ "Recent",
        year >= historical_cutoff ~ "Historical",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(era)) |>
    group_by(order, era) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occurrences, values_fill = 0) |>
    filter(Historical > 0) |>
    mutate(
      pct_change = round(100 * (Recent - Historical) / Historical, 1),
      direction = ifelse(pct_change >= 0, "Increased", "Decreased")
    ) |>
    arrange(desc(pct_change))
  
  shiny_data$order_change <- order_change
  cli_alert_success("Order change analysis: {nrow(order_change)} orders")
}

# Family summary
family_summary <- safe_read(here(p_integrated, "family_summary.csv"))
if (!is.null(family_summary)) {
  shiny_data$family_summary <- as_tibble(family_summary)
  cli_alert_success("Family summary: {nrow(shiny_data$family_summary)} families")
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

# Priority taxa - all (if not already derived)
if (is.null(shiny_data$priority_taxa_missing)) {
  priority_taxa_all <- safe_read(here(p_integrated, "priority_taxa_all.csv"))
  if (!is.null(priority_taxa_all)) {
    shiny_data$priority_taxa_all <- as_tibble(priority_taxa_all)
    cli_alert_success("Priority taxa (all): {nrow(shiny_data$priority_taxa_all)}")
  }
}

# ===========================================================================
# 8. DERIVED SUMMARIES (for detailed exploration)
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
  shiny_data$time_summary_10km <- as_tibble(time_summary) |>
    mutate(
      yearmonth_chr = str_trim(as.character(yearmonth)),
      year = as.integer(str_sub(yearmonth_chr, 1, 4)),
      month = as.integer(str_sub(yearmonth_chr, 6, 7))
    )
  cli_alert_success("Time summary 10km: {nrow(shiny_data$time_summary_10km)} rows")
}

# ===========================================================================
# 9. METADATA
# ===========================================================================

cli_h2("Adding Metadata")

# Get dataset names (excluding metadata)
dataset_names <- names(shiny_data)

shiny_data$metadata <- list(
  created_at = Sys.time(),
  created_by = "scripts/11_prepare_gap_app_data.R",
  r_version = R.version.string,
  n_datasets = length(dataset_names),
  datasets = dataset_names,
  
  # Summary counts
  n_cells_10km = if (!is.null(shiny_data$grid_10km)) nrow(shiny_data$grid_10km) else NA,
  n_cells_50km = if (!is.null(shiny_data$grid_50km)) nrow(shiny_data$grid_50km) else NA,
  
  # Data availability flags
  has_spatial = !is.null(shiny_data$spatial_gaps_10km),
  has_temporal = !is.null(shiny_data$temporal_year) || !is.null(shiny_data$time_summary_10km),
  has_taxonomic = !is.null(shiny_data$tax_by_rank) || !is.null(shiny_data$taxonomic_match_summary),
  has_threat_status = !is.null(shiny_data$tax_by_threat),
  has_orders = !is.null(shiny_data$order_temporal),
  has_priorities = !is.null(shiny_data$priority_taxa_missing) || !is.null(shiny_data$priority_taxa_all)
)

# ===========================================================================
# 10. SAVE BUNDLE
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
n_spatial <- sum(str_detect(dataset_names, "spatial|grid|cell|comparison"))
n_temporal <- sum(str_detect(dataset_names, "temporal|recency|time"))
n_taxonomic <- sum(str_detect(dataset_names, "tax|order|family|threatened|kingdom"))
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
cli_alert_info("")
cli_alert_info("Key features:")
cli_alert_info("  - Threat status data: {shiny_data$metadata$has_threat_status}")
cli_alert_info("  - Priority taxa: {shiny_data$metadata$has_priorities}")
cli_alert_info("  - Temporal trends: {shiny_data$metadata$has_temporal}")
