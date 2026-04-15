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

# p_gaps, p_tables, p_integrated, p_derived are defined in R/globals.R

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

# Administrative boundaries (optional — from GADM via 00b script)
admin_dir <- here(raw_admin_dir)
for (lvl in c(1, 2)) {
  admin_path <- here(admin_dir, paste0("admin_level", lvl, ".gpkg"))
  if (file.exists(admin_path)) {
    admin_sf <- tryCatch(st_read(admin_path, quiet = TRUE), error = function(e) NULL)
    if (!is.null(admin_sf)) {
      if (st_crs(admin_sf)$epsg != 4326) admin_sf <- st_transform(admin_sf, 4326)
      shiny_data[[paste0("admin_level", lvl)]] <- admin_sf
      cli_alert_success("Admin level {lvl}: {nrow(admin_sf)} units")
    }
  }
}

# Cell → admin region spatial join (for regional filtering in explorer app)
if (!is.null(shiny_data$grid_10km) && !is.null(shiny_data$admin_level1)) {
  cli_alert_info("Computing cell → admin region mapping...")
  # Use centroids of grid cells for fast point-in-polygon join
  grid_centroids <- suppressWarnings(
    st_centroid(shiny_data$grid_10km, of_largest_polygon = TRUE)
  )
  cell_admin <- st_join(grid_centroids, shiny_data$admin_level1, join = st_within) |>
    st_drop_geometry() |>
    as_tibble() |>
    select(eeacellcode, admin_name_level1 = admin_name) |>
    distinct(eeacellcode, .keep_all = TRUE)

  # Also join level 2 if available
  if (!is.null(shiny_data$admin_level2)) {
    cell_admin2 <- st_join(grid_centroids, shiny_data$admin_level2, join = st_within) |>
      st_drop_geometry() |>
      as_tibble() |>
      select(eeacellcode, admin_name_level2 = admin_name) |>
      distinct(eeacellcode, .keep_all = TRUE)
    cell_admin <- cell_admin |> left_join(cell_admin2, by = "eeacellcode")
  }

  shiny_data$cell_admin_lookup <- cell_admin
  cli_alert_success("Cell-admin lookup: {nrow(cell_admin)} cells mapped to admin regions")
  cli_alert_info("  Level 1 regions: {n_distinct(cell_admin$admin_name_level1, na.rm = TRUE)}")
  if ("admin_name_level2" %in% names(cell_admin)) {
    cli_alert_info("  Level 2 units: {n_distinct(cell_admin$admin_name_level2, na.rm = TRUE)}")
  }
  rm(grid_centroids)
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
stale_cells <- safe_read(here(p_integrated, "priority_cells_stale.csv"))
if (is.null(stale_cells) || nrow(stale_cells) == 0) {
  stale_cells <- safe_read(here(p_integrated, "priority_stale_cells.csv"))
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

# Coverage by establishment means (for scope filter)
if (!is.null(match_summary) && "establishmentMeans" %in% names(match_summary)) {
  estab_coverage <- match_summary |>
    as_tibble() |>
    group_by(establishmentMeans) |>
    summarise(
      n_ref_total = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_ref_total - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_ref_total, 2),
      .groups = "drop"
    ) |>
    arrange(desc(n_ref_total))
  shiny_data$tax_by_establishment <- estab_coverage
  cli_alert_success("Coverage by establishment means: {nrow(estab_coverage)} categories")
}

# Coverage by occurrence status (for scope filter)
if (!is.null(match_summary) && "occurrenceStatus" %in% names(match_summary)) {
  occ_status_coverage <- match_summary |>
    as_tibble() |>
    group_by(occurrenceStatus) |>
    summarise(
      n_ref_total = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_ref_total - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_ref_total, 2),
      .groups = "drop"
    ) |>
    arrange(desc(n_ref_total))
  shiny_data$tax_by_occurrence_status <- occ_status_coverage
  cli_alert_success("Coverage by occurrence status: {nrow(occ_status_coverage)} categories")
}

# Coverage by invasive status (for scope filter)
if (!is.null(match_summary) && "is_invasive" %in% names(match_summary)) {
  invasive_coverage <- match_summary |>
    as_tibble() |>
    group_by(is_invasive) |>
    summarise(
      n_ref_total = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_ref_total - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_ref_total, 2),
      .groups = "drop"
    )
  shiny_data$tax_by_invasive <- invasive_coverage
  n_inv <- sum(match_summary$is_invasive, na.rm = TRUE)
  cli_alert_success("Invasive species in backbone: {scales::comma(n_inv)}")
} else {
  cli_alert_info("No is_invasive column in match_summary")
}

# Dyntaxa scope summary (for the global toggle)
if (!is.null(match_summary) && "in_dyntaxa" %in% names(match_summary)) {
  dyntaxa_coverage <- match_summary |>
    as_tibble() |>
    group_by(in_dyntaxa) |>
    summarise(
      n_taxa = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      .groups = "drop"
    )
  shiny_data$dyntaxa_scope <- dyntaxa_coverage
  n_dyntaxa <- sum(match_summary$in_dyntaxa, na.rm = TRUE)
  n_all <- nrow(match_summary)
  cli_alert_success("Dyntaxa scope: {scales::comma(n_dyntaxa)} in Dyntaxa / {scales::comma(n_all)} total")
} else {
  cli_alert_info("No in_dyntaxa column in match_summary — toggle will not be available")
}

# Gaps by order — derive from match_summary to ensure hierarchy columns
# (Pre-computed CSVs may lack kingdom/phylum/class needed for cascading filters)
tax_by_order_file <- safe_read(here(p_tables, "overview_taxonomic_gaps_by_order.csv"))
if (!is.null(match_summary) && all(c("kingdom", "phylum", "class", "order") %in% names(match_summary))) {
  cli_alert_info("Deriving tax_by_order from match_summary (with hierarchy columns)")
  order_coverage <- match_summary |>
    as_tibble() |>
    filter(!is.na(order), order != "") |>
    group_by(kingdom, phylum, class, order) |>
    summarise(
      n_taxa = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_taxa - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_taxa, 1),
      .groups = "drop"
    ) |>
    arrange(desc(n_taxa))
  shiny_data$tax_by_order <- order_coverage
  cli_alert_success("Taxonomic by order: {nrow(order_coverage)} orders (with kingdom/phylum/class)")
} else if (!is.null(tax_by_order_file)) {
  # Fallback to pre-computed CSV
  shiny_data$tax_by_order <- as_tibble(tax_by_order_file)
  cli_alert_success("Taxonomic by order: {nrow(shiny_data$tax_by_order)} orders (from file)")
}

# Gaps by family — derive from match_summary to ensure hierarchy columns
tax_by_family_file <- safe_read(here(p_tables, "overview_taxonomic_gaps_by_family.csv"))
if (!is.null(match_summary) && all(c("kingdom", "phylum", "class", "order", "family") %in% names(match_summary))) {
  cli_alert_info("Deriving tax_by_family from match_summary (with hierarchy columns)")
  family_coverage <- match_summary |>
    as_tibble() |>
    filter(!is.na(family), family != "") |>
    group_by(kingdom, phylum, class, order, family) |>
    summarise(
      n_taxa = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      n_missing = n_taxa - n_in_gbif,
      pct_coverage = round(100 * n_in_gbif / n_taxa, 1),
      .groups = "drop"
    ) |>
    arrange(desc(n_taxa))
  shiny_data$tax_by_family <- family_coverage
  cli_alert_success("Taxonomic by family: {nrow(family_coverage)} families (with kingdom/phylum/class/order)")
} else if (!is.null(tax_by_family_file)) {
  shiny_data$tax_by_family <- as_tibble(tax_by_family_file)
  cli_alert_success("Taxonomic by family: {nrow(shiny_data$tax_by_family)} families (from file)")
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
# 5b. RECONCILIATION TABLE (specieskey → Dyntaxa mapping)
# ===========================================================================
# Load the full reconciliation table for the Dyntaxa/All GBIF toggle.
# This lets the app filter all occurrence data by in_dyntaxa at runtime.

cli_h2("Loading Reconciliation Table")

recon_path <- here(p_data_proc, "taxonomic_reconciliation.rds")
if (file.exists(recon_path)) {
  recon_full <- as_tibble(readRDS(recon_path))

  # Create a lightweight lookup: specieskey → in_dyntaxa, is_invasive, is_sensitive, kingdom
  recon_cols <- intersect(
    c("specieskey", "in_dyntaxa", "is_invasive", "is_sensitive", "kingdom", "phylum",
      "class", "order", "family", "match_tier", "establishmentMeans"),
    names(recon_full)
  )

  shiny_data$species_scope_lookup <- recon_full |>
    select(all_of(recon_cols)) |>
    mutate(
      in_dyntaxa = replace_na(in_dyntaxa, FALSE),
      is_invasive = replace_na(is_invasive, FALSE),
      is_sensitive = if ("is_sensitive" %in% names(pick(everything())))
        replace_na(is_sensitive, FALSE) else FALSE
    )
  cli_alert_success(
    "Species scope lookup: {scales::comma(nrow(shiny_data$species_scope_lookup))} species"
  )
  cli_alert_info(
    "  in_dyntaxa: {scales::comma(sum(shiny_data$species_scope_lookup$in_dyntaxa))}"
  )
  cli_alert_info(
    "  is_invasive: {scales::comma(sum(shiny_data$species_scope_lookup$is_invasive))}"
  )
  if ("is_sensitive" %in% names(shiny_data$species_scope_lookup)) {
    cli_alert_info(
      "  is_sensitive: {scales::comma(sum(shiny_data$species_scope_lookup$is_sensitive))}"
    )
  }
  rm(recon_full); gc()
} else {
  cli_alert_warning("Reconciliation table not found — Dyntaxa toggle not available")
}

# ===========================================================================
# 5c. TAXONOMIC CELL RECENCY (for spatial tab taxonomic filter)
# ===========================================================================
# Build a per-class recency map so users can filter by kingdom, class, or
# exclude specific groups (e.g. Aves). Grouped by eeacellcode × kingdom × class.
# Uses the parquet cube directly.

cli_h2("Computing Taxonomic Cell Recency (Kingdom × Class)")

cube_10km_parquet <- here(p_data_proc, "cubes", "cube_10km.parquet")
if (file.exists(cube_10km_parquet) && requireNamespace("arrow", quietly = TRUE)) {
  tax_cell_dt <- as.data.table(
    arrow::open_dataset(cube_10km_parquet) |>
      dplyr::filter(!is.na(kingdom), kingdom != "") |>
      dplyr::select(eeacellcode, kingdom, class, year, month, occurrences) |>
      dplyr::collect()
  )

  # Clean class column
  tax_cell_dt[is.na(class) | class == "", class := "Unplaced"]

  # Coerce year/month to numeric upfront to avoid type inconsistencies
  tax_cell_dt[, `:=`(
    year_num  = as.numeric(year),
    month_num = as.numeric(month),
    occ_num   = as.numeric(occurrences)
  )]
  tax_cell_dt[, yearmonth_num := fifelse(
    !is.na(year_num) & !is.na(month_num),
    year_num * 100 + month_num,
    NA_real_
  )]

  # Compute recency per kingdom × class × cell
  tax_cell_recency <- tax_cell_dt[, .(
    total_occ     = sum(occ_num, na.rm = TRUE),
    max_year      = ifelse(all(is.na(year_num)), NA_real_, max(year_num, na.rm = TRUE)),
    max_yearmonth = ifelse(all(is.na(yearmonth_num)), NA_real_, max(yearmonth_num, na.rm = TRUE))
  ), by = .(eeacellcode, kingdom, class)]

  # Compute staleness in months from latest data point
  global_max_ym <- max(tax_cell_dt$yearmonth_num, na.rm = TRUE)
  if (!is.infinite(global_max_ym) && !is.na(global_max_ym)) {
    latest_date <- as.Date(paste0(
      substr(as.character(as.integer(global_max_ym)), 1, 4), "-",
      substr(as.character(as.integer(global_max_ym)), 5, 6), "-01"
    ))
    tax_cell_recency[, staleness_months := {
      cell_ym <- as.integer(max_yearmonth)
      cell_date <- as.Date(paste0(
        substr(as.character(cell_ym), 1, 4), "-",
        substr(as.character(cell_ym), 5, 6), "-01"
      ))
      as.numeric(difftime(latest_date, cell_date, units = "days")) / 30.44
    }]
    tax_cell_recency[is.na(max_yearmonth), staleness_months := NA_real_]
  }

  shiny_data$tax_cell_recency <- as_tibble(tax_cell_recency)
  kingdoms_found <- unique(tax_cell_recency$kingdom)
  classes_found <- length(unique(tax_cell_recency$class))
  cli_alert_success(
    "Taxonomic cell recency: {scales::comma(nrow(tax_cell_recency))} rows, {length(kingdoms_found)} kingdoms, {classes_found} classes"
  )
  cli_alert_info("  Kingdoms: {paste(kingdoms_found, collapse = ', ')}")

  # Also create a kingdom-only aggregate (for backward compat / kingdom-only filter)
  shiny_data$kingdom_cell_recency <- tax_cell_recency |>
    as_tibble() |>
    group_by(eeacellcode, kingdom) |>
    summarise(
      total_occ = sum(total_occ, na.rm = TRUE),
      max_year = ifelse(all(is.na(max_year)), NA_real_, max(max_year, na.rm = TRUE)),
      max_yearmonth = ifelse(all(is.na(max_yearmonth)), NA_real_, max(max_yearmonth, na.rm = TRUE)),
      staleness_months = ifelse(all(is.na(staleness_months)), NA_real_, min(staleness_months, na.rm = TRUE)),
      .groups = "drop"
    )
  cli_alert_success("Kingdom cell recency (aggregate): {scales::comma(nrow(shiny_data$kingdom_cell_recency))} rows")

  rm(tax_cell_dt, tax_cell_recency); gc()
} else {
  cli_alert_info("Parquet cube not found — kingdom cell recency not computed")
}

# ===========================================================================
# 5d. SCOPE-FILTERED SUMMARIES + LAST-12-MONTHS SPLITS
# ===========================================================================
# Compute parallel summary sets for Dyntaxa and All GBIF scopes, each with
# last-12-months observed/published splits. All computed from a single pass
# over the parquet cube. This section is self-contained — it determines the
# recent period cutoff from the data before computing anything.

cli_h2("Computing Scope-Filtered Summaries (Dyntaxa + All GBIF)")

cube_10km_parquet <- here(p_data_proc, "cubes", "cube_10km.parquet")
recon_path <- here(p_data_proc, "taxonomic_reconciliation.rds")

if (file.exists(cube_10km_parquet) && file.exists(recon_path) &&
    requireNamespace("arrow", quietly = TRUE)) {

  # Load Dyntaxa specieskeys
  recon_dt <- as.data.table(readRDS(recon_path))
  if ("in_dyntaxa" %in% names(recon_dt)) {
    dyntaxa_keys <- recon_dt[in_dyntaxa == TRUE, unique(specieskey)]
    cli_alert_info("Dyntaxa specieskeys: {scales::comma(length(dyntaxa_keys))}")
  } else {
    dyntaxa_keys <- recon_dt[match_tier != "unmatched", unique(specieskey)]
    cli_alert_info("Dyntaxa specieskeys (from match_tier): {scales::comma(length(dyntaxa_keys))}")
  }
  rm(recon_dt); gc()

  # Read parquet cube — single read, used for all computations below
  cube_dt <- as.data.table(
    arrow::open_dataset(cube_10km_parquet) |>
      dplyr::select(specieskey, eeacellcode, kingdom, class, order, family,
                    basisofrecord, year, month,
                    year_published, month_published, occurrences) |>
      dplyr::collect()
  )
  cli_alert_info("Cube loaded: {scales::comma(nrow(cube_dt))} rows")

  # Coerce types
  cube_dt[, `:=`(
    year_num  = as.numeric(year),
    month_num = as.numeric(month),
    occ_num   = as.numeric(occurrences)
  )]
  cube_dt[, yearmonth := fifelse(
    !is.na(year_num) & !is.na(month_num),
    as.integer(year_num) * 100L + as.integer(month_num),
    NA_integer_
  )]
  cube_dt[, yearmonth_pub := fifelse(
    !is.na(year_published) & !is.na(month_published) &
      as.integer(year_published) >= 1900 & as.integer(year_published) <= year(Sys.Date()) + 1 &
      as.integer(month_published) >= 1 & as.integer(month_published) <= 12,
    as.integer(year_published) * 100L + as.integer(month_published),
    NA_integer_
  )]

  # Diagnostic: check if year_published is actually lastinterpreted (always recent)
  if (sum(!is.na(cube_dt$yearmonth_pub)) > 0) {
    pub_ym_range <- range(cube_dt$yearmonth_pub, na.rm = TRUE)
    pct_recent <- round(100 * sum(cube_dt$yearmonth_pub >= 202400L, na.rm = TRUE) /
                        sum(!is.na(cube_dt$yearmonth_pub)), 1)
    cli_alert_info("year_published range: {pub_ym_range[1]} to {pub_ym_range[2]}")
    cli_alert_info("Records with year_published >= 2024: {pct_recent}%")
    if (pct_recent > 95) {
      cli_alert_warning(
        "year_published appears to be lastinterpreted (GBIF processing date), not original publication date. ",
        "Published-in-last-12-months splits may not be meaningful."
      )
    }
  }
  cube_dt[is.na(order)  | order  == "", order  := "Unplaced"]
  cube_dt[is.na(family) | family == "", family := "Unplaced"]

  # --- Determine recent period cutoff (last 12 months of data) ---
  all_ym <- sort(unique(cube_dt$yearmonth[!is.na(cube_dt$yearmonth)]))
  if (length(all_ym) > 0) {
    latest_ym <- max(all_ym)
    latest_year <- as.integer(substr(as.character(latest_ym), 1, 4))
    latest_month <- as.integer(substr(as.character(latest_ym), 5, 6))
    cutoff_date <- as.Date(paste0(latest_year, "-", sprintf("%02d", latest_month), "-01")) %m-% months(11)
    recent_cutoff_ym <- as.integer(format(cutoff_date, "%Y%m"))
    recent_label <- paste0(
      format(cutoff_date, "%b %Y"), " \u2013 ",
      format(as.Date(paste0(latest_year, "-", sprintf("%02d", latest_month), "-01")), "%b %Y")
    )
  } else {
    recent_cutoff_ym <- as.integer(paste0(year(Sys.Date()) - 1, "01"))
    recent_label <- paste0(year(Sys.Date()) - 1)
  }
  cli_alert_info("Recent period: {recent_label} (cutoff: {recent_cutoff_ym})")

  # Store for later use in sections 8b+
  shiny_data$last_year <- recent_cutoff_ym
  shiny_data$recent_label <- recent_label

  # --- Split cube into Dyntaxa and All GBIF ---
  dyn_dt <- cube_dt[specieskey %in% dyntaxa_keys]
  cli_alert_info("Dyntaxa: {scales::comma(nrow(dyn_dt))} / {scales::comma(nrow(cube_dt))} rows ({round(100 * nrow(dyn_dt) / nrow(cube_dt), 1)}%)")

  # === Helper: compute all summaries for a given data.table ===
  compute_scope_summaries <- function(dt, scope_label) {
    cli_h3("Computing summaries: {scope_label}")

    results <- list()

    # 1. Time summary (per-basis + "all")
    ts_basis <- dt[, .(
      occurrences = sum(occ_num, na.rm = TRUE),
      n_species   = as.numeric(uniqueN(specieskey)),
      n_cells     = as.numeric(uniqueN(eeacellcode))
    ), by = .(basisofrecord, yearmonth)]

    ts_all <- dt[, .(
      occurrences = sum(occ_num, na.rm = TRUE),
      n_species   = as.numeric(uniqueN(specieskey)),
      n_cells     = as.numeric(uniqueN(eeacellcode))
    ), by = .(yearmonth)]
    ts_all[, basisofrecord := "all"]

    ts <- rbindlist(list(ts_basis, ts_all), use.names = TRUE, fill = TRUE)
    ts[, `:=`(
      year  = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )]
    results$time_summary <- as_tibble(ts)
    cli_alert_success("{scope_label} time summary: {scales::comma(nrow(ts))} rows ({uniqueN(ts$basisofrecord)} basis types)")

    # 2. Order x time summary
    ots <- dt[, .(
      occurrences = sum(occ_num, na.rm = TRUE),
      n_species   = as.numeric(uniqueN(specieskey)),
      n_cells     = as.numeric(uniqueN(eeacellcode))
    ), by = .(order, yearmonth)]
    ots[, basisofrecord := "all"]
    ots[, `:=`(
      year  = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )]
    results$order_time_summary <- as_tibble(ots)
    cli_alert_success("{scope_label} order time summary: {scales::comma(nrow(ots))} rows")

    # 3. Family x time summary
    fts <- dt[, .(
      occurrences = sum(occ_num, na.rm = TRUE),
      n_species   = as.numeric(uniqueN(specieskey)),
      n_cells     = as.numeric(uniqueN(eeacellcode))
    ), by = .(order, family, yearmonth)]
    fts[, basisofrecord := "all"]
    fts[, `:=`(
      year  = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )]
    results$family_time_summary <- as_tibble(fts)
    cli_alert_success("{scope_label} family time summary: {scales::comma(nrow(fts))} rows")

    # 4. Cell summary
    cs <- dt[, .(
      occurrences = sum(occ_num, na.rm = TRUE),
      n_species   = as.numeric(uniqueN(specieskey))
    ), by = .(eeacellcode)]
    cs[, basisofrecord := "all"]
    results$cell_summary <- as_tibble(cs)
    cli_alert_success("{scope_label} cell summary: {scales::comma(nrow(cs))} cells")

    # 5. Cell recency
    global_max <- max(dt$yearmonth, na.rm = TRUE)
    if (!is.na(global_max) && !is.infinite(global_max)) {
      latest_date <- as.Date(paste0(
        substr(as.character(global_max), 1, 4), "-",
        substr(as.character(global_max), 5, 6), "-01"
      ))
      cr <- dt[, .(
        max_yearmonth = ifelse(all(is.na(yearmonth)), NA_integer_, max(yearmonth, na.rm = TRUE))
      ), by = .(eeacellcode)]
      cr[, staleness_months := {
        cell_d <- as.Date(paste0(
          substr(as.character(max_yearmonth), 1, 4), "-",
          substr(as.character(max_yearmonth), 5, 6), "-01"
        ))
        as.numeric(difftime(latest_date, cell_d, units = "days")) / 30.44
      }]
      cr[is.na(max_yearmonth), staleness_months := NA_real_]
    } else {
      cr <- dt[, .(max_yearmonth = NA_integer_, staleness_months = NA_real_), by = .(eeacellcode)]
    }
    cr[, basisofrecord := "all"]
    results$cell_recency <- as_tibble(cr)
    cli_alert_success("{scope_label} cell recency: {scales::comma(nrow(cr))} cells")

    # 6. Spatial gaps (per-basis + "all") — zero-filled to match grid
    # Get all grid cells from the loaded grid (must exist in shiny_data)
    all_cells <- if (!is.null(shiny_data$grid_10km)) {
      as.character(shiny_data$grid_10km$eeacellcode)
    } else {
      unique(dt$eeacellcode)
    }
    all_basis <- unique(dt$basisofrecord)

    # Per-basis: aggregate, then zero-fill
    sg_basis <- dt[, .(
      occurrences = sum(occ_num, na.rm = TRUE),
      n_species   = as.numeric(uniqueN(specieskey))
    ), by = .(eeacellcode, basisofrecord)]

    # Create complete grid: every cell × every basis type
    complete_grid <- as.data.table(expand.grid(
      eeacellcode = all_cells,
      basisofrecord = all_basis,
      stringsAsFactors = FALSE
    ))
    sg_basis <- merge(complete_grid, sg_basis, by = c("eeacellcode", "basisofrecord"), all.x = TRUE)
    sg_basis[is.na(occurrences), occurrences := 0]
    sg_basis[is.na(n_species), n_species := 0]
    sg_basis[, has_data := occurrences > 0]

    # "all" basis: aggregate across basis types per cell, zero-fill
    sg_all <- dt[, .(
      occurrences = sum(occ_num, na.rm = TRUE),
      n_species   = as.numeric(uniqueN(specieskey))
    ), by = .(eeacellcode)]
    sg_all_complete <- data.table(eeacellcode = all_cells)
    sg_all <- merge(sg_all_complete, sg_all, by = "eeacellcode", all.x = TRUE)
    sg_all[is.na(occurrences), occurrences := 0]
    sg_all[is.na(n_species), n_species := 0]
    sg_all[, `:=`(basisofrecord = "all", has_data = occurrences > 0)]

    results$spatial_gaps <- as_tibble(
      rbindlist(list(sg_basis, sg_all), use.names = TRUE, fill = TRUE)
    )
    cli_alert_success("{scope_label} spatial gaps: {scales::comma(nrow(results$spatial_gaps))} rows ({length(all_basis) + 1} basis types, {length(all_cells)} cells)")

    # 7. Basis recent splits (observed + published last 12 months)
    br <- dt[, .(
      occ_total     = sum(occ_num, na.rm = TRUE),
      occ_last_year = sum(occ_num[!is.na(yearmonth) & yearmonth >= recent_cutoff_ym], na.rm = TRUE),
      occ_prior     = sum(occ_num[is.na(yearmonth) | yearmonth < recent_cutoff_ym], na.rm = TRUE),
      pub_last_year = sum(occ_num[!is.na(yearmonth_pub) & yearmonth_pub >= recent_cutoff_ym], na.rm = TRUE),
      pub_prior     = sum(occ_num[is.na(yearmonth_pub) | yearmonth_pub < recent_cutoff_ym], na.rm = TRUE),
      n_species     = as.numeric(uniqueN(specieskey))
    ), by = .(basisofrecord)]

    br_all <- dt[, .(
      occ_total     = sum(occ_num, na.rm = TRUE),
      occ_last_year = sum(occ_num[!is.na(yearmonth) & yearmonth >= recent_cutoff_ym], na.rm = TRUE),
      occ_prior     = sum(occ_num[is.na(yearmonth) | yearmonth < recent_cutoff_ym], na.rm = TRUE),
      pub_last_year = sum(occ_num[!is.na(yearmonth_pub) & yearmonth_pub >= recent_cutoff_ym], na.rm = TRUE),
      pub_prior     = sum(occ_num[is.na(yearmonth_pub) | yearmonth_pub < recent_cutoff_ym], na.rm = TRUE),
      n_species     = as.numeric(uniqueN(specieskey))
    )]
    br_all[, basisofrecord := "all"]

    results$basis_recent <- as_tibble(
      rbindlist(list(br, br_all), use.names = TRUE)
    )
    cli_alert_success("{scope_label} basis recent: {nrow(results$basis_recent)} basis types")

    results
  }

  # === Compute for both scopes ===
  dyntaxa_results <- compute_scope_summaries(dyn_dt, "Dyntaxa")
  all_results     <- compute_scope_summaries(cube_dt, "All GBIF")

  # Store Dyntaxa versions
  shiny_data$dyntaxa_time_summary        <- dyntaxa_results$time_summary
  shiny_data$dyntaxa_order_time_summary  <- dyntaxa_results$order_time_summary
  shiny_data$dyntaxa_family_time_summary <- dyntaxa_results$family_time_summary
  shiny_data$dyntaxa_cell_summary        <- dyntaxa_results$cell_summary
  shiny_data$dyntaxa_cell_recency        <- dyntaxa_results$cell_recency
  shiny_data$dyntaxa_spatial_gaps        <- dyntaxa_results$spatial_gaps
  shiny_data$dyntaxa_basis_recent        <- dyntaxa_results$basis_recent

  # Store All GBIF versions
  shiny_data$all_basis_recent            <- all_results$basis_recent

  # Clean up cube
  rm(cube_dt, dyn_dt, dyntaxa_results, all_results); gc()

} else {
  cli_alert_info("Parquet cube or reconciliation not found — scope-filtered summaries not computed")
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
      yearmonth = as.integer(gsub("-", "", as.character(yearmonth))),
      year = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )
  cli_alert_success("Time summary 10km: {nrow(shiny_data$time_summary_10km)} rows")
}

# Order × time summary (for taxonomy-filtered temporal charts)
order_time_summary <- safe_read(here(p_derived, "order_time_summary_10km.csv"))
if (!is.null(order_time_summary)) {
  shiny_data$order_time_summary <- as_tibble(order_time_summary) |>
    mutate(
      yearmonth = as.integer(gsub("-", "", as.character(yearmonth))),
      year = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )
  cli_alert_success("Order time summary 10km: {nrow(shiny_data$order_time_summary)} rows")
}

# Family × time summary (for family-level temporal filtering)
family_time_summary <- safe_read(here(p_derived, "family_time_summary_10km.csv"))
if (!is.null(family_time_summary)) {
  shiny_data$family_time_summary <- as_tibble(family_time_summary) |>
    mutate(
      yearmonth = as.integer(gsub("-", "", as.character(yearmonth))),
      year = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )
  cli_alert_success("Family time summary 10km: {nrow(shiny_data$family_time_summary)} rows")
}

# ===========================================================================
# 8b. "LAST YEAR" DATA LAYER (2025 vs prior)
# ===========================================================================
# Adds temporal splits across spatial, taxonomic, and overview data.
# Used for: Troudet bias figure, spatial overlay, overview stats,
#           taxonomic bar stacking, priorities "resolved" view.
# The reference period is the last 12 months of data.
# We use yearmonth (YYYYMM) to define the cutoff precisely.

cli_h2("Computing Last 12 Months (Recent Delta) Data")

# recent_cutoff_ym and recent_label may already be set by section 5d.
# Only recompute if they weren't (e.g. if parquet cube was not available).
if (!exists("recent_cutoff_ym") || is.null(recent_cutoff_ym)) {
  if (!is.null(shiny_data$time_summary_10km)) {
    ts_all <- shiny_data$time_summary_10km |> filter(basisofrecord == "all")

    if ("yearmonth" %in% names(ts_all)) {
      all_ym <- sort(unique(as.integer(gsub("-", "", ts_all$yearmonth))))
    } else {
      all_ym <- sort(unique(as.integer(paste0(ts_all$year, sprintf("%02d", ts_all$month)))))
    }

    if (length(all_ym) > 0) {
      latest_ym <- max(all_ym)
      latest_year <- as.integer(substr(as.character(latest_ym), 1, 4))
      latest_month <- as.integer(substr(as.character(latest_ym), 5, 6))
      cutoff_date <- as.Date(paste0(latest_year, "-", sprintf("%02d", latest_month), "-01")) %m-% months(11)
      recent_cutoff_ym <- as.integer(format(cutoff_date, "%Y%m"))
      recent_label <- paste0(
        format(cutoff_date, "%b %Y"), " \u2013 ",
        format(as.Date(paste0(latest_year, "-", sprintf("%02d", latest_month), "-01")), "%b %Y")
      )
    }
  }

  if (!exists("recent_cutoff_ym") || is.null(recent_cutoff_ym)) {
    recent_cutoff_ym <- as.integer(paste0(year(Sys.Date()) - 1, "01"))
    recent_label <- paste0(year(Sys.Date()) - 1)
    cli_alert_warning("Could not determine recent period from data, defaulting to {recent_label}")
  }

  shiny_data$last_year <- recent_cutoff_ym
  shiny_data$recent_label <- recent_label
} else {
  # Already computed in section 5d
  recent_label <- shiny_data$recent_label %||% as.character(recent_cutoff_ym)
}

cli_alert_info("Recent period: {recent_label} (yearmonth cutoff: {recent_cutoff_ym})")

# ---- 8b.1  TAXONOMIC: Recent period splits for order/family/class/kingdom ----
# Uses yearmonth cutoff for precise 12-month window.

if (!is.null(shiny_data$time_summary_10km)) {
  ts <- shiny_data$time_summary_10km |> filter(basisofrecord == "all")

  # Ensure yearmonth column
  if (!"yearmonth" %in% names(ts)) {
    ts <- ts |> mutate(yearmonth = as.integer(paste0(year, sprintf("%02d", month))))
  } else {
    ts <- ts |> mutate(yearmonth = as.integer(gsub("-", "", yearmonth)))
  }

  # Total occurrences by year — used for overview stats
  yearly_totals <- ts |>
    group_by(year) |>
    summarise(
      total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
      n_cells = sum(as.numeric(n_cells), na.rm = TRUE),
      .groups = "drop"
    )
  shiny_data$yearly_totals <- yearly_totals

  # Overview: recent vs prior using yearmonth cutoff
  recent_occ <- ts |>
    filter(yearmonth >= recent_cutoff_ym) |>
    summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
              n_cells = sum(as.numeric(n_cells), na.rm = TRUE))
  prior_occ <- ts |>
    filter(yearmonth < recent_cutoff_ym) |>
    summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE))

  shiny_data$overview_last_year <- list(
    label = recent_label,
    cutoff_ym = recent_cutoff_ym,
    occ_last_year = recent_occ$total_occ[1],
    occ_prior = prior_occ$total_occ[1],
    cells_active_last_year = recent_occ$n_cells[1]
  )
  cli_alert_success("Overview recent: {scales::comma(recent_occ$total_occ[1])} occurrences in {recent_label}")
}

# ---- 8b.2  SPATIAL: Cells newly covered in last 12 months ----
cell_time_path <- here(p_derived, "cell_time_summary_10km.csv")
cell_time_raw <- safe_read(cell_time_path)

if (!is.null(cell_time_raw)) {
  cts <- cell_time_raw |>
    as_tibble() |>
    filter(basisofrecord == "all") |>
    mutate(yearmonth = as.integer(gsub("-", "", yearmonth)))

  cell_by_era <- cts |>
    mutate(era = ifelse(yearmonth >= recent_cutoff_ym, "last_year", "prior")) |>
    group_by(eeacellcode, era) |>
    summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occ, values_fill = 0)

  # Ensure both columns exist
  if (!"prior" %in% names(cell_by_era)) cell_by_era$prior <- 0
  if (!"last_year" %in% names(cell_by_era)) cell_by_era$last_year <- 0

  cell_by_era <- cell_by_era |>
    mutate(
      newly_covered = prior == 0 & last_year > 0,
      has_last_year_data = last_year > 0
    )

  shiny_data$cell_last_year <- cell_by_era
  n_newly <- sum(cell_by_era$newly_covered)
  n_active <- sum(cell_by_era$has_last_year_data)
  cli_alert_success("Spatial recent: {scales::comma(n_newly)} newly covered cells, {scales::comma(n_active)} cells active in {recent_label}")

  # Add to overview
  if (!is.null(shiny_data$overview_last_year)) {
    shiny_data$overview_last_year$cells_newly_covered <- n_newly
  }

  rm(cts, cell_time_raw)
  invisible(gc())
} else {
  cli_alert_warning("cell_time_summary_10km.csv not found — spatial recent data not computed")
}

# ---- 8b.2b  SPATIAL: Cells with data PUBLISHED to GBIF in last 12 months ----
cube_10km_parquet <- here(p_data_proc, "cubes", "cube_10km.parquet")
if (file.exists(cube_10km_parquet) && requireNamespace("arrow", quietly = TRUE) && !is.null(recent_cutoff_ym)) {
  cli_alert_info("Computing cell-level published data from parquet...")
  pub_cell_dt <- as.data.table(
    arrow::open_dataset(cube_10km_parquet) |>
      dplyr::select(eeacellcode, year_published, month_published, occurrences) |>
      dplyr::collect()
  )
  pub_cell_dt[, yearmonth_pub := as.integer(year_published) * 100L + as.integer(month_published)]

  cell_pub <- pub_cell_dt[, .(
    pub_last_year = sum(as.numeric(occurrences[!is.na(yearmonth_pub) & yearmonth_pub >= recent_cutoff_ym]), na.rm = TRUE),
    pub_prior = sum(as.numeric(occurrences[is.na(yearmonth_pub) | yearmonth_pub < recent_cutoff_ym]), na.rm = TRUE)
  ), by = eeacellcode]

  shiny_data$cell_published_last_year <- as_tibble(cell_pub)
  n_pub_active <- sum(cell_pub$pub_last_year > 0)
  cli_alert_success("Cells with published data in recent period: {scales::comma(n_pub_active)}")
  rm(pub_cell_dt, cell_pub); gc()
} else {
  cli_alert_info("Parquet cube not found — cell-level published data not computed")
}

# ---- 8b.3  PRIORITIES: Resolved cells (were zero, now have data) ----
if (!is.null(shiny_data$cell_last_year) && !is.null(shiny_data$priority_zero_cells)) {
  zero_codes <- shiny_data$priority_zero_cells$eeacellcode
  resolved <- shiny_data$cell_last_year |>
    filter(eeacellcode %in% zero_codes, last_year > 0)
  shiny_data$priority_resolved_last_year <- resolved
  cli_alert_success("Priorities: {nrow(resolved)} formerly-zero cells got data in {recent_label}")

  if (!is.null(shiny_data$overview_last_year)) {
    shiny_data$overview_last_year$cells_resolved <- nrow(resolved)
  }
}

# ---- 8b.4  TROUDET-STYLE BIAS DATA ----
# For each taxonomic class: known species richness (from backbone/Dyntaxa)
# vs. GBIF occurrence count, split by prior/last_year.
# The "ideal" line is proportional sampling: if class X has p% of all known
# species, it should have p% of all occurrences.

cli_h2("Computing Troudet-style Taxonomic Bias Data")

if (!is.null(match_summary) && !is.null(shiny_data$time_summary_10km)) {

  # Step 1: Known species per class from the reference taxonomy
  known_by_class <- match_summary |>
    as_tibble() |>
    filter(!is.na(class), class != "") |>
    group_by(kingdom, phylum, class) |>
    summarise(
      n_known_species = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      .groups = "drop"
    )

  # Step 2: GBIF occurrence counts per class from order_temporal
  # (order_temporal has year × order, but we need class — use match_summary
  #  to map order→class, then join with time_summary via... hmm.)
  #
  # Actually, time_summary is at cell level, not taxonomic level.
  # order_temporal IS at the taxonomic level. We'll use it plus the
  # order→class mapping from match_summary.

  order_to_class <- match_summary |>
    as_tibble() |>
    filter(!is.na(order), order != "", !is.na(class), class != "") |>
    distinct(kingdom, phylum, class, order)

  order_temporal <- safe_read(here(p_integrated, "order_temporal_trends.csv"))
  if (!is.null(order_temporal)) {
    occ_by_class <- order_temporal |>
      as_tibble() |>
      inner_join(order_to_class, by = "order") |>
      mutate(era = ifelse(year >= as.integer(substr(as.character(recent_cutoff_ym), 1, 4)), "last_year", "prior")) |>
      group_by(kingdom, phylum, class, era) |>
      summarise(
        occurrences = sum(total_occurrences, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)

    # Ensure both columns exist
    if (!"prior" %in% names(occ_by_class)) occ_by_class$prior <- 0
    if (!"last_year" %in% names(occ_by_class)) occ_by_class$last_year <- 0

    # Step 3: Join and compute bias
    troudet <- known_by_class |>
      left_join(occ_by_class, by = c("kingdom", "phylum", "class")) |>
      mutate(
        prior = replace_na(prior, 0),
        last_year = replace_na(last_year, 0),
        total_occ = prior + last_year,
        # Ideal: proportional to known species
        total_known = sum(n_known_species),
        total_occ_all = sum(total_occ),
        pct_known = n_known_species / total_known,
        ideal_occ = pct_known * total_occ_all,
        bias = total_occ - ideal_occ
      ) |>
      select(kingdom, phylum, class,
             n_known_species, n_in_gbif,
             occ_prior = prior, occ_last_year = last_year, total_occ,
             ideal_occ, bias) |>
      arrange(desc(abs(bias)))

    shiny_data$troudet_bias <- troudet
    cli_alert_success("Troudet bias data: {nrow(troudet)} classes")

    # Also compute at order level for drill-down
    occ_by_order <- order_temporal |>
      as_tibble() |>
      inner_join(order_to_class, by = "order") |>
      mutate(era = ifelse(year >= as.integer(substr(as.character(recent_cutoff_ym), 1, 4)), "last_year", "prior")) |>
      group_by(kingdom, phylum, class, order, era) |>
      summarise(
        occurrences = sum(total_occurrences, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)

    if (!"prior" %in% names(occ_by_order)) occ_by_order$prior <- 0
    if (!"last_year" %in% names(occ_by_order)) occ_by_order$last_year <- 0

    known_by_order <- match_summary |>
      as_tibble() |>
      filter(!is.na(order), order != "") |>
      group_by(kingdom, phylum, class, order) |>
      summarise(
        n_known_species = n(),
        n_in_gbif = sum(matched_any, na.rm = TRUE),
        .groups = "drop"
      )

    troudet_order <- known_by_order |>
      left_join(occ_by_order, by = c("kingdom", "phylum", "class", "order")) |>
      mutate(
        prior = replace_na(prior, 0),
        last_year = replace_na(last_year, 0),
        total_occ = prior + last_year,
        total_known = sum(n_known_species),
        total_occ_all = sum(total_occ),
        pct_known = n_known_species / total_known,
        ideal_occ = pct_known * total_occ_all,
        bias = total_occ - ideal_occ
      ) |>
      select(kingdom, phylum, class, order,
             n_known_species, n_in_gbif,
             occ_prior = prior, occ_last_year = last_year, total_occ,
             ideal_occ, bias) |>
      arrange(desc(abs(bias)))

    shiny_data$troudet_bias_order <- troudet_order
    cli_alert_success("Troudet bias by order: {nrow(troudet_order)} orders")

    # Also compute at family level for drill-down
    family_time_raw <- safe_read(here(p_derived, "family_time_summary_10km.csv"))
    if (!is.null(family_time_raw)) {
      order_to_family <- match_summary |>
        as_tibble() |>
        filter(!is.na(family), family != "", !is.na(order), order != "") |>
        distinct(kingdom, phylum, class, order, family)

      occ_by_family <- family_time_raw |>
        as_tibble() |>
        filter(basisofrecord == "all") |>
        mutate(year = as.integer(str_sub(as.character(yearmonth), 1, 4))) |>
        inner_join(order_to_family, by = c("order", "family")) |>
        mutate(era = ifelse(year >= as.integer(substr(as.character(recent_cutoff_ym), 1, 4)), "last_year", "prior")) |>
        group_by(kingdom, phylum, class, order, family, era) |>
        summarise(occurrences = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
        pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)

      if (!"prior" %in% names(occ_by_family)) occ_by_family$prior <- 0
      if (!"last_year" %in% names(occ_by_family)) occ_by_family$last_year <- 0

      known_by_family <- match_summary |>
        as_tibble() |>
        filter(!is.na(family), family != "") |>
        group_by(kingdom, phylum, class, order, family) |>
        summarise(
          n_known_species = n(),
          n_in_gbif = sum(matched_any, na.rm = TRUE),
          .groups = "drop"
        )

      troudet_family <- known_by_family |>
        left_join(occ_by_family, by = c("kingdom", "phylum", "class", "order", "family")) |>
        mutate(
          prior = replace_na(prior, 0),
          last_year = replace_na(last_year, 0),
          total_occ = prior + last_year,
          total_known = sum(n_known_species),
          total_occ_all = sum(total_occ),
          pct_known = n_known_species / total_known,
          ideal_occ = pct_known * total_occ_all,
          bias = total_occ - ideal_occ
        ) |>
        select(kingdom, phylum, class, order, family,
               n_known_species, n_in_gbif,
               occ_prior = prior, occ_last_year = last_year, total_occ,
               ideal_occ, bias) |>
        arrange(desc(abs(bias)))

      shiny_data$troudet_bias_family <- troudet_family
      cli_alert_success("Troudet bias by family: {nrow(troudet_family)} families")
    }

  } else {
    cli_alert_warning("order_temporal_trends.csv not found — Troudet bias data not computed")
  }

  # --- Published (mobilised) last 12 months by class/order/family ---
  # Read directly from parquet cube to get year_published × order breakdowns
  cli_h3("Computing Published (Mobilised) Last 12 Months by Taxonomy")

  cube_10km_parquet <- here(p_data_proc, "cubes", "cube_10km.parquet")
  if (file.exists(cube_10km_parquet) && requireNamespace("arrow", quietly = TRUE)) {
    pub_dt <- as.data.table(
      arrow::open_dataset(cube_10km_parquet) |>
        dplyr::select(specieskey, species, kingdom, phylum, class, order, family,
                      year_published, month_published, occurrences) |>
        dplyr::collect()
    )
    pub_dt[, yearmonth_pub := as.integer(year_published) * 100L + as.integer(month_published)]
    pub_dt[is.na(order) | order == "", order := "Unplaced"]
    pub_dt[is.na(family) | family == "", family := "Unplaced"]

    # Published by class
    pub_by_class <- pub_dt[, .(
      pub_last_year = sum(as.numeric(occurrences[yearmonth_pub >= recent_cutoff_ym]), na.rm = TRUE),
      pub_prior = sum(as.numeric(occurrences[yearmonth_pub < recent_cutoff_ym]), na.rm = TRUE)
    ), by = .(kingdom, phylum, class)]

    if (!is.null(shiny_data$troudet_bias)) {
      shiny_data$troudet_bias <- shiny_data$troudet_bias |>
        left_join(pub_by_class, by = c("kingdom", "phylum", "class")) |>
        mutate(pub_last_year = replace_na(pub_last_year, 0),
               pub_prior = replace_na(pub_prior, 0))
      cli_alert_success("Troudet bias (class): added pub_last_year column")
    }

    # Published by order
    pub_by_order <- pub_dt[, .(
      pub_last_year = sum(as.numeric(occurrences[yearmonth_pub >= recent_cutoff_ym]), na.rm = TRUE),
      pub_prior = sum(as.numeric(occurrences[yearmonth_pub < recent_cutoff_ym]), na.rm = TRUE)
    ), by = .(kingdom, phylum, class, order)]

    if (!is.null(shiny_data$troudet_bias_order)) {
      shiny_data$troudet_bias_order <- shiny_data$troudet_bias_order |>
        left_join(pub_by_order, by = c("kingdom", "phylum", "class", "order")) |>
        mutate(pub_last_year = replace_na(pub_last_year, 0),
               pub_prior = replace_na(pub_prior, 0))
      cli_alert_success("Troudet bias (order): added pub_last_year column")
    }

    # Published by family
    pub_by_family <- pub_dt[, .(
      pub_last_year = sum(as.numeric(occurrences[yearmonth_pub >= recent_cutoff_ym]), na.rm = TRUE),
      pub_prior = sum(as.numeric(occurrences[yearmonth_pub < recent_cutoff_ym]), na.rm = TRUE)
    ), by = .(kingdom, phylum, class, order, family)]

    if (!is.null(shiny_data$troudet_bias_family)) {
      shiny_data$troudet_bias_family <- shiny_data$troudet_bias_family |>
        left_join(pub_by_family, by = c("kingdom", "phylum", "class", "order", "family")) |>
        mutate(pub_last_year = replace_na(pub_last_year, 0),
               pub_prior = replace_na(pub_prior, 0))
      cli_alert_success("Troudet bias (family): added pub_last_year column")
    }

    rm(pub_dt); gc()
  } else {
    cli_alert_info("Parquet cube not found — published-by-taxonomy not computed")
  }
} else {
  cli_alert_warning("Cannot compute Troudet data (need match_summary + time_summary)")
}

# ===========================================================================
# 8c. LAST-YEAR SPLITS FOR TAXONOMIC BAR CHARTS
# ===========================================================================
# Add occ_prior / occ_last_year to the tax_by_order and tax_by_family
# summaries so the app can stack bars showing the last-year contribution.

cli_h2("Adding Last-Year Occurrence Splits to Taxonomic Summaries")

if (!is.null(shiny_data$troudet_bias_order) && !is.null(shiny_data$tax_by_order)) {
  # Select observed AND published last-year columns (pub columns may not exist if parquet wasn't available)
  avail_cols <- intersect(
    c("kingdom", "phylum", "class", "order", "occ_prior", "occ_last_year", "total_occ",
      "pub_last_year", "pub_prior"),
    names(shiny_data$troudet_bias_order)
  )
  order_occ <- shiny_data$troudet_bias_order |> select(all_of(avail_cols))

  shiny_data$tax_by_order <- shiny_data$tax_by_order |>
    left_join(order_occ, by = intersect(names(shiny_data$tax_by_order), names(order_occ))) |>
    mutate(
      occ_prior = replace_na(occ_prior, 0),
      occ_last_year = replace_na(occ_last_year, 0),
      total_occ = replace_na(total_occ, 0)
    )
  # Also fill pub columns if they were joined
  if ("pub_last_year" %in% names(shiny_data$tax_by_order)) {
    shiny_data$tax_by_order <- shiny_data$tax_by_order |>
      mutate(pub_last_year = replace_na(pub_last_year, 0),
             pub_prior = replace_na(pub_prior, 0))
  }
  cli_alert_success("tax_by_order now includes occ + pub last-year columns")
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
  has_priorities = !is.null(shiny_data$priority_taxa_missing) || !is.null(shiny_data$priority_taxa_all),
  has_last_year = !is.null(shiny_data$overview_last_year),
  has_troudet = !is.null(shiny_data$troudet_bias),
  has_invasive = !is.null(shiny_data$tax_by_invasive),
  has_dyntaxa_scope = !is.null(shiny_data$species_scope_lookup),
  has_kingdom_cell_recency = !is.null(shiny_data$kingdom_cell_recency),
  last_year = if (!is.null(shiny_data$last_year)) shiny_data$last_year else NA
)

# ===========================================================================
# 9b. PUBLISHER DATA (NEW)
# ===========================================================================

cli_h2("Loading Publisher Data")

publisher_summary_path <- here(p_derived, "publisher_summary_10km.csv")
publisher_cell_path <- here(p_derived, "publisher_cell_dependency_10km.csv")
published_time_path <- here(p_derived, "published_time_summary_10km.csv")

if (file.exists(publisher_summary_path)) {
  shiny_data$publisher_summary <- as_tibble(fread(publisher_summary_path))
  cli_alert_success("Publisher summary: {nrow(shiny_data$publisher_summary)} publishers")
} else {
  cli_alert_info("No publisher summary found — run 06a with publisher phase enabled")
}

if (file.exists(publisher_cell_path)) {
  shiny_data$publisher_cell_dependency <- as_tibble(fread(publisher_cell_path))
  n_single <- sum(shiny_data$publisher_cell_dependency$n_publishers == 1)
  cli_alert_success("Publisher cell dependency: {n_single} single-publisher cells")
}

if (file.exists(published_time_path)) {
  pub_time <- as_tibble(fread(published_time_path))
  # Create yearmonth_published for the "published to GBIF" timeline
  # Ensure year_published is numeric and filter out invalid values
  if (all(c("year_published", "month_published") %in% names(pub_time))) {
    pub_time <- pub_time |>
      mutate(
        year_published = as.integer(year_published),
        month_published = as.integer(month_published)
      ) |>
      filter(
        !is.na(year_published), !is.na(month_published),
        year_published >= 1900, year_published <= year(Sys.Date()) + 1,
        month_published >= 1, month_published <= 12
      ) |>
      mutate(
        yearmonth_published = year_published * 100L + month_published
      )
    n_dropped <- nrow(as_tibble(fread(published_time_path))) - nrow(pub_time)
    if (n_dropped > 0) {
      cli_alert_info("Filtered {scales::comma(n_dropped)} rows with invalid year/month_published")
    }
  }
  shiny_data$published_time_summary <- pub_time
  cli_alert_success("Published time summary: {nrow(pub_time)} rows")

  # Compute "published in last 12 months" vs "observed in last 12 months"
  if (!is.null(recent_cutoff_ym) && "yearmonth_published" %in% names(pub_time)) {
    published_recent <- pub_time |>
      filter(yearmonth_published >= recent_cutoff_ym) |>
      summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE))

    shiny_data$overview_last_year$occ_published_last_year <- published_recent$total_occ[1]
    cli_alert_success("Published in recent period: {scales::comma(published_recent$total_occ[1])} occurrences")
    cli_alert_info("  vs observed in recent period: {scales::comma(shiny_data$overview_last_year$occ_last_year)}")
  }
} else {
  cli_alert_info("No published time summary found")
}

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
cli_alert_info("  - Last year delta ({shiny_data$metadata$last_year}): {shiny_data$metadata$has_last_year}")
cli_alert_info("  - Troudet bias data: {shiny_data$metadata$has_troudet}")
cli_alert_info("  - Cascading filters (kingdom/phylum/class in tax_by_order/family): {all(c('kingdom', 'phylum', 'class') %in% names(shiny_data$tax_by_order))}")
