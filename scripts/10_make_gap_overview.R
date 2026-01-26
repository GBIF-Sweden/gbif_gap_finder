# scripts/10_make_gap_overview.R
# ==============================================================================
# Integrated Gap Overview - Multi-Dimensional Summaries
# ==============================================================================
# Creates comprehensive, plot-ready integrated tables combining spatial,
# temporal, and taxonomic dimensions with priority lists for action

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(data.table)
library(cli)
library(lubridate)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------
p_gaps <- here(p_data_proc, "gaps")
p_derived <- here(p_data_proc, "derived")
p_output <- here(cfg_get("paths.output", "output"))
p_tables <- here(p_output, "tables")
p_integrated <- here(p_output, "tables", "integrated")

# Create directories
dir.create(p_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(p_integrated, showWarnings = FALSE, recursive = TRUE)

# Helper functions --------------------------------------------------------

safe_read_gap <- function(filename) {
  path <- here(p_gaps, filename)
  if (!file.exists(path)) {
    cli_alert_warning("File not found: {.path {filename}}")
    return(NULL)
  }
  fread(path)
}

write_integrated <- function(dt, filename) {
  if (is.null(dt) || nrow(dt) == 0) {
    cli_alert_warning("{filename}: No data to write")
    return(invisible(NULL))
  }
  path <- here(p_integrated, filename)
  fwrite(dt, path)
  cli_alert_success("{filename}: {scales::comma(nrow(dt))} rows")
}

write_table <- function(dt, filename) {
  if (is.null(dt) || nrow(dt) == 0) {
    cli_alert_warning("{filename}: No data to write")
    return(invisible(NULL))
  }
  path <- here(p_tables, filename)
  fwrite(dt, path)
  cli_alert_success("{filename}: {scales::comma(nrow(dt))} rows")
}

# Load gap outputs --------------------------------------------------------
cli_h2("Loading Gap Analysis Outputs")

spatial_10 <- safe_read_gap("spatial_gaps_10km.csv")
spatial_50 <- safe_read_gap("spatial_gaps_50km.csv")
temporal_year_10 <- safe_read_gap("temporal_overview_year_10km.csv")
temporal_year_50 <- safe_read_gap("temporal_overview_year_50km.csv")
temporal_year_basis_10 <- safe_read_gap("temporal_year_by_basis_10km.csv")
temporal_year_basis_50 <- safe_read_gap("temporal_year_by_basis_50km.csv")
temporal_month_basis_10 <- safe_read_gap("temporal_month_by_basis_10km.csv")
temporal_month_basis_50 <- safe_read_gap("temporal_month_by_basis_50km.csv")
recency_10 <- safe_read_gap("cell_recency_10km.csv")
recency_50 <- safe_read_gap("cell_recency_50km.csv")
tax_coverage_rank <- safe_read_gap("taxonomic_coverage_by_rank.csv")
tax_coverage_threat <- safe_read_gap("taxonomic_coverage_by_threat.csv")
tax_spatial <- safe_read_gap("taxonomic_spatial_coverage.csv")
tax_priority <- safe_read_gap("taxonomic_priority_taxa.csv")
tax_missing <- safe_read_gap("taxonomic_missing_taxa.csv")

cli_alert_success("Gap files loaded")

# 1. Dashboard Summary ----------------------------------------------------
cli_h2("Creating Dashboard Summary")

dashboard <- tibble(
  cells_10km_total = if(!is.null(spatial_10)) nrow(spatial_10[basisofrecord == "all"]) else NA,
  cells_10km_with_data = if(!is.null(spatial_10)) sum(spatial_10[basisofrecord == "all"]$has_data) else NA,
  cells_10km_pct_coverage = if(!is.null(spatial_10)) round(100 * mean(spatial_10[basisofrecord == "all"]$has_data), 1) else NA,
  
  cells_50km_total = if(!is.null(spatial_50)) nrow(spatial_50[basisofrecord == "all"]) else NA,
  cells_50km_with_data = if(!is.null(spatial_50)) sum(spatial_50[basisofrecord == "all"]$has_data) else NA,
  cells_50km_pct_coverage = if(!is.null(spatial_50)) round(100 * mean(spatial_50[basisofrecord == "all"]$has_data), 1) else NA,
  
  year_min = if(!is.null(temporal_year_10)) min(temporal_year_10$year, na.rm = TRUE) else NA,
  year_max = if(!is.null(temporal_year_10)) max(temporal_year_10$year, na.rm = TRUE) else NA,
  year_range = if(!is.null(temporal_year_10)) max(temporal_year_10$year) - min(temporal_year_10$year) + 1 else NA,
  
  median_staleness_months_10km = if(!is.null(recency_10)) median(recency_10[basisofrecord == "all"]$staleness_months, na.rm = TRUE) else NA,
  pct_stale_5y_10km = if(!is.null(recency_10)) round(100 * mean(recency_10[basisofrecord == "all"]$gap_stale_5y, na.rm = TRUE), 1) else NA,
  
  taxa_in_reference = if(!is.null(tax_coverage_rank)) sum(tax_coverage_rank$n_ref_total, na.rm = TRUE) else NA,
  taxa_in_gbif = if(!is.null(tax_coverage_rank)) sum(tax_coverage_rank$n_in_gbif, na.rm = TRUE) else NA,
  taxa_pct_coverage = if(!is.null(tax_coverage_rank)) round(100 * sum(tax_coverage_rank$n_in_gbif) / sum(tax_coverage_rank$n_ref_total), 1) else NA,
  
  n_priority_taxa = if(!is.null(tax_priority)) nrow(tax_priority) else NA,
  analysis_date = as.character(Sys.Date())
)

write_table(dashboard, "dashboard_summary.csv")

# 2. Spatial × Temporal × Basis -------------------------------------------
cli_h2("Creating Space × Time × Basis")

if (!is.null(recency_10) && !is.null(temporal_year_basis_10)) {
  if ("last_ym" %in% names(recency_10)) {
    
    recency_copy <- copy(recency_10)
    recency_copy[, last_year := year(last_ym)]
    
    cells_per_year <- recency_copy[, .(
      n_cells_sampled = .N,
      total_occurrences = sum(total_occurrences, na.rm = TRUE)
    ), by = .(grid, last_year, basisofrecord)]
    
    setnames(cells_per_year, "last_year", "year")
    
    space_time_basis <- merge(
      temporal_year_basis_10,
      cells_per_year,
      by = c("grid", "year", "basisofrecord"),
      all.x = TRUE
    )
    
    space_time_basis[is.na(n_cells_sampled), n_cells_sampled := 0]
    write_integrated(space_time_basis, "space_time_basis_10km.csv")
  }
}

if (!is.null(recency_50) && !is.null(temporal_year_basis_50)) {
  if ("last_ym" %in% names(recency_50)) {
    
    recency_copy <- copy(recency_50)
    recency_copy[, last_year := year(last_ym)]
    
    cells_per_year <- recency_copy[, .(
      n_cells_sampled = .N,
      total_occurrences = sum(total_occurrences, na.rm = TRUE)
    ), by = .(grid, last_year, basisofrecord)]
    
    setnames(cells_per_year, "last_year", "year")
    
    space_time_basis <- merge(
      temporal_year_basis_50,
      cells_per_year,
      by = c("grid", "year", "basisofrecord"),
      all.x = TRUE
    )
    
    space_time_basis[is.na(n_cells_sampled), n_cells_sampled := 0]
    write_integrated(space_time_basis, "space_time_basis_50km.csv")
  }
}

# 3. Spatial × Taxonomic × Basis ------------------------------------------
cli_h2("Creating Space × Taxonomy × Basis")

sp10_path <- here(p_derived, "species_summary_10km.csv")

if (file.exists(sp10_path)) {
  sp10_data <- fread(sp10_path)
  
  if ("specieskey" %in% names(sp10_data) && "occurrences" %in% names(sp10_data)) {
    
    # Filter to positive occurrences
    sp10_pos <- sp10_data[occurrences > 0]
    
    # Basic aggregation
    if ("eeacellcode" %in% names(sp10_data)) {
      space_tax_basis <- sp10_pos[, .(
        n_species = uniqueN(specieskey),
        n_cells = length(unique(eeacellcode)),
        total_occurrences = sum(occurrences)
      ), by = .(grid, basisofrecord)]
    } else {
      space_tax_basis <- sp10_pos[, .(
        n_species = uniqueN(specieskey),
        total_occurrences = sum(occurrences)
      ), by = .(grid, basisofrecord)]
    }
    
    write_integrated(space_tax_basis, "space_taxonomy_simple_10km.csv")
    
    # With rank if available
    if (!is.null(tax_spatial) && "species" %in% names(sp10_data) && "scientificName" %in% names(tax_spatial)) {
      
      tax_copy <- copy(tax_spatial)
      sp_copy <- copy(sp10_pos)
      
      tax_copy[, species_std := tolower(str_trim(scientificName))]
      sp_copy[, species_std := tolower(str_trim(species))]
      
      sp_with_rank <- merge(
        sp_copy,
        tax_copy[, .(species_std, taxonRank)],
        by = "species_std",
        all.x = TRUE
      )
      
      sp_ranked <- sp_with_rank[!is.na(taxonRank)]
      
      if ("eeacellcode" %in% names(sp_ranked)) {
        space_tax_rank_basis <- sp_ranked[, .(
          n_taxa = uniqueN(specieskey),
          n_cells = length(unique(eeacellcode)),
          total_occurrences = sum(occurrences)
        ), by = .(grid, taxonRank, basisofrecord)]
      } else {
        space_tax_rank_basis <- sp_ranked[, .(
          n_taxa = uniqueN(specieskey),
          total_occurrences = sum(occurrences)
        ), by = .(grid, taxonRank, basisofrecord)]
      }
      
      write_integrated(space_tax_rank_basis, "space_taxonomy_basis_10km.csv")
    }
  }
}

# 4. Temporal × Taxonomic × Basis -----------------------------------------
cli_h2("Creating Time × Taxonomy × Basis")

fam10_path <- here(p_derived, "family_time_summary_10km.csv")

if (file.exists(fam10_path)) {
  fam_data <- fread(fam10_path)
  
  if ("yearmonth" %in% names(fam_data) && "family" %in% names(fam_data)) {
    fam_copy <- copy(fam_data)
    fam_copy[, year := as.integer(str_sub(str_trim(as.character(yearmonth)), 1, 4))]
    
    time_tax_basis <- fam_copy[, .(
      n_families = uniqueN(family),
      total_occurrences = sum(occurrences, na.rm = TRUE)
    ), by = .(grid, year, basisofrecord)]
    
    write_integrated(time_tax_basis, "time_taxonomy_basis_10km.csv")
  }
}

# 5. Priority Lists -------------------------------------------------------
cli_h2("Creating Priority Lists")

# Priority 1: Zero coverage cells
if (!is.null(spatial_10) && "cell_all_zero" %in% names(spatial_10)) {
  zero_cells <- spatial_10[cell_all_zero == TRUE]
  
  if (nrow(zero_cells) > 0 && "eeacellcode" %in% names(zero_cells)) {
    priority_zero <- unique(zero_cells[, .(grid, eeacellcode, total_occurrences_cell)])
    priority_zero[, priority_reason := "Zero coverage across all basis types"]
    
    write_integrated(priority_zero, "priority_zero_coverage_cells.csv")
  } else {
    cli_alert_info("No zero coverage cells found")
  }
}

# Priority 2: Undersampled threatened taxa
if (!is.null(tax_priority) && nrow(tax_priority) > 0) {
  priority_taxa_copy <- copy(tax_priority)
  
  # Select only existing columns
  cols_to_keep <- c("taxonID", "scientificName", "taxonRank", "threatStatus", 
                    "family", "order", "priority")
  
  optional_cols <- c("n_cells_10km", "n_cells_50km", "total_occ_10km")
  for (col in optional_cols) {
    if (col %in% names(priority_taxa_copy)) {
      cols_to_keep <- c(cols_to_keep, col)
    }
  }
  
  priority_taxa_out <- priority_taxa_copy[, ..cols_to_keep]
  setorder(priority_taxa_out, threatStatus, scientificName)
  
  write_integrated(priority_taxa_out, "priority_undersampled_taxa.csv")
}

# Priority 3: Stale cells
if (!is.null(recency_10) && "gap_stale_5y" %in% names(recency_10) && "staleness_months" %in% names(recency_10)) {
  stale_cells <- recency_10[basisofrecord == "all" & gap_stale_5y == TRUE]
  
  if (nrow(stale_cells) > 0) {
    priority_stale <- stale_cells[, .(grid, eeacellcode, last_ym, staleness_months, total_occurrences)]
    
    # Create reason without using glue inside data.table
    priority_stale[, years_stale := round(staleness_months / 12, 1)]
    priority_stale[, priority_reason := paste0("Not sampled in ", years_stale, " years")]
    priority_stale[, years_stale := NULL]
    
    setorder(priority_stale, -staleness_months)
    
    write_integrated(priority_stale, "priority_stale_cells.csv")
  } else {
    cli_alert_info("No stale cells found")
  }
}

# 6. Comparison Tables ----------------------------------------------------
cli_h2("Creating Comparison Tables")

# Grid resolutions
if (!is.null(spatial_10) && !is.null(spatial_50)) {
  
  comp_10 <- spatial_10[basisofrecord == "all", .(
    grid_resolution = "10km",
    n_cells_total = .N,
    n_cells_with_data = sum(has_data),
    pct_coverage = round(100 * mean(has_data), 1),
    total_occurrences = sum(occurrences),
    mean_occ_per_cell = round(mean(occurrences[occurrences > 0]), 1)
  )]
  
  comp_50 <- spatial_50[basisofrecord == "all", .(
    grid_resolution = "50km",
    n_cells_total = .N,
    n_cells_with_data = sum(has_data),
    pct_coverage = round(100 * mean(has_data), 1),
    total_occurrences = sum(occurrences),
    mean_occ_per_cell = round(mean(occurrences[occurrences > 0]), 1)
  )]
  
  comparison_grids <- rbindlist(list(comp_10, comp_50))
  write_table(comparison_grids, "comparison_grid_resolutions.csv")
}

# Basis types
if (!is.null(spatial_10)) {
  comparison_basis <- spatial_10[basisofrecord != "all", .(
    n_cells_with_data = sum(has_data),
    pct_cells_covered = round(100 * mean(has_data), 1),
    total_occurrences = sum(occurrences)
  ), by = basisofrecord]
  
  setorder(comparison_basis, -total_occurrences)
  write_table(comparison_basis, "comparison_basis_types.csv")
}

# Taxonomic ranks
if (!is.null(tax_coverage_rank)) {
  comparison_ranks <- tax_coverage_rank[, .(
    taxonRank, n_total = n_ref_total, n_in_gbif, pct_coverage, pct_grid10, pct_grid50
  )]
  setorder(comparison_ranks, -n_total)
  write_table(comparison_ranks, "comparison_taxon_ranks.csv")
}

# 7. Standard Overview Tables ---------------------------------------------
cli_h2("Creating Standard Overview Tables")

# Spatial
if (!is.null(spatial_10) && !is.null(spatial_50)) {
  spatial_all <- rbindlist(list(spatial_10, spatial_50))
  
  spatial_overview <- spatial_all[, .(
    n_cells = .N,
    n_zero = sum(gap_zero, na.rm = TRUE),
    pct_zero = round(100 * mean(gap_zero, na.rm = TRUE), 2),
    n_low_q10 = sum(gap_low_q10, na.rm = TRUE),
    pct_low_q10 = round(100 * mean(gap_low_q10, na.rm = TRUE), 2),
    total_occurrences = sum(occurrences)
  ), by = .(grid, basisofrecord)]
  
  setorder(spatial_overview, grid, basisofrecord)
  write_table(spatial_overview, "overview_spatial_gap_rates.csv")
}

# Temporal - year
if (!is.null(temporal_year_10) && !is.null(temporal_year_50)) {
  temporal_year_all <- rbindlist(list(temporal_year_10, temporal_year_50))
  write_table(temporal_year_all, "overview_temporal_year.csv")
}

# Temporal - month
if (!is.null(temporal_month_basis_10) && !is.null(temporal_month_basis_50)) {
  month_10_all <- temporal_month_basis_10[basisofrecord == "all"]
  month_50_all <- temporal_month_basis_50[basisofrecord == "all"]
  temporal_month_all <- rbindlist(list(month_10_all, month_50_all))
  
  write_table(temporal_month_all, "overview_temporal_month.csv")
}

# Recency
if (!is.null(recency_10) && !is.null(recency_50)) {
  recency_all <- rbindlist(list(recency_10, recency_50))
  
  recency_overview <- recency_all[, .(
    n_cells = .N,
    pct_stale_12m = round(100 * mean(gap_stale_12m, na.rm = TRUE), 2),
    pct_stale_5y = round(100 * mean(gap_stale_5y, na.rm = TRUE), 2),
    median_staleness_months = as.numeric(median(staleness_months, na.rm = TRUE))
  ), by = .(grid, basisofrecord)]
  
  setorder(recency_overview, grid, basisofrecord)
  write_table(recency_overview, "overview_temporal_recency_rates.csv")
}

# Taxonomic
if (!is.null(tax_coverage_rank)) {
  write_table(tax_coverage_rank, "overview_taxonomic_summary.csv")
}

if (!is.null(tax_missing)) {
  write_table(tax_missing, "overview_missing_taxa.csv")
}

# Summary -----------------------------------------------------------------
cli_h2("Summary Statistics")

n_integrated <- length(list.files(p_integrated, pattern = "\\.csv$"))
n_tables <- length(list.files(p_tables, pattern = "\\.csv$", recursive = FALSE))

cli_alert_success("Created {n_integrated} integrated tables")
cli_alert_success("Created {n_tables} standard tables")
cli_alert_info("Standard tables: {.path {p_tables}}")
cli_alert_info("Integrated tables: {.path {p_integrated}}")
cli_alert_success("Gap overview complete!")
