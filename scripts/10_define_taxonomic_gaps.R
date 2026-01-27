# scripts/10_define_taxonomic_gaps.R
# ==============================================================================
# Taxonomic Gap Analysis - Comprehensive
# ==============================================================================
# This script creates detailed taxonomic gap metrics across multiple dimensions:
# - Coverage by taxonomic rank
# - Coverage by threat status
# - Coverage by basis of record
# - Spatial coverage per taxon
# - Temporal coverage per taxon
# - Priority taxa (threatened, endemic, poorly sampled)
#
# Outputs organized for flexible conservation planning

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(data.table)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------
p_derived <- here(p_data_proc, "derived")
p_gaps <- here(p_data_proc, "gaps")

dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# Thresholds for "poorly sampled" definition
MIN_OCCURRENCES <- 10
MIN_CELLS <- 5

# Helper functions --------------------------------------------------------

#' Standardize scientific names for matching
standardize_name <- function(x) {
  x <- str_to_lower(as.character(x))
  x <- str_trim(x)
  x <- str_squish(x)  # Collapse multiple spaces
  
  # Conservative binomial extraction
  # Keep only Genus species if it looks like a binomial
  parts <- str_split(x, "\\s+", simplify = TRUE)
  is_binomial <- ncol(parts) >= 2 & 
    str_detect(parts[, 1], "^[a-z]+$") & 
    str_detect(parts[, 2], "^[a-z-]+$")
  
  result <- x
  result[is_binomial] <- paste(parts[is_binomial, 1], parts[is_binomial, 2])
  result
}

#' Read species summary
read_species_summary <- function(filename) {
  path <- here(p_derived, filename)
  
  if (!file.exists(path)) {
    cli_abort("Species summary not found: {.path {path}}")
  }
  
  dt <- fread(path)
  
  if (!("species" %in% names(dt))) {
    cli_abort("Column 'species' not found in: {.path {filename}}")
  }
  
  dt
}

# Load data ---------------------------------------------------------------
cli_h2("Loading Data")

# Species summaries from cubes 
sp10 <- read_species_summary("species_summary_10km.csv")
sp50 <- read_species_summary("species_summary_50km.csv")

cli_alert_success("Loaded 10km species: {scales::comma(nrow(sp10))} rows")
cli_alert_success("Loaded 50km species: {scales::comma(nrow(sp50))} rows")

# Taxonomic reference
tax_ref_path <- here(p_data_proc, "taxa_reference_current.rds")

if (!file.exists(tax_ref_path)) {
  cli_abort("Taxa reference not found: {.path {tax_ref_path}}")
}

tax_ref <- readRDS(tax_ref_path)
tax_ref <- as.data.table(tax_ref)

cli_alert_success("Loaded taxa reference: {scales::comma(nrow(tax_ref))} taxa")

# Validate required columns
required_cols <- c("taxonID", "scientificName", "taxonRank", "threatStatus")
missing_cols <- setdiff(required_cols, names(tax_ref))

if (length(missing_cols) > 0) {
  cli_abort(c(
    "Missing required columns in taxa reference",
    "x" = "Missing: {paste(missing_cols, collapse = ', ')}"
  ))
}

# Prepare cube species ----------------------------------------------------
cli_h2("Preparing Cube Species Lists")

# Extract unique species from cubes (using basisofrecord == "all")
extract_species <- function(sp_data, grid_label) {
  dt <- as.data.table(sp_data)
  dt <- dt[basisofrecord == "all"]
  dt[, species_std := standardize_name(species)]
  dt[, grid := grid_label]
  
  unique(dt[, .(grid, specieskey, species, species_std)])
}

cube_species_10 <- extract_species(sp10, "grid10km")
cube_species_50 <- extract_species(sp50, "grid50km")

# Combined species list across grids
cube_species_combined <- rbindlist(list(cube_species_10, cube_species_50))
cube_species_combined <- unique(cube_species_combined)

cli_alert_info("Unique species in cubes: {scales::comma(uniqueN(cube_species_combined$specieskey))}")

# Prepare reference taxonomy ----------------------------------------------
cli_h2("Preparing Reference Taxonomy")

tax_ref[, scientific_std := standardize_name(scientificName)]

# Select key columns
tax_clean <- tax_ref[, .(
  taxonID, 
  scientificName, 
  scientific_std,
  taxonRank,
  threatStatus,
  kingdom = if("kingdom" %in% names(tax_ref)) kingdom else NA_character_,
  phylum = if("phylum" %in% names(tax_ref)) phylum else NA_character_,
  class = if("class" %in% names(tax_ref)) class else NA_character_,
  order = if("order" %in% names(tax_ref)) order else NA_character_,
  family = if("family" %in% names(tax_ref)) family else NA_character_,
  establishmentMeans = if("establishmentMeans" %in% names(tax_ref)) establishmentMeans else NA_character_
)]

# Match cube species to reference taxonomy --------------------------------
cli_h2("Matching Cube Species to Reference")

# Match on standardized names
match_table <- merge(
  tax_clean,
  cube_species_combined[, .(grid, specieskey, species, species_std)],
  by.x = "scientific_std",
  by.y = "species_std",
  all.x = TRUE,
  allow.cartesian = TRUE
)

match_table[, matched := !is.na(specieskey)]

cli_alert_info("Matching results:")
cli_alert_info("  Matched: {scales::comma(sum(match_table$matched))}")
cli_alert_info("  Unmatched: {scales::comma(sum(!match_table$matched))}")

# Summarize matches per taxon ---------------------------------------------
match_summary <- match_table[, .(
  matched_any = any(matched),
  matched_grid10 = any(matched & grid == "grid10km", na.rm = TRUE),
  matched_grid50 = any(matched & grid == "grid50km", na.rm = TRUE),
  n_grids = uniqueN(grid[matched], na.rm = TRUE)
), by = .(taxonID, scientificName, taxonRank, threatStatus, 
          kingdom, phylum, class, order, family, establishmentMeans, scientific_std)]

# Identify gaps -----------------------------------------------------------
cli_h2("Identifying Taxonomic Gaps")

# Missing taxa (not in GBIF cubes)
missing_taxa <- match_summary[matched_any == FALSE]

cli_alert_warning("Missing taxa: {scales::comma(nrow(missing_taxa))}")

# Missing threatened taxa
threatened_codes <- c("CR", "EN", "VU", "NT")  # Include Near Threatened
missing_threatened <- missing_taxa[threatStatus %in% threatened_codes]

cli_alert_warning("Missing threatened taxa: {scales::comma(nrow(missing_threatened))}")

# Coverage by taxonomic rank ----------------------------------------------
cli_h2("Analyzing Coverage by Rank")

rank_summary <- match_summary[, .(
  n_ref_total = .N,
  n_in_gbif = sum(matched_any),
  n_missing = sum(!matched_any),
  pct_coverage = round(100 * sum(matched_any) / .N, 2),
  n_grid10 = sum(matched_grid10),
  n_grid50 = sum(matched_grid50),
  pct_grid10 = round(100 * sum(matched_grid10) / .N, 2),
  pct_grid50 = round(100 * sum(matched_grid50) / .N, 2)
), by = taxonRank]

setorder(rank_summary, -n_ref_total)

# Coverage by threat status -----------------------------------------------
threat_summary <- match_summary[, .(
  n_ref_total = .N,
  n_in_gbif = sum(matched_any),
  n_missing = sum(!matched_any),
  pct_coverage = round(100 * sum(matched_any) / .N, 2)
), by = threatStatus]

setorder(threat_summary, -n_ref_total)

# Coverage by rank × threat -----------------------------------------------
rank_threat_summary <- match_summary[, .(
  n_ref_total = .N,
  n_in_gbif = sum(matched_any),
  n_missing = sum(!matched_any),
  pct_coverage = round(100 * sum(matched_any) / .N, 2)
), by = .(taxonRank, threatStatus)]

setorder(rank_threat_summary, taxonRank, threatStatus)

# Get spatial coverage for matched taxa -----------------------------------
cli_h2("Analyzing Spatial Coverage per Taxon")

# Load cell summaries
cell10_path <- here(p_derived, "cell_summary_10km.csv")
cell50_path <- here(p_derived, "cell_summary_50km.csv")

spatial_coverage <- NULL

if (file.exists(cell10_path) && file.exists(cell50_path)) {
  cell10 <- fread(cell10_path)
  cell50 <- fread(cell50_path)
  
  # Get species × cell from full species summaries (not just "all")
  # different to sp10/sp50!
  sp10_full <- fread(here(p_derived, "species_summary_10km.csv"))
  sp50_full <- fread(here(p_derived, "species_summary_50km.csv"))
  
  # Count cells per species (using "all" basis)
  sp10_cells <- sp10_full[basisofrecord == "all" & occurrences > 0, .(
    n_cells_10km = .N,
    total_occ_10km = sum(occurrences)
  ), by = .(specieskey, species)]
  
  sp50_cells <- sp50_full[basisofrecord == "all" & occurrences > 0, .(
    n_cells_50km = .N,
    total_occ_50km = sum(occurrences)
  ), by = .(specieskey, species)]
  
  # Merge and add to match table
  sp_cells <- merge(sp10_cells, sp50_cells, 
                    by = c("specieskey", "species"), 
                    all = TRUE)
  sp_cells[is.na(n_cells_10km), n_cells_10km := 0]
  sp_cells[is.na(n_cells_50km), n_cells_50km := 0]
  sp_cells[is.na(total_occ_10km), total_occ_10km := 0]
  sp_cells[is.na(total_occ_50km), total_occ_50km := 0]
  
  sp_cells[, scientific_std := standardize_name(species)]
  
  # Join with taxonomy
  spatial_coverage <- merge(
    match_summary[matched_any == TRUE, .(
      taxonID, scientificName, scientific_std, taxonRank, threatStatus,
      family, order, establishmentMeans
    )],
    sp_cells[, .(scientific_std, n_cells_10km, n_cells_50km, 
                 total_occ_10km, total_occ_50km)],
    by = "scientific_std",
    all.x = TRUE
  )
  
  spatial_coverage[is.na(n_cells_10km), n_cells_10km := 0]
  spatial_coverage[is.na(n_cells_50km), n_cells_50km := 0]
  
  # Flag poorly sampled
  spatial_coverage[, poorly_sampled_spatial := (
    n_cells_10km < MIN_CELLS & n_cells_50km < MIN_CELLS
  )]
  
  spatial_coverage[, poorly_sampled_abundance := (
    total_occ_10km < MIN_OCCURRENCES & total_occ_50km < MIN_OCCURRENCES
  )]
  
  cli_alert_success("Spatial coverage computed for matched taxa")
}

# Coverage by basis of record ---------------------------------------------
cli_h2("Analyzing Coverage by Basis of Record")

# Get species counts per basis
sp10_by_basis <- sp10[, .(
  n_species = uniqueN(specieskey[occurrences > 0])
), by = basisofrecord]
sp10_by_basis[, grid := "grid10km"]

sp50_by_basis <- sp50[, .(
  n_species = uniqueN(specieskey[occurrences > 0])
), by = basisofrecord]
sp50_by_basis[, grid := "grid50km"]

basis_coverage <- rbindlist(list(sp10_by_basis, sp50_by_basis))
setorder(basis_coverage, grid, -n_species)

# Coverage by higher taxonomy ---------------------------------------------
cli_h2("Analyzing Coverage by Higher Taxonomy")

# By family
if ("family" %in% names(match_summary)) {
  family_summary <- match_summary[!is.na(family) & family != "", .(
    n_taxa = .N,
    n_in_gbif = sum(matched_any),
    pct_coverage = round(100 * sum(matched_any) / .N, 2),
    n_threatened = sum(!is.na(threatStatus) & threatStatus %in% threatened_codes),
    n_threatened_in_gbif = sum(matched_any & !is.na(threatStatus) & 
                                 threatStatus %in% threatened_codes)
  ), by = family]
  
  setorder(family_summary, -n_taxa)
} else {
  family_summary <- NULL
  cli_alert_info("Family data not available")
}

# By order
if ("order" %in% names(match_summary)) {
  order_summary <- match_summary[!is.na(order) & order != "", .(
    n_taxa = .N,
    n_in_gbif = sum(matched_any),
    pct_coverage = round(100 * sum(matched_any) / .N, 2),
    n_threatened = sum(!is.na(threatStatus) & threatStatus %in% threatened_codes),
    n_threatened_in_gbif = sum(matched_any & !is.na(threatStatus) & 
                                 threatStatus %in% threatened_codes)
  ), by = order]
  
  setorder(order_summary, -n_taxa)
} else {
  order_summary <- NULL
  cli_alert_info("Order data not available")
}

# Priority taxa for targeted sampling -------------------------------------
cli_h2("Identifying Priority Taxa")

# Priority 1: Threatened taxa missing from GBIF
priority_threatened <- missing_threatened[, .(
  taxonID, scientificName, taxonRank, threatStatus,
  family, order, establishmentMeans,
  priority = "Threatened - Not in GBIF"
)]

# Priority 2: Threatened taxa with poor spatial coverage
if (!is.null(spatial_coverage)) {
  priority_poorly_sampled <- spatial_coverage[
    threatStatus %in% threatened_codes & 
      (poorly_sampled_spatial | poorly_sampled_abundance),
    .(taxonID, scientificName, taxonRank, threatStatus,
      family, order, establishmentMeans,
      n_cells_10km, n_cells_50km, 
      total_occ_10km, total_occ_50km,
      priority = "Threatened - Poorly Sampled")
  ]
} else {
  priority_poorly_sampled <- NULL
}

# Combine priorities
priority_all <- rbindlist(list(
  priority_threatened,
  priority_poorly_sampled
), fill = TRUE)

setorder(priority_all, threatStatus, scientificName)

# Write outputs -----------------------------------------------------------
cli_h2("Writing Taxonomic Gap Outputs")

# Core matching results
fwrite(match_table, here(p_gaps, "taxonomic_match_table.csv"))
cli_alert_success("taxonomic_match_table.csv")

fwrite(match_summary, here(p_gaps, "taxonomic_match_summary.csv"))
cli_alert_success("taxonomic_match_summary.csv")

# Missing taxa
fwrite(missing_taxa, here(p_gaps, "taxonomic_missing_taxa.csv"))
cli_alert_success("taxonomic_missing_taxa.csv ({nrow(missing_taxa)} taxa)")

fwrite(missing_threatened, here(p_gaps, "taxonomic_missing_threatened.csv"))
cli_alert_success("taxonomic_missing_threatened.csv ({nrow(missing_threatened)} taxa)")

# Coverage summaries
fwrite(rank_summary, here(p_gaps, "taxonomic_coverage_by_rank.csv"))
cli_alert_success("taxonomic_coverage_by_rank.csv")

fwrite(threat_summary, here(p_gaps, "taxonomic_coverage_by_threat.csv"))
cli_alert_success("taxonomic_coverage_by_threat.csv")

fwrite(rank_threat_summary, here(p_gaps, "taxonomic_gap_summary.csv"))
cli_alert_success("taxonomic_gap_summary.csv")

# Basis coverage
fwrite(basis_coverage, here(p_gaps, "taxonomic_coverage_by_basis.csv"))
cli_alert_success("taxonomic_coverage_by_basis.csv")

# Spatial coverage
if (!is.null(spatial_coverage)) {
  fwrite(spatial_coverage, here(p_gaps, "taxonomic_spatial_coverage.csv"))
  cli_alert_success("taxonomic_spatial_coverage.csv")
  
  # Threatened species spatial coverage specifically
  threatened_spatial <- spatial_coverage[threatStatus %in% threatened_codes]
  fwrite(threatened_spatial, here(p_gaps, "taxonomic_threatened_spatial_coverage.csv"))
  cli_alert_success("taxonomic_threatened_spatial_coverage.csv")
}

# Higher taxonomy
if (!is.null(family_summary)) {
  fwrite(family_summary, here(p_gaps, "taxonomic_gaps_by_family.csv"))
  cli_alert_success("taxonomic_gaps_by_family.csv")
}

if (!is.null(order_summary)) {
  fwrite(order_summary, here(p_gaps, "taxonomic_gaps_by_order.csv"))
  cli_alert_success("taxonomic_gaps_by_order.csv")
}

# Priority taxa
fwrite(priority_all, here(p_gaps, "taxonomic_priority_taxa.csv"))
cli_alert_success("taxonomic_priority_taxa.csv ({nrow(priority_all)} taxa)")

# Summary statistics ------------------------------------------------------
cli_h2("Summary Statistics")

summary_table <- tibble::tribble(
  ~metric, ~value,
  "Taxa in reference", scales::comma(nrow(tax_clean)),
  "Taxa matched to GBIF", scales::comma(sum(match_summary$matched_any)),
  "Coverage %", paste0(round(100 * sum(match_summary$matched_any) / nrow(match_summary), 1), "%"),
  "Threatened taxa in ref", scales::comma(sum(match_summary$threatStatus %in% threatened_codes, na.rm = TRUE)),
  "Threatened in GBIF", scales::comma(sum(match_summary$matched_any & match_summary$threatStatus %in% threatened_codes, na.rm = TRUE)),
  "Priority taxa", scales::comma(nrow(priority_all))
)

print(summary_table)

cli_alert_success("Taxonomic gap analysis complete!")
cli_alert_info("Output location: {.path {p_gaps}}")

# Count output files
n_outputs <- 10 + 
  (!is.null(spatial_coverage)) * 2 + 
  (!is.null(family_summary)) + 
  (!is.null(order_summary))

cli_alert_info("Created {n_outputs} taxonomic gap files")
