# scripts/09_taxonomic_gaps.R
# ==============================================================================
# Taxonomic Gap Analysis
# ==============================================================================
# This script identifies taxonomic gaps by comparing GBIF data to reference 
# taxonomies (national backbone as primary, Red List for threat status).
#
# INPUTS:
#   - data_proc/taxa_reference_current.rds (from 03, backbone + Red List)
#   - data_proc/derived/by_order/species_summary_*.csv (from 06b)
#   - data_proc/derived/by_family/species_summary_*.csv (from 06b)
#
# OUTPUTS (in data_proc/gaps/):
#   Matching results:
#     - taxonomic_match_table.csv       Full match details
#     - taxonomic_match_summary.csv     Summary per taxon
#
#   Missing taxa:
#     - taxonomic_missing_taxa.csv      Taxa not found in GBIF
#     - taxonomic_missing_threatened.csv Threatened taxa not in GBIF
#
#   Coverage:
#     - taxonomic_coverage_by_rank.csv  Coverage % by taxonomic rank
#     - taxonomic_coverage_by_threat.csv Coverage % by threat status
#     - taxonomic_gap_summary.csv       Rank × threat coverage matrix
#     - taxonomic_coverage_by_basis.csv Species counts by basis of record
#
#   Spatial:
#     - taxonomic_spatial_coverage.csv  Cells per matched species
#     - taxonomic_threatened_spatial_coverage.csv For threatened only
#
#   Higher taxonomy:
#     - taxonomic_gaps_by_family.csv    Coverage by family
#     - taxonomic_gaps_by_order.csv     Coverage by order
#
#   Priority:
#     - taxonomic_priority_taxa.csv     Priority taxa for targeted sampling
#
# REFERENCE TAXONOMY:
#   - National Taxonomy: Primary taxonomic backbone (via config)
#   - National Red List: Threat status (CR, EN, VU, NT, LC, etc.)
#   - Dual threat status columns: threatStatus_dyntaxa + threatStatus_redlist

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

# ===========================================================================
# CONFIGURATION
# ===========================================================================

p_derived <- here(p_data_proc, "derived")
p_gaps <- here(p_data_proc, "gaps")

dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# Thresholds for "poorly sampled" definition
MIN_OCCURRENCES <- cfg_get("parameters.taxonomic.min_occurrences", 10)
MIN_CELLS <- cfg_get("parameters.taxonomic.min_cells", 5)

# Threatened categories to flag
THREATENED_CODES <- c("CR", "EN", "VU", "NT")  # Critical, Endangered, Vulnerable, Near Threatened

cli_h1("Taxonomic Gap Analysis (Script 09)")
cli_alert_info("Poorly sampled thresholds: <{MIN_OCCURRENCES} occurrences OR <{MIN_CELLS} cells")
cli_alert_info("Threatened categories: {paste(THREATENED_CODES, collapse = ', ')}")

# ===========================================================================
# HELPER FUNCTIONS
# ===========================================================================

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

#' Load species summaries from by_order and by_family directories
load_species_summaries <- function(p_derived, grid_suffix) {
  
  by_order_dir <- here(p_derived, "by_order", "species_summary")
  by_family_dir <- here(p_derived, "by_family", "species_summary")
  
  all_files <- c()
  
  # Find files matching grid suffix
  if (dir.exists(by_order_dir)) {
    order_files <- list.files(by_order_dir, pattern = glue("_{grid_suffix}\\.csv$"), full.names = TRUE)
    all_files <- c(all_files, order_files)
  }
  
  if (dir.exists(by_family_dir)) {
    family_files <- list.files(by_family_dir, pattern = glue("_{grid_suffix}\\.csv$"), full.names = TRUE)
    all_files <- c(all_files, family_files)
  }
  
  if (length(all_files) == 0) {
    cli_alert_warning("No species summary files found for {grid_suffix}")
    return(NULL)
  }
  
  cli_alert_info("Found {length(all_files)} species summary files for {grid_suffix}")
  
  # Read and combine
  all_data <- rbindlist(lapply(all_files, fread), fill = TRUE)
  
  all_data
}

# ===========================================================================
# LOAD REFERENCE TAXONOMY (National Backbone)
# ===========================================================================

cli_h2("Loading Reference Taxonomy")

# Primary backbone: National taxonomy (via taxa_reference from script 03)
dyntaxa_path <- here(p_data_proc, "taxa_reference_current.rds")

if (!file.exists(dyntaxa_path)) {
  cli_abort(c(
    "Taxa reference not found: {.path {dyntaxa_path}}",
    "i" = "Run script 03_ingest_taxonomy.R first"
  ))
}

backbone <- readRDS(dyntaxa_path)
backbone <- as.data.table(backbone)

cli_alert_success("Loaded backbone: {scales::comma(nrow(backbone))} taxa")

# Check available columns
backbone_cols <- names(backbone)
cli_alert_info("Backbone columns: {paste(head(backbone_cols, 15), collapse = ', ')}")

# Identify key columns (may vary by export format)
# Common patterns: scientificName, taxonRank, taxonID/TaxonId, etc.
name_col <- intersect(c("scientificName", "ScientificName", "scientific_name", "canonicalName"), backbone_cols)[1]
rank_col <- intersect(c("taxonRank", "TaxonRank", "taxon_rank", "Rank"), backbone_cols)[1]
id_col <- intersect(c("taxonID", "TaxonId", "taxon_id", "id", "Id"), backbone_cols)[1]

if (is.na(name_col)) {
  cli_abort("Could not find scientific name column in backbone")
}

cli_alert_info("Using name column: {name_col}")
cli_alert_info("Using rank column: {rank_col}")
cli_alert_info("Using ID column: {id_col}")

# Identify threat status columns (from script 03 output)
# Script 03 creates: threatStatus_dyntaxa, threatStatus_redlist
threat_cols <- intersect(
  c("threatStatus_redlist", "threatStatus_dyntaxa",  # From script 03
    "dyntaxa_redlist_category", "swedish_redlist_category",  # Legacy names
    "redlistCategory", "threatStatus", "RedlistCategory", "conservation_status"),
  backbone_cols
)

if (length(threat_cols) > 0) {
  cli_alert_info("Threat status columns found: {paste(threat_cols, collapse = ', ')}")
} else {
  cli_alert_warning("No threat status columns found in backbone reference")
}

# Create standardized reference
tax_ref <- copy(backbone)

# Rename columns to standard names
if (!is.na(name_col) && name_col != "scientificName") {
  setnames(tax_ref, name_col, "scientificName")
}
if (!is.na(rank_col) && rank_col != "taxonRank") {
  setnames(tax_ref, rank_col, "taxonRank")
}
if (!is.na(id_col) && id_col != "taxonID") {
  setnames(tax_ref, id_col, "taxonID")
}

# Create primary threat status column
# Priority: threatStatus_redlist > threatStatus_dyntaxa > legacy columns
if ("threatStatus_redlist" %in% names(tax_ref)) {
  # Use Red List as primary (most authoritative for national assessment)
  tax_ref[, threatStatus := threatStatus_redlist]
  cli_alert_info("Using threatStatus_redlist as primary threat status")
  
  # Count non-empty values
  n_with_threat <- sum(!is.na(tax_ref$threatStatus) & tax_ref$threatStatus != "", na.rm = TRUE)
  cli_alert_info("Taxa with Red List threat status: {scales::comma(n_with_threat)}")
  
} else if ("threatStatus_dyntaxa" %in% names(tax_ref)) {
  tax_ref[, threatStatus := threatStatus_dyntaxa]
  cli_alert_info("Using threatStatus_dyntaxa as primary threat status")
  
} else if ("dyntaxa_redlist_category" %in% names(tax_ref)) {
  tax_ref[, threatStatus := dyntaxa_redlist_category]
  cli_alert_info("Using dyntaxa_redlist_category as primary threat status")
  
} else if ("swedish_redlist_category" %in% names(tax_ref)) {
  tax_ref[, threatStatus := swedish_redlist_category]
  cli_alert_info("Using swedish_redlist_category as primary threat status")
  
} else if (length(threat_cols) > 0) {
  tax_ref[, threatStatus := get(threat_cols[1])]
  cli_alert_info("Using {threat_cols[1]} as primary threat status")
  
} else {
  tax_ref[, threatStatus := NA_character_]
  cli_alert_warning("No threat status data available - threatStatus will be NA for all taxa")
}

# Report threat status coverage
if ("threatStatus" %in% names(tax_ref)) {
  threat_summary <- tax_ref[!is.na(threatStatus) & threatStatus != "", .N, by = threatStatus]
  if (nrow(threat_summary) > 0) {
    setorder(threat_summary, -N)
    cli_alert_info("Threat status breakdown:")
    print(threat_summary)
  }
}

# Standardize names for matching
tax_ref[, scientific_std := standardize_name(scientificName)]

# Select key columns for analysis
keep_cols <- intersect(
  c("taxonID", "scientificName", "scientific_std", "taxonRank", "threatStatus",
    "dyntaxa_redlist_category", "swedish_redlist_category",
    "kingdom", "phylum", "class", "order", "family", "genus",
    "Kingdom", "Phylum", "Class", "Order", "Family", "Genus"),
  names(tax_ref)
)

tax_clean <- tax_ref[, ..keep_cols]

# Standardize case for higher taxonomy columns
for (col in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")) {
  lower_col <- tolower(col)
  if (col %in% names(tax_clean) && !(lower_col %in% names(tax_clean))) {
    setnames(tax_clean, col, lower_col)
  }
}

cli_alert_success("Prepared {scales::comma(nrow(tax_clean))} taxa for matching")

# Filter to species rank if available
if ("taxonRank" %in% names(tax_clean)) {
  species_ranks <- c("species", "Species", "SPECIES", "subspecies", "Subspecies", "variety", "form")
  n_species <- sum(tax_clean$taxonRank %in% species_ranks, na.rm = TRUE)
  cli_alert_info("Species-rank taxa: {scales::comma(n_species)}")
}

# ===========================================================================
# LOAD GBIF SPECIES DATA
# ===========================================================================

cli_h2("Loading GBIF Species Data")

sp10 <- load_species_summaries(p_derived, "10km")
sp50 <- load_species_summaries(p_derived, "50km")

if (is.null(sp10) && is.null(sp50)) {
  cli_abort(c(
    "No species summary files found",
    "i" = "Run script 06b_make_species_summaries.R first"
  ))
}

if (!is.null(sp10)) cli_alert_success("Loaded 10km species: {scales::comma(nrow(sp10))} rows")
if (!is.null(sp50)) cli_alert_success("Loaded 50km species: {scales::comma(nrow(sp50))} rows")

# ===========================================================================
# PREPARE GBIF SPECIES LISTS
# ===========================================================================

cli_h2("Preparing GBIF Species Lists")

extract_species <- function(sp_data, grid_label) {
  if (is.null(sp_data)) return(NULL)
  
  dt <- as.data.table(sp_data)
  
  # Filter to "all" basis if present
  if ("basisofrecord" %in% names(dt)) {
    dt <- dt[basisofrecord == "all"]
  }
  
  # Standardize species names
  if ("species" %in% names(dt)) {
    dt[, species_std := standardize_name(species)]
  } else if ("scientificName" %in% names(dt)) {
    dt[, species_std := standardize_name(scientificName)]
    setnames(dt, "scientificName", "species")
  } else {
    cli_abort("No species or scientificName column in GBIF data")
  }
  
  dt[, grid := grid_label]
  
  # Get key columns
  key_cols <- intersect(c("grid", "specieskey", "species", "species_std", "occurrences"), names(dt))
  
  unique(dt[, ..key_cols])
}

cube_species_10 <- extract_species(sp10, "grid10km")
cube_species_50 <- extract_species(sp50, "grid50km")

# Combined species list
cube_species_combined <- rbindlist(list(cube_species_10, cube_species_50), fill = TRUE)
cube_species_combined <- unique(cube_species_combined)

n_unique_species <- if ("specieskey" %in% names(cube_species_combined)) {
  uniqueN(cube_species_combined$specieskey)
} else {
  uniqueN(cube_species_combined$species_std)
}

cli_alert_info("Unique species in GBIF cubes: {scales::comma(n_unique_species)}")

# ===========================================================================
# MATCH GBIF TO REFERENCE TAXONOMY
# ===========================================================================

cli_h2("Matching GBIF to Reference Taxonomy")

# Deduplicate GBIF species - aggregate occurrences per species_std × grid
gbif_for_match <- cube_species_combined[, .(
  species = first(species),
  occurrences = sum(occurrences, na.rm = TRUE)
), by = .(species_std, grid)]

cli_alert_info("GBIF species for matching: {scales::comma(nrow(gbif_for_match))} rows")

# Deduplicate taxonomy reference - keep first occurrence per scientific_std
tax_for_match <- unique(tax_clean, by = "scientific_std")
cli_alert_info("Reference taxa for matching: {scales::comma(nrow(tax_for_match))} rows")

# Match on standardized names
match_table <- merge(
  tax_for_match,
  gbif_for_match,
  by.x = "scientific_std",
  by.y = "species_std",
  all.x = TRUE
)

match_table[, matched := !is.na(species)]

cli_alert_info("Matching results:")
cli_alert_info("  Reference taxa: {scales::comma(nrow(tax_for_match))}")
cli_alert_info("  Matched to GBIF: {scales::comma(sum(match_table$matched))}")
cli_alert_info("  Unmatched: {scales::comma(sum(!match_table$matched))}")

# ===========================================================================
# SUMMARIZE MATCHES
# ===========================================================================

cli_h2("Summarizing Match Results")

# Aggregate by taxon (a taxon may match to multiple grids)
# Get the grouping columns from tax_for_match
group_cols <- setdiff(names(tax_for_match), "scientific_std")
group_cols <- c(group_cols, "scientific_std")

match_summary <- match_table[, .(
  matched_any = any(matched),
  matched_grid10 = any(matched & grid == "grid10km", na.rm = TRUE),
  matched_grid50 = any(matched & grid == "grid50km", na.rm = TRUE),
  n_grids = uniqueN(grid[matched], na.rm = TRUE),
  total_occurrences = sum(occurrences[matched], na.rm = TRUE)
), by = group_cols]

# ===========================================================================
# IDENTIFY GAPS
# ===========================================================================

cli_h2("Identifying Taxonomic Gaps")

# Missing taxa (not in GBIF cubes)
missing_taxa <- match_summary[matched_any == FALSE]
cli_alert_warning("Missing taxa: {scales::comma(nrow(missing_taxa))}")

# Missing threatened taxa
missing_threatened <- missing_taxa[threatStatus %in% THREATENED_CODES]
cli_alert_warning("Missing threatened taxa: {scales::comma(nrow(missing_threatened))}")

# ===========================================================================
# COVERAGE BY TAXONOMIC RANK
# ===========================================================================

cli_h2("Analyzing Coverage by Rank")

if ("taxonRank" %in% names(match_summary)) {
  rank_summary <- match_summary[, .(
    n_ref_total = .N,
    n_in_gbif = sum(matched_any),
    n_missing = sum(!matched_any),
    pct_coverage = round(100 * sum(matched_any) / .N, 2),
    n_grid10 = sum(matched_grid10, na.rm = TRUE),
    n_grid50 = sum(matched_grid50, na.rm = TRUE)
  ), by = taxonRank]
  
  setorder(rank_summary, -n_ref_total)
} else {
  rank_summary <- data.table(
    taxonRank = "unknown",
    n_ref_total = nrow(match_summary),
    n_in_gbif = sum(match_summary$matched_any),
    n_missing = sum(!match_summary$matched_any),
    pct_coverage = round(100 * mean(match_summary$matched_any), 2)
  )
}

# ===========================================================================
# COVERAGE BY THREAT STATUS
# ===========================================================================

cli_h2("Analyzing Coverage by Threat Status")

threat_summary <- match_summary[, .(
  n_ref_total = .N,
  n_in_gbif = sum(matched_any),
  n_missing = sum(!matched_any),
  pct_coverage = round(100 * sum(matched_any) / .N, 2)
), by = threatStatus]

setorder(threat_summary, -n_ref_total)

# Rank × threat matrix
rank_threat_summary <- match_summary[, .(
  n_ref_total = .N,
  n_in_gbif = sum(matched_any),
  n_missing = sum(!matched_any),
  pct_coverage = round(100 * sum(matched_any) / .N, 2)
), by = .(taxonRank, threatStatus)]

# ===========================================================================
# SPATIAL COVERAGE PER TAXON
# ===========================================================================

cli_h2("Analyzing Spatial Coverage per Taxon")

# Load species × cell summaries
load_species_cell <- function(p_derived, grid_suffix) {
  by_order_dir <- here(p_derived, "by_order", "species_cell")
  by_family_dir <- here(p_derived, "by_family", "species_cell")
  
  all_files <- c()
  
  if (dir.exists(by_order_dir)) {
    all_files <- c(all_files, list.files(by_order_dir, pattern = glue("_{grid_suffix}\\.csv$"), full.names = TRUE))
  }
  if (dir.exists(by_family_dir)) {
    all_files <- c(all_files, list.files(by_family_dir, pattern = glue("_{grid_suffix}\\.csv$"), full.names = TRUE))
  }
  
  if (length(all_files) == 0) return(NULL)
  
  rbindlist(lapply(all_files, fread), fill = TRUE)
}

spatial_coverage <- NULL

sp_cell_10 <- load_species_cell(p_derived, "10km")
sp_cell_50 <- load_species_cell(p_derived, "50km")

if (!is.null(sp_cell_10) || !is.null(sp_cell_50)) {
  
  # Count cells per species
  count_cells <- function(dt, grid_label) {
    if (is.null(dt)) return(NULL)
    
    dt <- as.data.table(dt)
    if ("basisofrecord" %in% names(dt)) {
      dt <- dt[basisofrecord == "all"]
    }
    
    # Get species name column
    sp_col <- intersect(c("species", "scientificName"), names(dt))[1]
    if (is.na(sp_col)) return(NULL)
    
    dt[occurrences > 0, .(
      n_cells = uniqueN(eeacellcode),
      total_occ = sum(occurrences, na.rm = TRUE)
    ), by = sp_col]
  }
  
  cells_10 <- count_cells(sp_cell_10, "10km")
  cells_50 <- count_cells(sp_cell_50, "50km")
  
  if (!is.null(cells_10)) {
    setnames(cells_10, c("species", "n_cells_10km", "total_occ_10km"))
    cells_10[, species_std := standardize_name(species)]
  }
  
  if (!is.null(cells_50)) {
    setnames(cells_50, c("species", "n_cells_50km", "total_occ_50km"))
    cells_50[, species_std := standardize_name(species)]
  }
  
  # Merge cell counts
  if (!is.null(cells_10) && !is.null(cells_50)) {
    sp_cells <- merge(cells_10, cells_50, by = "species_std", all = TRUE, suffixes = c("", ".y"))
    sp_cells[, species.y := NULL]
  } else if (!is.null(cells_10)) {
    sp_cells <- cells_10
    sp_cells[, n_cells_50km := NA_integer_]
    sp_cells[, total_occ_50km := NA_real_]
  } else {
    sp_cells <- cells_50
    sp_cells[, n_cells_10km := NA_integer_]
    sp_cells[, total_occ_10km := NA_real_]
  }
  
  sp_cells[is.na(n_cells_10km), n_cells_10km := 0]
  sp_cells[is.na(n_cells_50km), n_cells_50km := 0]
  sp_cells[is.na(total_occ_10km), total_occ_10km := 0]
  sp_cells[is.na(total_occ_50km), total_occ_50km := 0]
  
  # Join with taxonomy
  spatial_coverage <- merge(
    match_summary[matched_any == TRUE, .(
      taxonID, scientificName, scientific_std, taxonRank, threatStatus,
      family = if ("family" %in% names(match_summary)) family else NA_character_,
      order = if ("order" %in% names(match_summary)) order else NA_character_
    )],
    sp_cells[, .(species_std, n_cells_10km, n_cells_50km, total_occ_10km, total_occ_50km)],
    by.x = "scientific_std",
    by.y = "species_std",
    all.x = TRUE
  )
  
  spatial_coverage[is.na(n_cells_10km), n_cells_10km := 0]
  spatial_coverage[is.na(n_cells_50km), n_cells_50km := 0]
  
  # Flag poorly sampled
  spatial_coverage[, poorly_sampled_spatial := (n_cells_10km < MIN_CELLS & n_cells_50km < MIN_CELLS)]
  spatial_coverage[, poorly_sampled_abundance := (total_occ_10km < MIN_OCCURRENCES & total_occ_50km < MIN_OCCURRENCES)]
  
  cli_alert_success("Spatial coverage computed for {scales::comma(nrow(spatial_coverage))} taxa")
}

# ===========================================================================
# COVERAGE BY BASIS OF RECORD
# ===========================================================================

cli_h2("Analyzing Coverage by Basis of Record")

get_species_by_basis <- function(sp_data, grid_label) {
  if (is.null(sp_data)) return(NULL)
  
  dt <- as.data.table(sp_data)
  if (!("basisofrecord" %in% names(dt))) return(NULL)
  
  sp_col <- intersect(c("species", "scientificName", "specieskey"), names(dt))[1]
  if (is.na(sp_col)) return(NULL)
  
  dt[occurrences > 0, .(
    n_species = uniqueN(get(sp_col))
  ), by = basisofrecord][, grid := grid_label]
}

basis_10 <- get_species_by_basis(sp10, "grid10km")
basis_50 <- get_species_by_basis(sp50, "grid50km")

basis_coverage <- rbindlist(list(basis_10, basis_50), fill = TRUE)
if (nrow(basis_coverage) > 0) {
  setorder(basis_coverage, grid, -n_species)
}

# ===========================================================================
# COVERAGE BY HIGHER TAXONOMY
# ===========================================================================

cli_h2("Analyzing Coverage by Higher Taxonomy")

# By family (include full taxonomy hierarchy for app filtering)
family_summary <- NULL
if ("family" %in% names(match_summary)) {
  # Determine which hierarchy columns are available
  hierarchy_cols <- intersect(c("kingdom", "phylum", "class", "order"), names(match_summary))
  group_cols <- c(hierarchy_cols, "family")
  
  family_summary <- match_summary[!is.na(family) & family != "", .(
    n_taxa = .N,
    n_in_gbif = sum(matched_any),
    pct_coverage = round(100 * sum(matched_any) / .N, 2),
    n_threatened = sum(threatStatus %in% THREATENED_CODES, na.rm = TRUE),
    n_threatened_in_gbif = sum(matched_any & threatStatus %in% THREATENED_CODES, na.rm = TRUE)
  ), by = group_cols]
  
  setorder(family_summary, -n_taxa)
  cli_alert_success("Family summary: {scales::comma(nrow(family_summary))} families (grouped by {paste(group_cols, collapse = ', ')})")
}

# By order (include full taxonomy hierarchy for app filtering)
order_summary <- NULL
if ("order" %in% names(match_summary)) {
  hierarchy_cols <- intersect(c("kingdom", "phylum", "class"), names(match_summary))
  group_cols <- c(hierarchy_cols, "order")
  
  order_summary <- match_summary[!is.na(order) & order != "", .(
    n_taxa = .N,
    n_in_gbif = sum(matched_any),
    pct_coverage = round(100 * sum(matched_any) / .N, 2),
    n_threatened = sum(threatStatus %in% THREATENED_CODES, na.rm = TRUE),
    n_threatened_in_gbif = sum(matched_any & threatStatus %in% THREATENED_CODES, na.rm = TRUE)
  ), by = group_cols]
  
  setorder(order_summary, -n_taxa)
  cli_alert_success("Order summary: {scales::comma(nrow(order_summary))} orders (grouped by {paste(group_cols, collapse = ', ')})")
}

# ===========================================================================
# PRIORITY TAXA
# ===========================================================================

cli_h2("Identifying Priority Taxa")

# Priority 1: Threatened taxa missing from GBIF
priority_threatened <- missing_threatened[, .(
  taxonID, scientificName, taxonRank, threatStatus,
  family = if ("family" %in% names(missing_threatened)) family else NA_character_,
  order = if ("order" %in% names(missing_threatened)) order else NA_character_,
  priority = "Threatened - Not in GBIF"
)]

# Priority 2: Threatened taxa with poor spatial coverage
priority_poorly_sampled <- NULL
if (!is.null(spatial_coverage)) {
  priority_poorly_sampled <- spatial_coverage[
    threatStatus %in% THREATENED_CODES & 
      (poorly_sampled_spatial | poorly_sampled_abundance),
    .(taxonID, scientificName, taxonRank, threatStatus,
      family, order,
      n_cells_10km, n_cells_50km, 
      total_occ_10km, total_occ_50km,
      priority = "Threatened - Poorly Sampled")
  ]
}

# Combine priorities
priority_all <- rbindlist(list(
  priority_threatened,
  priority_poorly_sampled
), fill = TRUE)

setorder(priority_all, threatStatus, scientificName)

cli_alert_success("Priority taxa identified: {scales::comma(nrow(priority_all))}")

# ===========================================================================
# WRITE OUTPUTS
# ===========================================================================

cli_h2("Writing Taxonomic Gap Outputs")

# Core matching results
fwrite(match_table, here(p_gaps, "taxonomic_match_table.csv"))
cli_alert_success("taxonomic_match_table.csv: {scales::comma(nrow(match_table))} rows")

fwrite(match_summary, here(p_gaps, "taxonomic_match_summary.csv"))
cli_alert_success("taxonomic_match_summary.csv: {scales::comma(nrow(match_summary))} rows")

# Missing taxa
fwrite(missing_taxa, here(p_gaps, "taxonomic_missing_taxa.csv"))
cli_alert_success("taxonomic_missing_taxa.csv: {scales::comma(nrow(missing_taxa))} taxa")

fwrite(missing_threatened, here(p_gaps, "taxonomic_missing_threatened.csv"))
cli_alert_success("taxonomic_missing_threatened.csv: {scales::comma(nrow(missing_threatened))} taxa")

# Coverage summaries
fwrite(rank_summary, here(p_gaps, "taxonomic_coverage_by_rank.csv"))
cli_alert_success("taxonomic_coverage_by_rank.csv")

fwrite(threat_summary, here(p_gaps, "taxonomic_coverage_by_threat.csv"))
cli_alert_success("taxonomic_coverage_by_threat.csv")

fwrite(rank_threat_summary, here(p_gaps, "taxonomic_gap_summary.csv"))
cli_alert_success("taxonomic_gap_summary.csv")

# Basis coverage
if (nrow(basis_coverage) > 0) {
  fwrite(basis_coverage, here(p_gaps, "taxonomic_coverage_by_basis.csv"))
  cli_alert_success("taxonomic_coverage_by_basis.csv")
}

# Spatial coverage
if (!is.null(spatial_coverage)) {
  fwrite(spatial_coverage, here(p_gaps, "taxonomic_spatial_coverage.csv"))
  cli_alert_success("taxonomic_spatial_coverage.csv: {scales::comma(nrow(spatial_coverage))} taxa")
  
  # Threatened only
  threatened_spatial <- spatial_coverage[threatStatus %in% THREATENED_CODES]
  fwrite(threatened_spatial, here(p_gaps, "taxonomic_threatened_spatial_coverage.csv"))
  cli_alert_success("taxonomic_threatened_spatial_coverage.csv: {scales::comma(nrow(threatened_spatial))} taxa")
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
cli_alert_success("taxonomic_priority_taxa.csv: {scales::comma(nrow(priority_all))} taxa")

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 09)")

summary_table <- data.table(
  Metric = c(
    "Reference taxa (backbone)",
    "Matched to GBIF",
    "Coverage %",
    "Threatened in reference",
    "Threatened in GBIF",
    "Priority taxa for sampling"
  ),
  Value = c(
    scales::comma(nrow(tax_clean)),
    scales::comma(sum(match_summary$matched_any)),
    paste0(round(100 * mean(match_summary$matched_any), 1), "%"),
    scales::comma(sum(match_summary$threatStatus %in% THREATENED_CODES, na.rm = TRUE)),
    scales::comma(sum(match_summary$matched_any & match_summary$threatStatus %in% THREATENED_CODES, na.rm = TRUE)),
    scales::comma(nrow(priority_all))
  )
)

print(summary_table)

cli_alert_success("Taxonomic gap analysis complete!")
cli_alert_info("Output location: {.path {p_gaps}}")

# Count output files
n_outputs <- length(list.files(p_gaps, pattern = "^taxonomic_"))
cli_alert_info("Created {n_outputs} taxonomic gap files")
cli_alert_info("Next: source('scripts/10_make_gap_overview.R')")
