# scripts/09b_taxonomic_gaps.R
# ==============================================================================
# Taxonomic Gap Analysis
# ==============================================================================
# This script identifies taxonomic gaps by comparing the reconciliation table
# (from 09a) to the national taxonomy backbone. It answers: which backbone
# taxa are present in GBIF? Which are missing? Which threatened taxa lack
# coverage?
#
# PREREQUISITES:
#   - 09a_reconcile_taxonomy.R must have been run (produces reconciliation)
#
# INPUTS:
#   - data/{CC}/proc/taxonomic_reconciliation.rds  (from 09a)
#   - data/{CC}/proc/taxa_reference_current.rds    (from 03)
#   - data/{CC}/proc/derived/by_order/species_cell_*.csv  (from 06b)
#   - data/{CC}/proc/derived/by_family/species_cell_*.csv (from 06b)
#   - data/{CC}/proc/derived/by_order/species_summary_*.csv  (from 06b)
#   - data/{CC}/proc/derived/by_family/species_summary_*.csv (from 06b)
#
# OUTPUTS (in data/{CC}/proc/gaps/):
#   Missing taxa:
#     - taxonomic_missing_taxa.csv           All backbone taxa not in GBIF
#     - taxonomic_missing_threatened.csv     Threatened backbone taxa not in GBIF
#   Coverage:
#     - taxonomic_coverage_by_rank.csv       Coverage % by taxonomic rank
#     - taxonomic_coverage_by_threat.csv     Coverage % by threat status
#     - taxonomic_gap_summary.csv            Rank x threat coverage matrix
#     - taxonomic_coverage_by_basis.csv      Species counts by basis of record
#   Spatial:
#     - taxonomic_spatial_coverage.csv       Cells per matched species
#     - taxonomic_threatened_spatial_coverage.csv  For threatened only
#   Higher taxonomy:
#     - taxonomic_gaps_by_family.csv         Coverage by family
#     - taxonomic_gaps_by_order.csv          Coverage by order
#   Priority:
#     - taxonomic_priority_taxa.csv          Priority taxa for targeted sampling
# ==============================================================================

library(here)
library(data.table)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

timer_start <- Sys.time()

# ===========================================================================
# CONFIGURATION
# ===========================================================================

# p_derived is defined in R/globals.R
p_gaps    <- here(p_data_proc, cfg_get("gaps.dir", "gaps"))
dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

MIN_OCCURRENCES <- cfg_get("parameters.taxonomic.min_occurrences", 10)
MIN_CELLS       <- cfg_get("parameters.taxonomic.min_cells", 5)
THREATENED_CODES <- c("CR", "EN", "VU", "NT")

# Grid cell code field (for spatial coverage)
CELLCODE_FIELD <- cfg_get("parameters.grid.cellcode_field", "eeacellcode")

cli_h1("09b -- Taxonomic Gap Analysis")
cli_alert_info("Poorly sampled: <{MIN_OCCURRENCES} occ OR <{MIN_CELLS} cells")
cli_alert_info("Threatened categories: {paste(THREATENED_CODES, collapse = ', ')}")


# ===========================================================================
# LOAD RECONCILIATION TABLE (from 09a)
# ===========================================================================

cli_h2("Loading Reconciliation Table")

recon_path <- here(p_data_proc, "taxonomic_reconciliation.rds")
if (!file.exists(recon_path)) {
  cli_abort(c(
    "Reconciliation table not found: {.path {recon_path}}",
    "i" = "Run script 09a_reconcile_taxonomy.R first"
  ))
}

recon <- as.data.table(readRDS(recon_path))
cli_alert_success("Loaded reconciliation: {scales::comma(nrow(recon))} GBIF species")

# Quick check
n_matched   <- sum(recon$match_tier != "unmatched")
n_unmatched <- sum(recon$match_tier == "unmatched")
cli_alert_info("Matched to backbone: {scales::comma(n_matched)}")
cli_alert_info("Unmatched: {scales::comma(n_unmatched)}")


# ===========================================================================
# LOAD BACKBONE REFERENCE (from 03)
# ===========================================================================

cli_h2("Loading Backbone Reference")

taxa_path <- here(p_data_proc, "taxa_reference_current.rds")
if (!file.exists(taxa_path)) {
  cli_abort("Backbone not found: {.path {taxa_path}}. Run script 03 first.")
}

backbone <- as.data.table(readRDS(taxa_path))
cli_alert_success("Loaded backbone: {scales::comma(nrow(backbone))} rows")

# Keep only accepted taxa (synonyms are handled by 09a's matching)
backbone[, is_accepted := (taxonID == acceptedNameUsageID)]
tax_accepted <- backbone[is_accepted == TRUE]
cli_alert_info("Accepted taxa: {scales::comma(nrow(tax_accepted))}")

# Standardise name for joining
tax_accepted[, name_std := tolower(trimws(scientificName))]

# --- Resolve threat status ---
# Priority: threatStatus_redlist > threatStatus_backbone > legacy columns
threat_col_candidates <- c(
  "threatStatus_redlist", "threatStatus_backbone",
  "dyntaxa_redlist_category", "swedish_redlist_category",
  "redlistCategory", "threatStatus"
)
threat_col <- intersect(threat_col_candidates, names(tax_accepted))

if (length(threat_col) > 0) {
  # Use first available, with redlist preferred
  primary_threat <- threat_col[1]
  tax_accepted[, threatStatus := get(primary_threat)]
  cli_alert_info("Threat status source: {primary_threat}")
  
  n_with_threat <- sum(!is.na(tax_accepted$threatStatus) &
                         tax_accepted$threatStatus != "")
  cli_alert_info("Taxa with threat status: {scales::comma(n_with_threat)}")
} else {
  tax_accepted[, threatStatus := NA_character_]
  cli_alert_warning("No threat status column found")
}

# Standardise higher taxonomy column names
for (col in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")) {
  lower <- tolower(col)
  if (col %in% names(tax_accepted) && !(lower %in% names(tax_accepted))) {
    setnames(tax_accepted, col, lower)
  }
}

# Select working columns
keep_cols <- intersect(
  c("taxonID", "scientificName", "name_std", "taxonRank", "threatStatus",
    "kingdom", "phylum", "class", "order", "family", "genus",
    "establishmentMeans", "occurrenceStatus",
    "is_invasive", "in_dyntaxa"),
  names(tax_accepted)
)
tax_clean <- tax_accepted[, ..keep_cols]

# Report rank distribution
if ("taxonRank" %in% names(tax_clean)) {
  species_ranks <- c("species", "Species", "SPECIES", "subspecies",
                     "Subspecies", "variety", "form")
  n_species_rank <- sum(tax_clean$taxonRank %in% species_ranks, na.rm = TRUE)
  cli_alert_info("Species-rank taxa: {scales::comma(n_species_rank)}")
}


# ===========================================================================
# BUILD MATCH SUMMARY: Backbone view
# ===========================================================================
# The reconciliation table is GBIF-centric (one row per GBIF species).
# The gap analysis needs the BACKBONE view: for each backbone taxon,
# is it represented in GBIF?

cli_h2("Building Backbone Match Summary")

# Create a lookup: which backbone_taxonIDs have GBIF data?
matched_taxa <- recon[match_tier != "unmatched",
                      .(backbone_taxonID,
                        gbif_specieskey = specieskey,
                        gbif_species    = species,
                        match_tier,
                        match_type,
                        gbif_total_occ  = total_occ)]

# A backbone taxon might be matched by multiple GBIF species (e.g. a
# species and its subspecies both resolve to the same accepted taxon).
# Aggregate to one row per backbone taxon.
matched_agg <- matched_taxa[, .(
  n_gbif_species = .N,
  gbif_total_occ = sum(as.numeric(gbif_total_occ), na.rm = TRUE),
  best_match_tier = match_tier[which.min(match(
    match_tier,
    c("tier1", "tier2", "tier2_ambiguous", "tier3", "tier4")
  ))],
  gbif_specieskeys = paste(unique(gbif_specieskey), collapse = ";")
), by = .(backbone_taxonID)]

# Join to backbone
match_summary <- merge(
  tax_clean,
  matched_agg,
  by.x = "taxonID", by.y = "backbone_taxonID",
  all.x = TRUE
)

match_summary[, matched_any := !is.na(n_gbif_species)]
match_summary[is.na(n_gbif_species), n_gbif_species := 0L]
match_summary[is.na(gbif_total_occ), gbif_total_occ := 0]

n_backbone_matched <- sum(match_summary$matched_any)
cli_alert_success("Backbone taxa matched: {scales::comma(n_backbone_matched)} / {scales::comma(nrow(match_summary))}")
cli_alert_info("Coverage: {round(100 * n_backbone_matched / nrow(match_summary), 1)}%")


# ===========================================================================
# IDENTIFY GAPS
# ===========================================================================

cli_h2("Identifying Taxonomic Gaps")

missing_taxa <- match_summary[matched_any == FALSE]
cli_alert_warning("Missing taxa (not in GBIF): {scales::comma(nrow(missing_taxa))}")

missing_threatened <- missing_taxa[threatStatus %in% THREATENED_CODES]
cli_alert_warning("Missing threatened taxa: {scales::comma(nrow(missing_threatened))}")

if (nrow(missing_threatened) > 0) {
  mt_summary <- missing_threatened[, .N, by = threatStatus][order(-N)]
  for (i in seq_len(nrow(mt_summary))) {
    cli_alert_info("  {mt_summary$threatStatus[i]}: {mt_summary$N[i]}")
  }
}


# ===========================================================================
# COVERAGE BY TAXONOMIC RANK
# ===========================================================================

cli_h2("Coverage by Rank")

if ("taxonRank" %in% names(match_summary)) {
  rank_summary <- match_summary[, .(
    n_ref_total  = .N,
    n_in_gbif    = sum(matched_any),
    n_missing    = sum(!matched_any),
    pct_coverage = round(100 * sum(matched_any) / .N, 2)
  ), by = taxonRank]
  setorder(rank_summary, -n_ref_total)
} else {
  rank_summary <- data.table(
    taxonRank    = "unknown",
    n_ref_total  = nrow(match_summary),
    n_in_gbif    = sum(match_summary$matched_any),
    n_missing    = sum(!match_summary$matched_any),
    pct_coverage = round(100 * mean(match_summary$matched_any), 2)
  )
}


# ===========================================================================
# COVERAGE BY THREAT STATUS
# ===========================================================================

cli_h2("Coverage by Threat Status")

threat_summary <- match_summary[, .(
  n_ref_total  = .N,
  n_in_gbif    = sum(matched_any),
  n_missing    = sum(!matched_any),
  pct_coverage = round(100 * sum(matched_any) / .N, 2)
), by = threatStatus]
setorder(threat_summary, -n_ref_total)

# Rank x threat matrix
rank_threat_summary <- match_summary[, .(
  n_ref_total  = .N,
  n_in_gbif    = sum(matched_any),
  n_missing    = sum(!matched_any),
  pct_coverage = round(100 * sum(matched_any) / .N, 2)
), by = .(taxonRank, threatStatus)]


# ===========================================================================
# COVERAGE BY ESTABLISHMENT MEANS
# ===========================================================================

if ("establishmentMeans" %in% names(match_summary)) {
  cli_h2("Coverage by Establishment Means")

  estab_summary <- match_summary[, .(
    n_ref_total  = .N,
    n_in_gbif    = sum(matched_any),
    n_missing    = sum(!matched_any),
    pct_coverage = round(100 * sum(matched_any) / .N, 2)
  ), by = establishmentMeans]
  setorder(estab_summary, -n_ref_total)
  print(estab_summary)

  # Occurrence status summary
  if ("occurrenceStatus" %in% names(match_summary)) {
    occ_status_summary <- match_summary[, .(
      n_ref_total  = .N,
      n_in_gbif    = sum(matched_any),
      n_missing    = sum(!matched_any),
      pct_coverage = round(100 * sum(matched_any) / .N, 2)
    ), by = occurrenceStatus]
    setorder(occ_status_summary, -n_ref_total)
    cli_alert_info("Coverage by occurrence status:")
    print(occ_status_summary)
  }
} else {
  cli_alert_info("No establishmentMeans column — skipping establishment means summary")
  estab_summary <- NULL
}


# ===========================================================================
# LOAD GBIF SPECIES DATA (for basis-of-record and spatial analysis)
# ===========================================================================

cli_h2("Loading GBIF Species Data for Spatial Analysis")

#' Load species summary or species_cell files
load_derived_files <- function(p_derived, file_type, grid_suffix) {
  dirs <- c(
    here(p_derived, "by_order", file_type),
    here(p_derived, "by_family", file_type)
  )
  all_files <- character(0)
  for (d in dirs) {
    if (dir.exists(d)) {
      all_files <- c(all_files,
        list.files(d, pattern = glue("_{grid_suffix}\\.csv$"),
                   full.names = TRUE))
    }
  }
  if (length(all_files) == 0) return(NULL)
  cli_alert_info("  {file_type}/{grid_suffix}: {length(all_files)} files")
  rbindlist(lapply(all_files, fread), fill = TRUE)
}

sp10 <- load_derived_files(p_derived, "species_summary", "10km")
sp50 <- load_derived_files(p_derived, "species_summary", "50km")


# ===========================================================================
# COVERAGE BY BASIS OF RECORD
# ===========================================================================

cli_h2("Coverage by Basis of Record")

get_species_by_basis <- function(sp_data, grid_label) {
  if (is.null(sp_data)) return(NULL)
  dt <- as.data.table(sp_data)
  if (!("basisofrecord" %in% names(dt))) return(NULL)
  sp_col <- intersect(c("specieskey", "species"), names(dt))[1]
  if (is.na(sp_col)) return(NULL)
  dt[occurrences > 0, .(
    n_species = uniqueN(get(sp_col))
  ), by = basisofrecord][, grid := grid_label]
}

basis_10 <- get_species_by_basis(sp10, "grid10km")
basis_50 <- get_species_by_basis(sp50, "grid50km")
basis_coverage <- rbindlist(list(basis_10, basis_50), fill = TRUE)
if (nrow(basis_coverage) > 0) setorder(basis_coverage, grid, -n_species)


# ===========================================================================
# SPATIAL COVERAGE PER TAXON
# ===========================================================================

cli_h2("Spatial Coverage per Taxon")

sp_cell_10 <- load_derived_files(p_derived, "species_cell", "10km")
sp_cell_50 <- load_derived_files(p_derived, "species_cell", "50km")

spatial_coverage <- NULL

if (!is.null(sp_cell_10) || !is.null(sp_cell_50)) {

  count_cells <- function(dt, grid_label) {
    if (is.null(dt)) return(NULL)
    dt <- as.data.table(dt)
    if ("basisofrecord" %in% names(dt)) dt <- dt[basisofrecord == "all"]

    # Use specieskey as the identifier (more reliable than name)
    if (!("specieskey" %in% names(dt))) return(NULL)

    # Use the configured cellcode field, falling back to eeacellcode
    cell_col <- intersect(c(CELLCODE_FIELD, "eeacellcode"), names(dt))[1]
    if (is.na(cell_col)) {
      cli_alert_warning("  No cell code column found in species_cell/{grid_label}")
      return(NULL)
    }

    dt[occurrences > 0, .(
      n_cells   = uniqueN(get(cell_col)),
      total_occ = sum(as.numeric(occurrences), na.rm = TRUE)
    ), by = specieskey]
  }

  cells_10 <- count_cells(sp_cell_10, "10km")
  cells_50 <- count_cells(sp_cell_50, "50km")

  if (!is.null(cells_10)) setnames(cells_10, c("specieskey", "n_cells_10km", "total_occ_10km"))
  if (!is.null(cells_50)) setnames(cells_50, c("specieskey", "n_cells_50km", "total_occ_50km"))

  # Merge cell counts
  if (!is.null(cells_10) && !is.null(cells_50)) {
    sp_cells <- merge(cells_10, cells_50, by = "specieskey", all = TRUE)
  } else if (!is.null(cells_10)) {
    sp_cells <- copy(cells_10)
    sp_cells[, `:=`(n_cells_50km = NA_integer_, total_occ_50km = NA_real_)]
  } else {
    sp_cells <- copy(cells_50)
    sp_cells[, `:=`(n_cells_10km = NA_integer_, total_occ_10km = NA_real_)]
  }

  for (col in c("n_cells_10km", "n_cells_50km", "total_occ_10km", "total_occ_50km")) {
    sp_cells[is.na(get(col)), (col) := 0]
  }

  # Join cell counts to matched backbone taxa via reconciliation
  # Step 1: map specieskey -> backbone_taxonID
  sk_to_taxon <- recon[match_tier != "unmatched",
                       .(specieskey, backbone_taxonID)]

  sp_cells_with_taxon <- merge(sp_cells, sk_to_taxon, by = "specieskey", all.x = TRUE)

  # Step 2: aggregate per backbone taxon (multiple specieskeys may map to same taxon)
  sp_cells_agg <- sp_cells_with_taxon[!is.na(backbone_taxonID), .(
    n_cells_10km   = max(n_cells_10km, na.rm = TRUE),
    n_cells_50km   = max(n_cells_50km, na.rm = TRUE),
    total_occ_10km = sum(total_occ_10km, na.rm = TRUE),
    total_occ_50km = sum(total_occ_50km, na.rm = TRUE)
  ), by = backbone_taxonID]

  # Step 3: join to backbone match summary
  spatial_cols <- intersect(
    c("taxonID", "scientificName", "taxonRank", "threatStatus", "family", "order"),
    names(match_summary)
  )

  spatial_coverage <- merge(
    match_summary[matched_any == TRUE, ..spatial_cols],
    sp_cells_agg,
    by.x = "taxonID", by.y = "backbone_taxonID",
    all.x = TRUE
  )

  for (col in c("n_cells_10km", "n_cells_50km", "total_occ_10km", "total_occ_50km")) {
    if (col %in% names(spatial_coverage)) {
      spatial_coverage[is.na(get(col)), (col) := 0]
    }
  }

  # Flag poorly sampled
  spatial_coverage[, poorly_sampled_spatial :=
    (n_cells_10km < MIN_CELLS & n_cells_50km < MIN_CELLS)]
  spatial_coverage[, poorly_sampled_abundance :=
    (total_occ_10km < MIN_OCCURRENCES & total_occ_50km < MIN_OCCURRENCES)]

  cli_alert_success("Spatial coverage: {scales::comma(nrow(spatial_coverage))} taxa")
}


# ===========================================================================
# COVERAGE BY HIGHER TAXONOMY
# ===========================================================================

cli_h2("Coverage by Higher Taxonomy")

# By family
family_summary <- NULL
if ("family" %in% names(match_summary)) {
  hierarchy_cols <- intersect(c("kingdom", "phylum", "class", "order"), names(match_summary))
  group_cols <- c(hierarchy_cols, "family")

  family_summary <- match_summary[!is.na(family) & family != "", .(
    n_taxa                = .N,
    n_in_gbif             = sum(matched_any),
    pct_coverage          = round(100 * sum(matched_any) / .N, 2),
    n_threatened          = sum(threatStatus %in% THREATENED_CODES, na.rm = TRUE),
    n_threatened_in_gbif  = sum(matched_any & threatStatus %in% THREATENED_CODES, na.rm = TRUE)
  ), by = group_cols]
  setorder(family_summary, -n_taxa)
  cli_alert_success("Family summary: {scales::comma(nrow(family_summary))} families")
}

# By order
order_summary <- NULL
if ("order" %in% names(match_summary)) {
  hierarchy_cols <- intersect(c("kingdom", "phylum", "class"), names(match_summary))
  group_cols <- c(hierarchy_cols, "order")

  order_summary <- match_summary[!is.na(order) & order != "", .(
    n_taxa                = .N,
    n_in_gbif             = sum(matched_any),
    pct_coverage          = round(100 * sum(matched_any) / .N, 2),
    n_threatened          = sum(threatStatus %in% THREATENED_CODES, na.rm = TRUE),
    n_threatened_in_gbif  = sum(matched_any & threatStatus %in% THREATENED_CODES, na.rm = TRUE)
  ), by = group_cols]
  setorder(order_summary, -n_taxa)
  cli_alert_success("Order summary: {scales::comma(nrow(order_summary))} orders")
}


# ===========================================================================
# PRIORITY TAXA
# ===========================================================================

cli_h2("Identifying Priority Taxa")

# Priority 1: Threatened taxa missing from GBIF
priority_cols <- intersect(
  c("taxonID", "scientificName", "taxonRank", "threatStatus", "family", "order"),
  names(missing_threatened)
)
priority_threatened <- missing_threatened[, ..priority_cols]
priority_threatened[, priority := "Threatened - Not in GBIF"]

# Priority 2: Threatened taxa with poor spatial coverage
priority_poorly_sampled <- NULL
if (!is.null(spatial_coverage)) {
  priority_poorly_sampled <- spatial_coverage[
    threatStatus %in% THREATENED_CODES &
      (poorly_sampled_spatial | poorly_sampled_abundance)
  ]
  if (nrow(priority_poorly_sampled) > 0) {
    ps_cols <- intersect(
      c("taxonID", "scientificName", "taxonRank", "threatStatus", "family", "order",
        "n_cells_10km", "n_cells_50km", "total_occ_10km", "total_occ_50km"),
      names(priority_poorly_sampled)
    )
    priority_poorly_sampled <- priority_poorly_sampled[, ..ps_cols]
    priority_poorly_sampled[, priority := "Threatened - Poorly Sampled"]
  } else {
    priority_poorly_sampled <- NULL
  }
}

priority_all <- rbindlist(list(
  priority_threatened,
  priority_poorly_sampled
), fill = TRUE)
setorder(priority_all, threatStatus, scientificName)

cli_alert_success("Priority taxa: {scales::comma(nrow(priority_all))}")


# ===========================================================================
# WRITE OUTPUTS
# ===========================================================================

cli_h2("Writing Outputs")

# Validate schemas before saving
if (exists("validate_match_summary")) {
  validate_match_summary(match_summary)
  validate_missing_threatened(missing_threatened)
  validate_coverage_by_rank(rank_summary)
  validate_coverage_by_threat(threat_summary)
  if (!is.null(spatial_coverage)) {
    validate_spatial_coverage(spatial_coverage)
  }
}

write_gap_file <- function(dt, filename, label = NULL) {
  path <- here(p_gaps, filename)
  fwrite(dt, path)
  if (is.null(label)) label <- filename
  cli_alert_success("{label}: {scales::comma(nrow(dt))} rows")
}

# Match summary (backbone view -- replaces 09's match_table and match_summary)
write_gap_file(match_summary, "taxonomic_match_summary.csv")

# Missing taxa
write_gap_file(missing_taxa, "taxonomic_missing_taxa.csv")
write_gap_file(missing_threatened, "taxonomic_missing_threatened.csv")

# Coverage summaries
write_gap_file(rank_summary, "taxonomic_coverage_by_rank.csv")
write_gap_file(threat_summary, "taxonomic_coverage_by_threat.csv")
write_gap_file(rank_threat_summary, "taxonomic_gap_summary.csv")

if (!is.null(estab_summary)) {
  write_gap_file(estab_summary, "taxonomic_coverage_by_establishment.csv")
}
if (exists("occ_status_summary") && !is.null(occ_status_summary)) {
  write_gap_file(occ_status_summary, "taxonomic_coverage_by_occurrence_status.csv")
}

if (nrow(basis_coverage) > 0) {
  write_gap_file(basis_coverage, "taxonomic_coverage_by_basis.csv")
}

# Spatial coverage
if (!is.null(spatial_coverage)) {
  write_gap_file(spatial_coverage, "taxonomic_spatial_coverage.csv")

  threatened_spatial <- spatial_coverage[threatStatus %in% THREATENED_CODES]
  write_gap_file(threatened_spatial, "taxonomic_threatened_spatial_coverage.csv")
}

# Higher taxonomy
if (!is.null(family_summary)) {
  write_gap_file(family_summary, "taxonomic_gaps_by_family.csv")
}
if (!is.null(order_summary)) {
  write_gap_file(order_summary, "taxonomic_gaps_by_order.csv")
}

# Priority taxa
write_gap_file(priority_all, "taxonomic_priority_taxa.csv")


# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 09b)")

summary_dt <- data.table(
  Metric = c(
    "Reference taxa (accepted)",
    "Matched to GBIF",
    "Coverage %",
    "Threatened in reference",
    "Threatened in GBIF",
    "Threatened missing from GBIF",
    "Priority taxa for sampling"
  ),
  Value = c(
    scales::comma(nrow(tax_clean)),
    scales::comma(n_backbone_matched),
    paste0(round(100 * n_backbone_matched / nrow(tax_clean), 1), "%"),
    scales::comma(sum(match_summary$threatStatus %in% THREATENED_CODES, na.rm = TRUE)),
    scales::comma(sum(match_summary$matched_any &
                        match_summary$threatStatus %in% THREATENED_CODES, na.rm = TRUE)),
    scales::comma(nrow(missing_threatened)),
    scales::comma(nrow(priority_all))
  )
)

print(summary_dt)

elapsed <- round(difftime(Sys.time(), timer_start, units = "mins"), 1)
cli_alert_info("Elapsed: {elapsed} minutes")
cli_alert_info("Output location: {.path {p_gaps}}")

n_outputs <- length(list.files(p_gaps, pattern = "^taxonomic_"))
cli_alert_info("Created {n_outputs} taxonomic gap files")
cli_alert_info("Next: source('scripts/10_make_gap_overview.R')")
