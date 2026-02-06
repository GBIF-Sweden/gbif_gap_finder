# scripts/03_ingest_taxonomy.R
# ==============================================================================
# Taxonomy Ingestion: Dyntaxa (Primary) + Swedish Red List (Threat Status)
# ==============================================================================
# This script:
# - Reads Dyntaxa taxonomy and distribution files (PRIMARY BACKBONE)
# - Reads Swedish Red List for threat status (SECONDARY REFERENCE)
# - Standardizes column names to Darwin Core (DwC) terms
# - Joins distribution and taxonomy data
# - Keeps BOTH Dyntaxa and Red List threat status as separate columns
# - Creates a unified taxa reference table
# - Writes processed RDS files to data_proc/
#
# Data Sources:
# - Dyntaxa: https://doi.org/10.15468/j43wfc (~110,000 taxa)
# - Swedish Red List: https://doi.org/10.15468/jhwkpq (~11,000 taxa with threat status)
#
# Output columns for threat status:
# - threatStatus_dyntaxa: from Dyntaxa SpeciesDistribution.csv
# - threatStatus_redlist: from Swedish Red List distribution.txt

library(here)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(cli)
library(glue)

source(here("scripts", "00_setup.R"))

# Configuration (from config.yml) ------------------------------------------

# Directories
dir_dyntaxa    <- here(cfg_get("paths.dyntaxa_dir", "data_raw/dyntaxa"))
dir_redlist_se <- here(cfg_get("paths.redlist_se_dir", "data_raw/red_list_se"))
dir_data_proc  <- here(cfg_get("paths.data_proc", "data_proc"))

# Dyntaxa input files (PRIMARY BACKBONE)
file_dyntaxa_taxon <- cfg_get("files.dyntaxa.dyntaxa_taxon", "Taxon.csv")
file_dyntaxa_distr <- cfg_get("files.dyntaxa.dyntaxa_distr", "SpeciesDistribution.csv")

# Swedish Red List input files (SECONDARY - for threat status)
file_redlist_taxon <- cfg_get("files.redlist_se.redlist_se_taxon", "taxon.txt")
file_redlist_distr <- cfg_get("files.redlist_se.redlist_se_distr", "distribution.txt")

# Build full input paths
input_files <- list(
  # Primary backbone (Dyntaxa)
  dyntaxa_taxon = file.path(dir_dyntaxa, file_dyntaxa_taxon),
  dyntaxa_distr = file.path(dir_dyntaxa, file_dyntaxa_distr),
  # Secondary reference (Red List)
  redlist_taxon = file.path(dir_redlist_se, file_redlist_taxon),
  redlist_distr = file.path(dir_redlist_se, file_redlist_distr)
)

# Output files
output_files <- list(
  # Dyntaxa processed
  dyntaxa_taxon      = file.path(dir_data_proc, "dyntaxa_taxon_current.rds"),
  dyntaxa_distr      = file.path(dir_data_proc, "dyntaxa_distribution_current.rds"),
  # Red List processed (optional, for reference)
  redlist_taxon      = file.path(dir_data_proc, "redlist_se_taxon_current.rds"),
  redlist_distr      = file.path(dir_data_proc, "redlist_se_distribution_current.rds"),
  # Main output: unified taxa reference
  taxa_reference     = file.path(dir_data_proc, "taxa_reference_current.rds")
)

# Darwin Core term mappings -----------------------------------------------
# Maps lowercase input column names to proper DwC camelCase
dwc_name_map <- c(
  # Identifiers
  "id" = "id",
  "taxonid" = "taxonID",
  "acceptednameusageid" = "acceptedNameUsageID",
  "parentnameusageid" = "parentNameUsageID",
  
  # Taxon core terms
  "scientificname" = "scientificName",
  "scientificnameauthorship" = "scientificNameAuthorship",
  "taxonrank" = "taxonRank",
  "taxonomicstatus" = "taxonomicStatus",
  "nomenclaturalstatus" = "nomenclaturalStatus",
  "taxonremarks" = "taxonRemarks",
  
  # Classification
  "kingdom" = "kingdom",
  "phylum" = "phylum",
  "class" = "class",
  "order" = "order",
  "family" = "family",
  "genus" = "genus",
  "species" = "species",
  
  # Occurrence/Distribution terms
  "country" = "country",
  "countrycode" = "countryCode",
  "occurrencestatus" = "occurrenceStatus",
  "establishmentmeans" = "establishmentMeans",
  
  # Threat status (not standard DwC but widely used)
  "threatstatus" = "threatStatus",
  
  # Dyntaxa-specific
  "dynamicproperties" = "dynamicProperties"
)

# Helper functions --------------------------------------------------------

#' Detect delimiter in text file
#' @param path File path
#' @return Delimiter character
detect_delimiter <- function(path) {
  first_line <- read_lines(path, n_max = 1)
  
  if (length(first_line) == 0) {
    return("\t")
  }
  
  # Count tabs vs commas
  n_tabs <- str_count(first_line, "\t")
  n_commas <- str_count(first_line, ",")
  
  if (n_tabs >= n_commas) "\t" else ","
}

#' Read delimited file with auto-detection
#' @param path File path
#' @return tibble
read_table_safe <- function(path) {
  if (!file.exists(path)) {
    cli_abort("File not found: {.path {path}}")
  }
  
  delim <- detect_delimiter(path)
  cli_alert_info("Reading with delimiter: {ifelse(delim == '\\t', 'TAB', delim)}")
  
  read_delim(
    path,
    delim = delim,
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    progress = FALSE
  )
}

#' Rename columns to Darwin Core standard
#' @param df Data frame
#' @param map Named vector of term mappings
#' @return Data frame with standardized names
rename_to_dwc <- function(df, map = dwc_name_map) {
  original_names <- names(df)
  lowercase_names <- str_to_lower(original_names)
  
  # Map known DwC terms
  new_names <- original_names
  matched <- lowercase_names %in% names(map)
  new_names[matched] <- map[lowercase_names[matched]]
  
  # Report renamed columns
  renamed <- original_names != new_names
  if (any(renamed)) {
    cli_alert_info("Standardized {sum(renamed)} column names to DwC")
  }
  
  df |>
    set_names(new_names)
}

#' Extract numeric Dyntaxa ID from LSID
#' @param lsid Character vector of LSIDs (e.g., "urn:lsid:dyntaxa.se:Taxon:219026")
#' @return Character vector of numeric IDs
extract_dyntaxa_id <- function(lsid) {
  str_extract(lsid, "(?<=:)[0-9]+$")
}

# ===========================================================================
# 1. READ DYNTAXA DATA (PRIMARY BACKBONE)
# ===========================================================================
cli_h1("Reading Dyntaxa - Swedish Taxonomic Database (PRIMARY)")

cli_alert_info("Source: https://doi.org/10.15468/j43wfc")

# Read Dyntaxa taxonomy
cli_h2("Dyntaxa Taxonomy")
cli_alert_info("Reading: {.path {input_files$dyntaxa_taxon}}")

dyntaxa_taxon <- read_table_safe(input_files$dyntaxa_taxon) |>
  rename_to_dwc()

cli_alert_success(
  "Dyntaxa taxonomy: {format(nrow(dyntaxa_taxon), big.mark = ',')} rows, {ncol(dyntaxa_taxon)} columns"
)

# Read Dyntaxa distribution
cli_h2("Dyntaxa Distribution")
cli_alert_info("Reading: {.path {input_files$dyntaxa_distr}}")

dyntaxa_distr <- read_table_safe(input_files$dyntaxa_distr) |>
  rename_to_dwc()

cli_alert_success(
  "Dyntaxa distribution: {format(nrow(dyntaxa_distr), big.mark = ',')} rows, {ncol(dyntaxa_distr)} columns"
)

# Save processed Dyntaxa files
cli_h2("Saving Processed Dyntaxa Files")

saveRDS(dyntaxa_taxon, output_files$dyntaxa_taxon, compress = "xz")
cli_alert_success("Wrote: {.path {output_files$dyntaxa_taxon}}")

saveRDS(dyntaxa_distr, output_files$dyntaxa_distr, compress = "xz")
cli_alert_success("Wrote: {.path {output_files$dyntaxa_distr}}")

# ===========================================================================
# 2. READ SWEDISH RED LIST (SECONDARY - FOR THREAT STATUS)
# ===========================================================================
cli_h1("Reading Swedish Red List (SECONDARY - threat status)")

cli_alert_info("Source: https://doi.org/10.15468/jhwkpq")

# Check if Red List files exist
redlist_available <- file.exists(input_files$redlist_taxon) && 
                     file.exists(input_files$redlist_distr)

if (redlist_available) {
  # Read Red List taxonomy
  cli_h2("Red List Taxonomy")
  cli_alert_info("Reading: {.path {input_files$redlist_taxon}}")
  
  redlist_taxon <- read_table_safe(input_files$redlist_taxon) |>
    rename_to_dwc()
  
  cli_alert_success(
    "Red List taxonomy: {format(nrow(redlist_taxon), big.mark = ',')} rows, {ncol(redlist_taxon)} columns"
  )
  
  # Read Red List distribution (contains threat status)
  cli_h2("Red List Distribution")
  cli_alert_info("Reading: {.path {input_files$redlist_distr}}")
  
  redlist_distr <- read_table_safe(input_files$redlist_distr) |>
    rename_to_dwc()
  
  cli_alert_success(
    "Red List distribution: {format(nrow(redlist_distr), big.mark = ',')} rows, {ncol(redlist_distr)} columns"
  )
  
  # Save processed Red List files
  cli_h2("Saving Processed Red List Files")
  
  saveRDS(redlist_taxon, output_files$redlist_taxon, compress = "xz")
  cli_alert_success("Wrote: {.path {output_files$redlist_taxon}}")
  
  saveRDS(redlist_distr, output_files$redlist_distr, compress = "xz")
  cli_alert_success("Wrote: {.path {output_files$redlist_distr}}")
  
} else {
  cli_alert_warning("Swedish Red List files not found - threat status will not be enriched")
  cli_alert_info("Expected files:")
  cli_alert_info("  - {.path {input_files$redlist_taxon}}")
  cli_alert_info("  - {.path {input_files$redlist_distr}}")
  redlist_taxon <- NULL
  redlist_distr <- NULL
}

# ===========================================================================
# 3. BUILD UNIFIED TAXA REFERENCE (DYNTAXA BASE + RED LIST ENRICHMENT)
# ===========================================================================
cli_h1("Building Unified Taxa Reference")

# 3.1 Join Dyntaxa taxonomy with Dyntaxa distribution ----------------------
cli_h2("Joining Dyntaxa Taxonomy + Distribution")

# Find join key for Dyntaxa files
dyntaxa_join_key <- case_when(
  "taxonID" %in% names(dyntaxa_taxon) && "taxonID" %in% names(dyntaxa_distr) ~ "taxonID",
  "id" %in% names(dyntaxa_taxon) && "id" %in% names(dyntaxa_distr) ~ "id",
  TRUE ~ NA_character_
)

if (is.na(dyntaxa_join_key)) {
  cli_alert_warning("No common ID column found between Dyntaxa files - using taxonomy only")
  taxa_reference <- dyntaxa_taxon
} else {
  cli_alert_info("Joining on: {dyntaxa_join_key}")
  
  # Select relevant columns from distribution (avoid duplicates)
  distr_cols <- c(
    dyntaxa_join_key,
    "country", "countryCode", "occurrenceStatus", 
    "establishmentMeans", "threatStatus", "dynamicProperties"
  )
  distr_cols <- intersect(distr_cols, names(dyntaxa_distr))
  
  taxa_reference <- dyntaxa_taxon |>
    left_join(
      dyntaxa_distr |> select(all_of(distr_cols)),
      by = dyntaxa_join_key
    )
  
  cli_alert_success(
    "Joined: {format(nrow(taxa_reference), big.mark = ',')} rows"
  )
}

# 3.2 Enrich with Swedish Red List threat status ---------------------------
if (redlist_available && !is.null(redlist_distr)) {
  cli_h2("Enriching with Swedish Red List Threat Status")
  
  # Prepare Red List threat lookup
  # First, join Red List taxon + distribution
  redlist_join_key <- case_when(
    "taxonID" %in% names(redlist_taxon) && "taxonID" %in% names(redlist_distr) ~ "taxonID",
    "id" %in% names(redlist_taxon) && "id" %in% names(redlist_distr) ~ "id",
    TRUE ~ NA_character_
  )
  
  if (!is.na(redlist_join_key)) {
    redlist_combined <- redlist_taxon |>
      left_join(
        redlist_distr |> select(any_of(c(redlist_join_key, "threatStatus", "occurrenceStatus"))),
        by = redlist_join_key
      )
  } else {
    redlist_combined <- redlist_taxon
    if ("threatStatus" %in% names(redlist_distr)) {
      # Try to add threatStatus if present
      redlist_combined <- bind_cols(redlist_combined, redlist_distr |> select(any_of("threatStatus")))
    }
  }
  
  # Create lookup by scientificName for matching
  redlist_threat_lookup <- redlist_combined |>
    filter(!is.na(scientificName)) |>
    select(scientificName, any_of(c("threatStatus"))) |>
    filter(!is.na(threatStatus)) |>
    distinct(scientificName, .keep_all = TRUE) |>
    rename(threatStatus_redlist = threatStatus)
  
  cli_alert_info(
    "Red List threat lookup: {format(nrow(redlist_threat_lookup), big.mark = ',')} taxa with threat status"
  )
  
  # Check if Dyntaxa already has threatStatus from its distribution file
  dyntaxa_has_threat <- "threatStatus" %in% names(taxa_reference)
  
  if (dyntaxa_has_threat) {
    # Keep both sources as separate columns
    cli_alert_info("Keeping both Dyntaxa and Red List threat status as separate columns")
    
    taxa_reference <- taxa_reference |>
      rename(threatStatus_dyntaxa = threatStatus) |>
      left_join(redlist_threat_lookup, by = "scientificName")
    
  } else {
    # Dyntaxa doesn't have threatStatus - add Red List as its own column
    cli_alert_info("Adding Red List threat status (Dyntaxa has no threat status)")
    
    taxa_reference <- taxa_reference |>
      left_join(redlist_threat_lookup, by = "scientificName") |>
      mutate(threatStatus_dyntaxa = NA_character_)
  }
  
  # Report on matches
  n_redlist_matched <- sum(!is.na(taxa_reference$threatStatus_redlist))
  n_redlist_unmatched <- nrow(redlist_threat_lookup) - n_redlist_matched
  
  cli_alert_success(
    "Matched: {format(n_redlist_matched, big.mark = ',')} Dyntaxa taxa have Red List threat status"
  )
  
  if (n_redlist_unmatched > 0) {
    cli_alert_warning(
      "{format(n_redlist_unmatched, big.mark = ',')} Red List taxa didn't match any Dyntaxa scientificName"
    )
  }
  
  # Report conflicts between Dyntaxa and Red List
  if (dyntaxa_has_threat) {
    conflicts <- taxa_reference |>
      filter(!is.na(threatStatus_dyntaxa) & !is.na(threatStatus_redlist)) |>
      filter(threatStatus_dyntaxa != threatStatus_redlist)
    
    if (nrow(conflicts) > 0) {
      cli_alert_warning(
        "Found {format(nrow(conflicts), big.mark = ',')} taxa with conflicting threat status"
      )
      
      # Show conflict breakdown
      conflict_summary <- conflicts |>
        count(threatStatus_dyntaxa, threatStatus_redlist, sort = TRUE) |>
        head(10)
      
      cli_alert_info("Top conflicts (Dyntaxa → Red List):")
      print(conflict_summary)
    } else {
      cli_alert_success("No conflicts between Dyntaxa and Red List threat status")
    }
  }
  
  # Count coverage from each source
  n_dyntaxa_threat <- sum(!is.na(taxa_reference$threatStatus_dyntaxa))
  n_redlist_threat <- sum(!is.na(taxa_reference$threatStatus_redlist))
  n_both_threat <- sum(!is.na(taxa_reference$threatStatus_dyntaxa) & 
                       !is.na(taxa_reference$threatStatus_redlist))
  
  cli_alert_success(
    "Threat status coverage: {format(n_dyntaxa_threat, big.mark = ',')} from Dyntaxa, {format(n_redlist_threat, big.mark = ',')} from Red List, {format(n_both_threat, big.mark = ',')} from both"
  )
  
  # Threat status breakdown by source
  if (n_dyntaxa_threat > 0) {
    dyntaxa_summary <- taxa_reference |>
      filter(!is.na(threatStatus_dyntaxa)) |>
      count(threatStatus_dyntaxa, sort = TRUE)
    
    cli_alert_info("Dyntaxa threat status breakdown:")
    print(dyntaxa_summary)
  }
  
  if (n_redlist_threat > 0) {
    redlist_summary <- taxa_reference |>
      filter(!is.na(threatStatus_redlist)) |>
      count(threatStatus_redlist, sort = TRUE)
    
    cli_alert_info("Red List threat status breakdown:")
    print(redlist_summary)
  }
}

# 3.3 Filter to accepted taxa only (optional but recommended) --------------
cli_h2("Filtering Taxonomic Status")

# Check what taxonomicStatus values exist
if ("taxonomicStatus" %in% names(taxa_reference)) {
  status_summary <- taxa_reference |>
    count(taxonomicStatus, sort = TRUE)
  
  cli_alert_info("Taxonomic status in Dyntaxa:")
  print(status_summary)
  
  # Keep accepted taxa only for the main reference
  # (synonyms can be kept for lookup purposes)
  n_accepted <- sum(taxa_reference$taxonomicStatus == "accepted", na.rm = TRUE)
  cli_alert_info(
    "Accepted taxa: {format(n_accepted, big.mark = ',')} / {format(nrow(taxa_reference), big.mark = ',')}"
  )
}

# 3.4 Add derived fields ---------------------------------------------------
cli_h2("Adding Derived Fields")

# Extract numeric Dyntaxa ID for easier matching
if ("taxonID" %in% names(taxa_reference)) {
  taxa_reference <- taxa_reference |>
    mutate(
      dyntaxaID = extract_dyntaxa_id(taxonID)
    )
  cli_alert_success("Added dyntaxaID (numeric ID extracted from LSID)")
}

# Add source indicator
taxa_reference <- taxa_reference |>
  mutate(
    taxonomic_backbone = "dyntaxa",
    backbone_version = "1.2",  
    backbone_doi = "https://doi.org/10.15468/j43wfc"
  )

cli_alert_success("Added backbone source metadata")

# 3.5 Select and order columns ---------------------------------------------
cli_h2("Organizing Columns")

core_columns <- c(
  # Identifiers
  "taxonID", "dyntaxaID", "acceptedNameUsageID", "parentNameUsageID",
  
  # Names
  "scientificName", "scientificNameAuthorship",
  
  # Classification hierarchy
  "kingdom", "phylum", "class", "order", "family", "genus", "species",
  
  # Status
  "taxonRank", "taxonomicStatus", "nomenclaturalStatus",
  
  # Distribution & Conservation
  "country", "countryCode", "occurrenceStatus", 
  "establishmentMeans", "threatStatus_dyntaxa", "threatStatus_redlist",
  
  # Metadata
  "taxonomic_backbone", "backbone_version", "backbone_doi",
  
  # Additional
  "taxonRemarks", "dynamicProperties"
)

# Keep available core columns first, then everything else
available_core <- intersect(core_columns, names(taxa_reference))
other_columns <- setdiff(names(taxa_reference), core_columns)

taxa_reference_clean <- taxa_reference |>
  select(all_of(available_core), all_of(other_columns))

cli_alert_info("Column order: {length(available_core)} core + {length(other_columns)} additional")

# ===========================================================================
# 4. SAVE FINAL TAXA REFERENCE
# ===========================================================================
cli_h1("Saving Taxa Reference")

saveRDS(taxa_reference_clean, output_files$taxa_reference, compress = "xz")
cli_alert_success("Wrote: {.path {output_files$taxa_reference}}")

# ===========================================================================
# 5. SUMMARY STATISTICS
# ===========================================================================
cli_h1("Summary Statistics")

summary_stats <- tibble::tibble(
  dataset = c(
    "Dyntaxa Taxonomy (raw)",
    "Dyntaxa Distribution (raw)",
    if (redlist_available) "Red List Taxonomy (raw)" else NULL,
    if (redlist_available) "Red List Distribution (raw)" else NULL,
    "Taxa Reference (final)"
  ),
  n_rows = c(
    nrow(dyntaxa_taxon),
    nrow(dyntaxa_distr),
    if (redlist_available) nrow(redlist_taxon) else NULL,
    if (redlist_available) nrow(redlist_distr) else NULL,
    nrow(taxa_reference_clean)
  ),
  n_columns = c(
    ncol(dyntaxa_taxon),
    ncol(dyntaxa_distr),
    if (redlist_available) ncol(redlist_taxon) else NULL,
    if (redlist_available) ncol(redlist_distr) else NULL,
    ncol(taxa_reference_clean)
  )
)

print(summary_stats)

# Taxonomic coverage summary
cli_h2("Taxonomic Coverage")

if ("taxonRank" %in% names(taxa_reference_clean)) {
  rank_summary <- taxa_reference_clean |>
    filter(taxonomicStatus == "accepted" | is.na(taxonomicStatus)) |>
    count(taxonRank, sort = TRUE) |>
    head(10)
  
  cli_alert_info("Top taxonomic ranks (accepted taxa):")
  print(rank_summary)
}

if ("kingdom" %in% names(taxa_reference_clean)) {
  kingdom_summary <- taxa_reference_clean |>
    filter(taxonomicStatus == "accepted" | is.na(taxonomicStatus)) |>
    count(kingdom, sort = TRUE)
  
  cli_alert_info("Kingdoms (accepted taxa):")
  print(kingdom_summary)
}

# ===========================================================================
# 6. VALIDATION CHECKS
# ===========================================================================
cli_h1("Validation Checks")

validation_checks <- list(
  "scientificName present" = "scientificName" %in% names(taxa_reference_clean),
  "taxonRank present" = "taxonRank" %in% names(taxa_reference_clean),
  "taxonID present" = "taxonID" %in% names(taxa_reference_clean),
  "threatStatus_dyntaxa present" = "threatStatus_dyntaxa" %in% names(taxa_reference_clean),
  "threatStatus_redlist present" = "threatStatus_redlist" %in% names(taxa_reference_clean),
  "No duplicate taxonIDs" = if ("taxonID" %in% names(taxa_reference_clean)) {
    !anyDuplicated(taxa_reference_clean$taxonID)
  } else {
    NA
  },
  "Accepted taxa exist" = if ("taxonomicStatus" %in% names(taxa_reference_clean)) {
    any(taxa_reference_clean$taxonomicStatus == "accepted", na.rm = TRUE)
  } else {
    NA
  }
)

purrr::iwalk(validation_checks, ~{
  if (is.na(.x)) {
    cli_alert_info("{.y}: NA (not applicable)")
  } else if (.x) {
    cli_alert_success("{.y}")
  } else {
    cli_alert_warning("{.y}: FAILED")
  }
})

# Final message
cli_h1("Ingestion Complete")
cli_alert_success("Primary backbone: Dyntaxa ({format(nrow(taxa_reference_clean), big.mark = ',')} taxa)")
if (redlist_available) {
  cli_alert_success("Threat status columns: threatStatus_dyntaxa, threatStatus_redlist")
}
cli_alert_info("Output: {.path {output_files$taxa_reference}}")
cli_alert_info("Next step: Run scripts/04_ingest_gbif_cubes.R")
