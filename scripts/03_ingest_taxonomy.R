# scripts/03_ingest_taxonomy.R
# ============================================================================
# Taxonomy Ingestion: National Backbone + Red List Threat Status
# ============================================================================
# Purpose:
#   Read and standardise the national taxonomy backbone (primary) and
#   national red list (secondary, for threat status enrichment).
#   Produces a unified taxa reference table used by all downstream
#   analysis scripts.
#
# Inputs:
#   - data/{CC}/raw/taxonomy/ (Taxon.csv + SpeciesDistribution.csv)
#   - data/{CC}/raw/redlist/  (taxon.txt + distribution.txt, optional)
#
# Outputs (in data/{CC}/proc/):
#   - <taxonomy>_taxon_current.rds
#   - <taxonomy>_distribution_current.rds
#   - redlist_taxon_current.rds
#   - redlist_distribution_current.rds
#   - taxa_reference_current.rds (main output)
#   - taxonomy_backbone.csv
#
# Dependencies: scripts/00_setup.R, readr, dplyr, stringr, purrr
# ============================================================================

library(here)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(cli)
library(glue)

source(here("scripts", "00_setup.R"))

# ============================================================================
# Configuration
# ============================================================================

# Directories (from globals.R — already country-aware)
dir_taxonomy   <- here(raw_taxonomy_dir)
dir_redlist    <- here(raw_redlist_dir)
dir_data_proc  <- here(p_data_proc)

# National taxonomy input files (PRIMARY BACKBONE)
file_taxonomy_taxon <- cfg_get("files.taxonomy.taxonomy_taxon", "Taxon.csv")
file_taxonomy_distr <- cfg_get("files.taxonomy.taxonomy_distr", "SpeciesDistribution.csv")

# National red list input files (SECONDARY - threat status)
file_redlist_taxon <- cfg_get("files.redlist.redlist_taxon", "taxon.txt")
file_redlist_distr <- cfg_get("files.redlist.redlist_distr", "distribution.txt")

# Full input paths
input_files <- list(
  taxonomy_taxon = file.path(dir_taxonomy, file_taxonomy_taxon),
  taxonomy_distr = file.path(dir_taxonomy, file_taxonomy_distr),
  redlist_taxon  = file.path(dir_redlist, file_redlist_taxon),
  redlist_distr  = file.path(dir_redlist, file_redlist_distr)
)

# Output files
output_files <- list(
  taxonomy_taxon = file.path(
    dir_data_proc, "taxonomy_taxon_current.rds"
  ),
  taxonomy_distr = file.path(
    dir_data_proc, "taxonomy_distribution_current.rds"
  ),
  redlist_taxon  = file.path(
    dir_data_proc, "redlist_taxon_current.rds"
  ),
  redlist_distr  = file.path(
    dir_data_proc, "redlist_distribution_current.rds"
  ),
  taxa_reference = file.path(
    dir_data_proc, "taxa_reference_current.rds"
  )
)

# Backbone metadata (from config, with backward-compatible defaults)
backbone_name <- cfg_get("taxonomy.name", "National Taxonomy")
backbone_doi  <- cfg_get(
  "taxonomy.doi", "https://doi.org/10.15468/j43wfc"
)
backbone_version <- cfg_get("taxonomy.version", "1.2")

# ============================================================================
# Darwin Core Term Mappings
# ============================================================================
# Maps lowercase input column names to proper DwC camelCase.
# This mapping is taxonomy-agnostic and works for any DwC-A export.

dwc_name_map <- c(
  # Identifiers
  "id"                    = "id",
  "taxonid"               = "taxonID",
  "acceptednameusageid"   = "acceptedNameUsageID",
  "parentnameusageid"     = "parentNameUsageID",

  # Taxon core
  "scientificname"             = "scientificName",
  "scientificnameauthorship"   = "scientificNameAuthorship",
  "taxonrank"                  = "taxonRank",
  "taxonomicstatus"            = "taxonomicStatus",
  "nomenclaturalstatus"        = "nomenclaturalStatus",
  "taxonremarks"               = "taxonRemarks",

  # Classification
  "kingdom" = "kingdom",
  "phylum"  = "phylum",
  "class"   = "class",
  "order"   = "order",
  "family"  = "family",
  "genus"   = "genus",
  "species" = "species",

  # Occurrence / distribution
  "country"            = "country",
  "countrycode"        = "countryCode",
  "occurrencestatus"   = "occurrenceStatus",
  "establishmentmeans" = "establishmentMeans",

  # Conservation
 "threatstatus"       = "threatStatus",

  # Extended
  "dynamicproperties"  = "dynamicProperties"
)

# ============================================================================
# Helper Functions
# ============================================================================

#' Detect delimiter in a text file (tab vs comma)
#'
#' @param path File path
#' @return Delimiter character ("\t" or ",")
detect_delimiter <- function(path) {
  first_line <- read_lines(path, n_max = 1)
  if (length(first_line) == 0) return("\t")

  n_tabs   <- str_count(first_line, "\t")
  n_commas <- str_count(first_line, ",")

  if (n_tabs >= n_commas) "\t" else ","
}

#' Read a delimited file with auto-detection
#'
#' @param path File path
#' @return A tibble
read_table_safe <- function(path) {
  if (!file.exists(path)) {
    cli_abort("File not found: {.path {path}}")
  }

  delim <- detect_delimiter(path)
  cli_alert_info(
    "Reading with delimiter: {ifelse(delim == '\\t', 'TAB', delim)}"
  )

  read_delim(
    path,
    delim          = delim,
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    progress       = FALSE
  )
}

#' Rename columns to Darwin Core standard
#'
#' @param df   A data frame
#' @param map  Named character vector of term mappings
#' @return Data frame with standardised column names
rename_to_dwc <- function(df, map = dwc_name_map) {
  original_names  <- names(df)
  lowercase_names <- str_to_lower(original_names)

  new_names <- original_names
  matched   <- lowercase_names %in% names(map)
  new_names[matched] <- map[lowercase_names[matched]]

  renamed <- original_names != new_names
  if (any(renamed)) {
    cli_alert_info(
      "Standardised {sum(renamed)} column names to DwC"
    )
  }

  df |>
    set_names(new_names)
}

#' Extract numeric ID from an LSID string
#'
#' Works for Dyntaxa-style LSIDs
#' (e.g., "urn:lsid:dyntaxa.se:Taxon:219026" -> "219026")
#' and passes through plain numeric IDs unchanged.
#'
#' @param lsid Character vector
#' @return Character vector of numeric IDs
extract_numeric_id <- function(lsid) {
  str_extract(lsid, "(?<=:)[0-9]+$")
}

# ============================================================================
# 1. Read National Taxonomy (PRIMARY BACKBONE)
# ============================================================================

cli_h1("Reading {backbone_name} (PRIMARY BACKBONE)")
cli_alert_info("Source: {backbone_doi}")

# Taxonomy table
cli_h2("{backbone_name} Taxonomy")
cli_alert_info("Reading: {.path {input_files$taxonomy_taxon}}")

taxonomy_taxon <- read_table_safe(input_files$taxonomy_taxon) |>
  rename_to_dwc()

# -- Backbone compatibility normalisations ----------------------------------
# These ensure consistent types/values across different national backbones.
# Some (e.g. Nortaxa) use numeric taxonID; others (Dyntaxa) use LSID strings.
# Some use "valid" instead of "accepted" for taxonomicStatus.
# Normalising here prevents type-mismatch errors in downstream joins (09a/09b).

# Force all ID columns to character (some backbones use numeric IDs)
id_cols <- c("taxonID", "acceptedNameUsageID", "parentNameUsageID")
id_cols_present <- intersect(id_cols, names(taxonomy_taxon))
if (length(id_cols_present) > 0) {
  taxonomy_taxon <- taxonomy_taxon |>
    mutate(across(all_of(id_cols_present), as.character))
  cli_alert_info(
    "Normalised to character: {paste(id_cols_present, collapse = ', ')}"
  )
}

if ("taxonomicStatus" %in% names(taxonomy_taxon)) {
  n_valid <- sum(taxonomy_taxon$taxonomicStatus == "valid", na.rm = TRUE)
  if (n_valid > 0) {
    taxonomy_taxon <- taxonomy_taxon |>
      mutate(
        taxonomicStatus = if_else(
          taxonomicStatus == "valid", "accepted", taxonomicStatus
        )
      )
    cli_alert_info(
      "Mapped {scales::comma(n_valid)} 'valid' \u2192 'accepted' in taxonomicStatus"
    )
  }
}

cli_alert_success(
  "{backbone_name} taxonomy: {scales::comma(nrow(taxonomy_taxon))} rows, {ncol(taxonomy_taxon)} columns"
)

# Distribution table
cli_h2("{backbone_name} Distribution")
cli_alert_info(
  "Reading: {.path {input_files$taxonomy_distr}}"
)

taxonomy_distr <- read_table_safe(input_files$taxonomy_distr) |>
  rename_to_dwc()

cli_alert_success(
  "{backbone_name} distribution: {scales::comma(nrow(taxonomy_distr))} rows, {ncol(taxonomy_distr)} columns"
)

# Save processed files
cli_h2("Saving Processed {backbone_name} Files")

saveRDS(
  taxonomy_taxon, output_files$taxonomy_taxon, compress = "xz"
)
cli_alert_success("Wrote: {.path {output_files$taxonomy_taxon}}")

saveRDS(
  taxonomy_distr, output_files$taxonomy_distr, compress = "xz"
)
cli_alert_success("Wrote: {.path {output_files$taxonomy_distr}}")

# ============================================================================
# 2. Read National Red List (SECONDARY - threat status)
# ============================================================================

cli_h1("Reading National Red List (SECONDARY)")

redlist_doi <- cfg_get(
  "redlist.doi", "https://doi.org/10.15468/jhwkpq"
)
cli_alert_info("Source: {redlist_doi}")

redlist_available <- file.exists(input_files$redlist_taxon) &&
  file.exists(input_files$redlist_distr)

if (redlist_available) {

  # Taxonomy table
  cli_h2("Red List Taxonomy")
  cli_alert_info(
    "Reading: {.path {input_files$redlist_taxon}}"
  )

  redlist_taxon <- read_table_safe(input_files$redlist_taxon) |>
    rename_to_dwc()

  cli_alert_success(
    "Red list taxonomy: {scales::comma(nrow(redlist_taxon))} rows, {ncol(redlist_taxon)} columns"
  )

  # Distribution table
  cli_h2("Red List Distribution")
  cli_alert_info(
    "Reading: {.path {input_files$redlist_distr}}"
  )

  redlist_distr <- read_table_safe(input_files$redlist_distr) |>
    rename_to_dwc()

  cli_alert_success(
    "Red list distribution: {scales::comma(nrow(redlist_distr))} rows, {ncol(redlist_distr)} columns"
  )

  # Save processed red list files
  cli_h2("Saving Processed Red List Files")

  saveRDS(
    redlist_taxon, output_files$redlist_taxon, compress = "xz"
  )
  cli_alert_success("Wrote: {.path {output_files$redlist_taxon}}")

  saveRDS(
    redlist_distr, output_files$redlist_distr, compress = "xz"
  )
  cli_alert_success("Wrote: {.path {output_files$redlist_distr}}")

} else {

  cli_alert_warning(
    "Red list files not found \u2014 threat status will not be enriched"
  )
  cli_alert_info("Expected files:")
  cli_alert_info("  {.path {input_files$redlist_taxon}}")
  cli_alert_info("  {.path {input_files$redlist_distr}}")
  redlist_taxon <- NULL
  redlist_distr <- NULL

}

# ============================================================================
# 3. Build Unified Taxa Reference
# ============================================================================

cli_h1("Building Unified Taxa Reference")

# 3.1 Join taxonomy + distribution -----------------------------------------

cli_h2("Joining {backbone_name} Taxonomy + Distribution")

taxonomy_join_key <- case_when(
  "taxonID" %in% names(taxonomy_taxon) &
    "taxonID" %in% names(taxonomy_distr) ~ "taxonID",
  "id" %in% names(taxonomy_taxon) &
    "id" %in% names(taxonomy_distr) ~ "id",
  TRUE ~ NA_character_
)

if (is.na(taxonomy_join_key)) {
  cli_alert_warning(
    "No common ID column \u2014 using taxonomy table only"
  )
  taxa_reference <- taxonomy_taxon
} else {
  cli_alert_info("Joining on: {taxonomy_join_key}")

  distr_cols <- c(
    taxonomy_join_key,
    "country", "countryCode", "occurrenceStatus",
    "establishmentMeans", "threatStatus",
    "dynamicProperties"
  ) |>
    intersect(names(taxonomy_distr))

  taxa_reference <- taxonomy_taxon |>
    left_join(
      taxonomy_distr |> select(all_of(distr_cols)),
      by = taxonomy_join_key
    )

  cli_alert_success(
    "Joined: {scales::comma(nrow(taxa_reference))} rows"
  )
}

# 3.2 Enrich with red list threat status -----------------------------------

if (redlist_available && !is.null(redlist_distr)) {
  cli_h2("Enriching with Red List Threat Status")

  # Join red list taxonomy + distribution
  redlist_join_key <- case_when(
    "taxonID" %in% names(redlist_taxon) &
      "taxonID" %in% names(redlist_distr) ~ "taxonID",
    "id" %in% names(redlist_taxon) &
      "id" %in% names(redlist_distr) ~ "id",
    TRUE ~ NA_character_
  )

  if (!is.na(redlist_join_key)) {
    redlist_combined <- redlist_taxon |>
      left_join(
        redlist_distr |>
          select(any_of(c(
            redlist_join_key, "threatStatus", "occurrenceStatus"
          ))),
        by = redlist_join_key
      )
  } else {
    redlist_combined <- redlist_taxon
    if ("threatStatus" %in% names(redlist_distr)) {
      redlist_combined <- bind_cols(
        redlist_combined,
        redlist_distr |> select(any_of("threatStatus"))
      )
    }
  }

  # Clean threat codes — some red lists have stray characters (e.g. NTº → NT)
  if ("threatStatus" %in% names(redlist_combined)) {
    raw_codes <- unique(redlist_combined$threatStatus[!is.na(redlist_combined$threatStatus)])
    redlist_combined <- redlist_combined |>
      mutate(threatStatus = gsub("[^A-Za-z]", "", threatStatus))
    clean_codes <- unique(redlist_combined$threatStatus[!is.na(redlist_combined$threatStatus)])
    if (length(raw_codes) != length(clean_codes)) {
      cli_alert_info(
        "Cleaned threat codes: {length(raw_codes)} unique \u2192 {length(clean_codes)} unique"
      )
    }
  }

  # Create threat lookup by scientificName
  redlist_threat_lookup <- redlist_combined |>
    filter(
      !is.na(scientificName),
      !is.na(threatStatus)
    ) |>
    select(scientificName, threatStatus) |>
    distinct(scientificName, .keep_all = TRUE) |>
    rename(threatStatus_redlist = threatStatus)

  cli_alert_info(
    "Red list threat lookup: {scales::comma(nrow(redlist_threat_lookup))} taxa"
  )

  # Merge: keep both sources as separate columns
  has_threat <- "threatStatus" %in% names(taxa_reference)

  if (has_threat) {
    cli_alert_info(
      "Keeping both backbone and red list threat status"
    )
    taxa_reference <- taxa_reference |>
      rename(threatStatus_backbone = threatStatus) |>
      left_join(redlist_threat_lookup, by = "scientificName")
  } else {
    cli_alert_info(
      "Adding red list threat status (backbone has none)"
    )
    taxa_reference <- taxa_reference |>
      left_join(redlist_threat_lookup, by = "scientificName") |>
      mutate(threatStatus_backbone = NA_character_)
  }

  # Report matches
  n_matched   <- sum(!is.na(taxa_reference$threatStatus_redlist))
  n_unmatched <- nrow(redlist_threat_lookup) - n_matched

  cli_alert_success(
    "Matched: {scales::comma(n_matched)} taxa with red list status"
  )

  if (n_unmatched > 0) {
    cli_alert_warning(
      "{scales::comma(n_unmatched)} red list taxa unmatched"
    )
  }

  # Report conflicts
  if (has_threat) {
    conflicts <- taxa_reference |>
      filter(
        !is.na(threatStatus_backbone),
        !is.na(threatStatus_redlist),
        threatStatus_backbone != threatStatus_redlist
      )

    if (nrow(conflicts) > 0) {
      cli_alert_warning(
        "{scales::comma(nrow(conflicts))} taxa with conflicting status"
      )
      conflict_summary <- conflicts |>
        count(
          threatStatus_backbone, threatStatus_redlist,
          sort = TRUE
        ) |>
        head(10)
      print(conflict_summary)
    } else {
      cli_alert_success("No conflicts between sources")
    }
  }

  # Coverage summary
  n_backbone <- sum(!is.na(taxa_reference$threatStatus_backbone))
  n_redlist <- sum(!is.na(taxa_reference$threatStatus_redlist))
  n_both    <- sum(
    !is.na(taxa_reference$threatStatus_backbone) &
      !is.na(taxa_reference$threatStatus_redlist)
  )

  cli_alert_success(
    "Threat coverage: {scales::comma(n_backbone)} backbone, {scales::comma(n_redlist)} red list, {scales::comma(n_both)} both"
  )

  # Breakdowns by source
  if (n_backbone > 0) {
    cli_alert_info("Backbone threat status breakdown:")
    taxa_reference |>
      filter(!is.na(threatStatus_backbone)) |>
      count(threatStatus_backbone, sort = TRUE) |>
      print()
  }

  if (n_redlist > 0) {
    cli_alert_info("Red list threat status breakdown:")
    taxa_reference |>
      filter(!is.na(threatStatus_redlist)) |>
      count(threatStatus_redlist, sort = TRUE) |>
      print()
  }
}

# 3.3 Ensure threat status columns exist -----------------------------------

cli_h2("Filtering Taxonomic Status")

if (!"threatStatus_backbone" %in% names(taxa_reference)) {
  taxa_reference <- taxa_reference |>
    mutate(threatStatus_backbone = NA_character_)
  cli_alert_info("Added empty threatStatus_backbone column")
}

if (!"threatStatus_redlist" %in% names(taxa_reference)) {
  taxa_reference <- taxa_reference |>
    mutate(threatStatus_redlist = NA_character_)
  cli_alert_info("Added empty threatStatus_redlist column")
}

# Taxonomic status summary
if ("taxonomicStatus" %in% names(taxa_reference)) {
  status_summary <- taxa_reference |>
    count(taxonomicStatus, sort = TRUE)

  cli_alert_info("Taxonomic status:")
  print(status_summary)

  n_accepted <- sum(
    taxa_reference$taxonomicStatus == "accepted",
    na.rm = TRUE
  )
  cli_alert_info(
    "Accepted: {scales::comma(n_accepted)} / {scales::comma(nrow(taxa_reference))}"
  )
}

# 3.4 Add derived fields ---------------------------------------------------

cli_h2("Adding Derived Fields")

# Extract numeric ID (works for LSID-style and plain IDs)
if ("taxonID" %in% names(taxa_reference)) {
  taxa_reference <- taxa_reference |>
    mutate(
      backbone_numericID = extract_numeric_id(taxonID)
    )
  cli_alert_success(
    "Added backbone_numericID (numeric ID from taxonID)"
  )
}

# Backbone metadata
taxa_reference <- taxa_reference |>
  mutate(
    taxonomic_backbone = backbone_name,
    backbone_version   = backbone_version,
    backbone_doi       = backbone_doi
  )

cli_alert_success("Added backbone source metadata")

# 3.5 Select and order columns ---------------------------------------------

cli_h2("Organising Columns")

core_columns <- c(
  # Identifiers
  "taxonID", "backbone_numericID",
  "acceptedNameUsageID", "parentNameUsageID",

  # Names
  "scientificName", "scientificNameAuthorship",

  # Classification hierarchy
  "kingdom", "phylum", "class", "order",
  "family", "genus", "species",

  # Status
  "taxonRank", "taxonomicStatus", "nomenclaturalStatus",

  # Distribution & conservation
  "country", "countryCode", "occurrenceStatus",
  "establishmentMeans",
  "threatStatus_backbone", "threatStatus_redlist",

  # Metadata
  "taxonomic_backbone", "backbone_version", "backbone_doi",

  # Additional
  "taxonRemarks", "dynamicProperties"
)

available_core <- intersect(core_columns, names(taxa_reference))
other_columns  <- setdiff(names(taxa_reference), core_columns)

taxa_reference_clean <- taxa_reference |>
  select(all_of(available_core), all_of(other_columns))

cli_alert_info(
  "Column order: {length(available_core)} core + {length(other_columns)} additional"
)

# ============================================================================
# 4. Save Final Taxa Reference
# ============================================================================

cli_h1("Saving Taxa Reference")

saveRDS(
  taxa_reference_clean,
  output_files$taxa_reference,
  compress = "xz"
)
cli_alert_success(
  "Wrote RDS: {.path {output_files$taxa_reference}}"
)

csv_path <- file.path(dir_data_proc, "taxonomy_backbone.csv")
write_csv(taxa_reference_clean, csv_path)
cli_alert_success("Wrote CSV: {.path {csv_path}}")

# Threat status in final output
n_threat_backbone <- sum(
  !is.na(taxa_reference_clean$threatStatus_backbone) &
    taxa_reference_clean$threatStatus_backbone != "",
  na.rm = TRUE
)
n_threat_redlist <- sum(
  !is.na(taxa_reference_clean$threatStatus_redlist) &
    taxa_reference_clean$threatStatus_redlist != "",
  na.rm = TRUE
)

cli_alert_info("Threat status in final output:")
cli_alert_info(
  "  threatStatus_backbone: {scales::comma(n_threat_backbone)}"
)
cli_alert_info(
  "  threatStatus_redlist: {scales::comma(n_threat_redlist)}"
)

# ============================================================================
# 5. Summary Statistics
# ============================================================================

cli_h1("Summary Statistics")

summary_parts <- list(
  tibble::tibble(
    dataset = paste0(backbone_name, " Taxonomy (raw)"),
    n_rows  = nrow(taxonomy_taxon),
    n_cols  = ncol(taxonomy_taxon)
  ),
  tibble::tibble(
    dataset = paste0(backbone_name, " Distribution (raw)"),
    n_rows  = nrow(taxonomy_distr),
    n_cols  = ncol(taxonomy_distr)
  )
)

if (redlist_available) {
  summary_parts <- c(summary_parts, list(
    tibble::tibble(
      dataset = "Red List Taxonomy (raw)",
      n_rows  = nrow(redlist_taxon),
      n_cols  = ncol(redlist_taxon)
    ),
    tibble::tibble(
      dataset = "Red List Distribution (raw)",
      n_rows  = nrow(redlist_distr),
      n_cols  = ncol(redlist_distr)
    )
  ))
}

summary_parts <- c(summary_parts, list(
  tibble::tibble(
    dataset = "Taxa Reference (final)",
    n_rows  = nrow(taxa_reference_clean),
    n_cols  = ncol(taxa_reference_clean)
  )
))

summary_stats <- bind_rows(summary_parts)
print(summary_stats)

# Taxonomic coverage
cli_h2("Taxonomic Coverage")

if ("taxonRank" %in% names(taxa_reference_clean)) {
  rank_summary <- taxa_reference_clean |>
    filter(
      taxonomicStatus == "accepted" | is.na(taxonomicStatus)
    ) |>
    count(taxonRank, sort = TRUE) |>
    head(10)

  cli_alert_info("Top taxonomic ranks (accepted):")
  print(rank_summary)
}

if ("kingdom" %in% names(taxa_reference_clean)) {
  kingdom_summary <- taxa_reference_clean |>
    filter(
      taxonomicStatus == "accepted" | is.na(taxonomicStatus)
    ) |>
    count(kingdom, sort = TRUE)

  cli_alert_info("Kingdoms (accepted):")
  print(kingdom_summary)
}

# ============================================================================
# 6. Validation Checks
# ============================================================================

cli_h1("Validation Checks")

validation_checks <- list(
  "scientificName present" =
    "scientificName" %in% names(taxa_reference_clean),
  "taxonRank present" =
    "taxonRank" %in% names(taxa_reference_clean),
  "taxonID present" =
    "taxonID" %in% names(taxa_reference_clean),
  "threatStatus_backbone present" =
    "threatStatus_backbone" %in% names(taxa_reference_clean),
  "threatStatus_redlist present" =
    "threatStatus_redlist" %in% names(taxa_reference_clean),
  "No duplicate taxonIDs" =
    if ("taxonID" %in% names(taxa_reference_clean)) {
      !anyDuplicated(taxa_reference_clean$taxonID)
    } else {
      NA
    },
  "Accepted taxa exist" =
    if ("taxonomicStatus" %in% names(taxa_reference_clean)) {
      any(
        taxa_reference_clean$taxonomicStatus == "accepted",
        na.rm = TRUE
      )
    } else {
      NA
    }
)

purrr::iwalk(validation_checks, \(result, label) {
  if (is.na(result)) {
    cli_alert_info("{label}: N/A")
  } else if (result) {
    cli_alert_success("{label}")
  } else {
    cli_alert_warning("{label}: FAILED")
  }
})

# ============================================================================
# Done
# ============================================================================

cli_h1("Ingestion Complete")
cli_alert_success(
  "Primary backbone: {backbone_name} ({scales::comma(nrow(taxa_reference_clean))} taxa)"
)
if (redlist_available) {
  cli_alert_success(
    "Threat columns: threatStatus_backbone, threatStatus_redlist"
  )
}
cli_alert_info(
  "Output: {.path {output_files$taxa_reference}}"
)
cli_alert_info("Next: source('scripts/04_convert_cubes_parquet.R')")
