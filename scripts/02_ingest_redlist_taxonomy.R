# scripts/02_ingest_redlist_taxonomy.R
# ==============================================================================
# Swedish Red List & Taxonomy Ingestion
# ==============================================================================
# This script:
# - Reads Swedish Red List distribution and taxon files
# - Standardizes column names to Darwin Core (DwC) terms
# - Joins distribution and taxonomy data
# - Creates a unified taxa reference table
# - Writes processed RDS files to data_proc/

library(here)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(cli)
library(glue)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------
input_files <- list(
  distribution = here(raw_redlist_se_dir, "distribution.txt"),
  taxon = here(raw_redlist_se_dir, "taxon.txt")
)

output_files <- list(
  distribution = here("data_proc", "redlist_se_distribution_current.rds"),
  taxon = here("data_proc", "redlist_se_taxon_current.rds"),
  taxa_reference = here("data_proc", "taxa_reference_current.rds")
)

# Darwin Core term mappings -----------------------------------------------
# Maps lowercase input column names to proper DwC camelCase
dwc_name_map <- c(
  # Identifiers
  "id" = "id",
  "taxonid" = "taxonID",
  "acceptednameusageid" = "acceptedNameUsageID",
  
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
  
  # Occurrence/Distribution terms
  "countrycode" = "countryCode",
  "occurrencestatus" = "occurrenceStatus",
  "establishmentmeans" = "establishmentMeans",
  
  # Project-specific (not standard DwC)
  "threatstatus" = "threatStatus"
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
  cli_alert_info("Reading with delimiter: {ifelse(delim == '\t', 'TAB', delim)}")
  
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

# Read data ---------------------------------------------------------------
cli_h2("Reading Red List Data")

cli_alert_info("Reading distribution data...")
distribution <- read_table_safe(input_files$distribution) |>
  rename_to_dwc()

cli_alert_success(
  "Distribution: {nrow(distribution)} rows, {ncol(distribution)} columns"
)

cli_alert_info("Reading taxonomy data...")
taxon <- read_table_safe(input_files$taxon) |>
  rename_to_dwc()

cli_alert_success(
  "Taxonomy: {nrow(taxon)} rows, {ncol(taxon)} columns"
)

# Save processed inputs ---------------------------------------------------
cli_h2("Saving Processed Files")

saveRDS(distribution, output_files$distribution, compress = "xz")
cli_alert_success("Wrote: {.path {output_files$distribution}}")

saveRDS(taxon, output_files$taxon, compress = "xz")
cli_alert_success("Wrote: {.path {output_files$taxon}}")

# Build unified taxa reference --------------------------------------------
cli_h2("Building Taxa Reference")

# Identify join key
join_key <- case_when(
  "id" %in% names(distribution) && "id" %in% names(taxon) ~ "id",
  "taxonID" %in% names(distribution) && "taxonID" %in% names(taxon) ~ "taxonID",
  TRUE ~ NA_character_
)

if (is.na(join_key)) {
  cli_alert_warning(
    "No common join key found (id or taxonID). Using taxonomy table only."
  )
  taxa_reference <- taxon
} else {
  cli_alert_info("Joining on: {.field {join_key}}")
  
  taxa_reference <- taxon |>
    left_join(
      distribution,
      by = join_key,
      suffix = c("_taxon", "_dist")
    )
  
  cli_alert_success(
    "Joined {nrow(taxon)} taxa with distribution data"
  )
}

# Select and order core columns -------------------------------------------
core_columns <- c(
  # Identifiers
  "id", "taxonID", "acceptedNameUsageID",
  
  # Names
  "scientificName", "scientificNameAuthorship",
  
  # Classification
  "kingdom", "phylum", "class", "order", "family", "genus",
  
  # Status
  "taxonRank", "taxonomicStatus", "nomenclaturalStatus",
  
  # Additional
  "taxonRemarks", "countryCode", "occurrenceStatus", 
  "establishmentMeans", "threatStatus"
)

# Keep available core columns first, then everything else
available_core <- intersect(core_columns, names(taxa_reference))

taxa_reference_clean <- taxa_reference |>
  select(all_of(available_core), everything())

# Save taxa reference -----------------------------------------------------
saveRDS(taxa_reference_clean, output_files$taxa_reference, compress = "xz")
cli_alert_success("Wrote: {.path {output_files$taxa_reference}}")

# Summary statistics ------------------------------------------------------
cli_h2("Summary Statistics")

summary_stats <- tibble::tibble(
  dataset = c("Distribution", "Taxonomy", "Taxa Reference"),
  n_rows = c(nrow(distribution), nrow(taxon), nrow(taxa_reference_clean)),
  n_columns = c(ncol(distribution), ncol(taxon), ncol(taxa_reference_clean))
)

print(summary_stats)

# Validate key fields -----------------------------------------------------
cli_h2("Validation Checks")

validation_checks <- list(
  "scientificName present" = "scientificName" %in% names(taxa_reference_clean),
  "taxonRank present" = "taxonRank" %in% names(taxa_reference_clean),
  "No duplicate IDs" = if (!is.na(join_key)) {
    !anyDuplicated(taxa_reference_clean[[join_key]])
  } else {
    NA
  }
)

purrr::iwalk(validation_checks, ~{
  if (is.na(.x)) {
    cli_alert_info("{(.y)}: NA (not applicable)")
  } else if (.x) {
    cli_alert_success("{(.y)}")
  } else {
    cli_alert_warning("{(.y)}: FAILED")
  }
})

cli_alert_success("Red List ingestion complete!")
