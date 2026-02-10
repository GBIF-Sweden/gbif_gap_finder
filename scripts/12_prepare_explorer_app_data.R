# scripts/12_prepare_explorer_app_data.R
# ==============================================================================
# Prepare Shiny Data for GBIF Explorer App
# ==============================================================================
# This script builds the data package for the explorer app, which needs:
#   1. Everything from the gap app (grid, taxonomy, temporal data)
#   2. Species lookup table (combined from species_summary files)
#   3. Path to derived data for on-demand species loading
#
# The explorer app loads species_cell and species_time files on-demand
# at runtime (too large to bundle: ~15 GB cell data, ~1.3 GB time data).
#
# Run AFTER script 11 (gap app data must exist first).
# ==============================================================================

library(here)
library(data.table)
library(dplyr)
library(stringr)
library(cli)
library(scales)

source(here("scripts", "00_setup.R"))

cli_h1("Preparing Explorer App Data")

# ===========================================================================
# 1. LOAD GAP APP DATA AS BASE
# ===========================================================================

cli_h2("Loading Gap App Data")

gap_data_path <- here("shiny_app", "gap_analysis", "data", "shiny_data.rds")
if (!file.exists(gap_data_path)) {
  cli_abort(c(
    "Gap app data not found: {.path {gap_data_path}}",
    "i" = "Run script 11_prepare_gap_app_data.R first"
  ))
}

explorer_data <- readRDS(gap_data_path)
cli_alert_success("Loaded gap app data: {length(names(explorer_data))} datasets")

# ===========================================================================
# 2. BUILD SPECIES LOOKUP TABLE
# ===========================================================================

cli_h2("Building Species Lookup Table")

p_derived <- here(p_data_proc, "derived")

species_summary_files <- list.files(
  p_derived, pattern = "species_summary.*10km\\.csv$",
  recursive = TRUE, full.names = TRUE
)

if (length(species_summary_files) > 0) {
  cli_alert_info("Found {length(species_summary_files)} species summary files")

  # Read and combine all species summary files
  species_lookup <- rbindlist(
    lapply(species_summary_files, function(f) {
      fread(f, select = c("grid", "basisofrecord", "specieskey", "species",
                           "order", "family", "class", "occurrences"))
    }),
    use.names = TRUE, fill = TRUE
  )

  # Aggregate to one row per species (basisofrecord == "all", grid10km)
  species_lookup <- species_lookup[
    basisofrecord == "all" & grid == "grid10km",
    .(total_occurrences = sum(occurrences, na.rm = TRUE)),
    by = .(specieskey, species, order, family, class)
  ]

  cli_alert_info("Aggregated {scales::comma(nrow(species_lookup))} species")

  # Add kingdom, phylum, threatStatus from taxonomic match summary
  match_summary_path <- here(p_data_proc, "gaps", "taxonomic_match_summary.csv")
  if (!file.exists(match_summary_path)) {
    # Try alternative location
    match_summary_path <- list.files(
      here(p_data_proc), pattern = "taxonomic_match_summary",
      recursive = TRUE, full.names = TRUE
    )[1]
  }

  if (!is.na(match_summary_path) && file.exists(match_summary_path)) {
    ms <- fread(match_summary_path,
      select = intersect(
        c("scientificName", "kingdom", "phylum", "threatStatus"),
        names(fread(match_summary_path, nrows = 0))
      )
    )
    ms <- ms[!duplicated(ms, by = "scientificName")]

    species_lookup <- merge(
      species_lookup, ms,
      by.x = "species", by.y = "scientificName",
      all.x = TRUE
    )
    n_threat <- sum(!is.na(species_lookup$threatStatus) & species_lookup$threatStatus != "")
    cli_alert_success("Merged taxonomy: {scales::comma(n_threat)} species with threat status")
  } else {
    # Fallback: try from the gap app's taxonomic_match_summary
    if (!is.null(explorer_data$taxonomic_match_summary)) {
      ms <- as.data.table(explorer_data$taxonomic_match_summary)
      cols <- intersect(c("scientificName", "kingdom", "phylum", "threatStatus"), names(ms))
      if (length(cols) >= 2) {
        ms <- ms[!duplicated(ms, by = "scientificName"), ..cols]
        species_lookup <- merge(
          species_lookup, ms,
          by.x = "species", by.y = "scientificName",
          all.x = TRUE
        )
        cli_alert_success("Merged taxonomy from gap app data")
      }
    }
  }

  setorder(species_lookup, -total_occurrences)
  explorer_data$species_lookup <- as_tibble(species_lookup)
  cli_alert_success("Species lookup: {scales::comma(nrow(species_lookup))} species")

} else {
  cli_abort(c(
    "No species summary files found in {.path {p_derived}}",
    "i" = "Run script 06b to create species-level summaries"
  ))
}

# ===========================================================================
# 3. STORE DERIVED DATA PATH
# ===========================================================================

# The explorer app reads species_cell and species_time files on-demand
# because they're too large to bundle (15+ GB total).
# Store the path so the app knows where to find them.
explorer_data$derived_data_path <- normalizePath(p_derived, mustWork = FALSE)

cli_alert_info("Derived data path: {.path {explorer_data$derived_data_path}}")

# Verify key file patterns exist
n_cell_files <- length(list.files(p_derived, pattern = "species_cell.*10km", recursive = TRUE))
n_time_files <- length(list.files(p_derived, pattern = "species_time_[^c].*10km", recursive = TRUE))
cli_alert_info("Species cell files available: {scales::comma(n_cell_files)}")
cli_alert_info("Species time files available: {scales::comma(n_time_files)}")

# ===========================================================================
# 4. BUILD FILE INDEX FOR ON-DEMAND LOADING
# ===========================================================================

cli_h2("Building File Index")

# Create an index mapping order → file path for fast lookup at runtime
# This avoids expensive list.files() calls when the app needs to load data

build_file_index <- function(pattern) {
  files <- list.files(p_derived, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(data.table())

  dt <- data.table(filepath = files)
  dt[, filename := basename(filepath)]

  # Extract order (and optionally family) from filename
  # Patterns: species_cell_{Order}_{grid}.csv or species_cell_{Order}_{Family}_{grid}.csv
  dt[, parts := str_remove(filename, "^species_(cell|time|summary)_")]
  dt[, parts := str_remove(parts, "_10km\\.csv$")]

  # If parts contain underscore, first part is order, second is family
  dt[, order_name := str_extract(parts, "^[^_]+")]
  dt[, family_name := fifelse(
    str_detect(parts, "_"),
    str_extract(parts, "(?<=_).+$"),
    NA_character_
  )]

  dt[, c("filename", "parts") := NULL]
  dt
}

cell_index <- build_file_index("species_cell.*10km\\.csv$")
time_index <- build_file_index("species_time_[^c].*10km\\.csv$")

if (nrow(cell_index) > 0) {
  explorer_data$file_index_cell <- as_tibble(cell_index)
  cli_alert_success("Cell file index: {nrow(cell_index)} files")
}
if (nrow(time_index) > 0) {
  explorer_data$file_index_time <- as_tibble(time_index)
  cli_alert_success("Time file index: {nrow(time_index)} files")
}

# ===========================================================================
# 5. UPDATE METADATA
# ===========================================================================

cli_h2("Updating Metadata")

explorer_data$metadata <- list(
  created_at = Sys.time(),
  created_by = "scripts/12_prepare_explorer_app_data.R",
  r_version = R.version.string,
  n_datasets = length(names(explorer_data)),
  datasets = names(explorer_data),

  # Summary counts
  n_cells_10km = if (!is.null(explorer_data$grid_10km)) nrow(explorer_data$grid_10km) else NA,
  n_species = nrow(explorer_data$species_lookup),

  # Data availability flags
  has_spatial = !is.null(explorer_data$spatial_gaps_10km),
  has_temporal = !is.null(explorer_data$time_summary_10km),
  has_taxonomic = !is.null(explorer_data$tax_by_rank),
  has_threat_status = !is.null(explorer_data$tax_by_threat),
  has_species_lookup = TRUE,
  has_derived_data = TRUE,
  n_cell_files = nrow(cell_index),
  n_time_files = nrow(time_index)
)

# ===========================================================================
# 6. SAVE
# ===========================================================================

cli_h2("Saving Explorer Data")

explorer_output_dir <- here("shiny_app", "explorer", "data")
if (!dir.exists(explorer_output_dir)) {
  dir.create(explorer_output_dir, recursive = TRUE)
  cli_alert_success("Created directory: {.path {explorer_output_dir}}")
}

explorer_data_path <- here(explorer_output_dir, "shiny_data.rds")
saveRDS(explorer_data, explorer_data_path, compress = "xz")

file_size_mb <- file.size(explorer_data_path) / 1024^2
cli_alert_success("Saved: {.path {explorer_data_path}} ({round(file_size_mb, 2)} MB)")

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Explorer Data Summary")
cli_alert_info("Species lookup: {scales::comma(nrow(explorer_data$species_lookup))} species")
cli_alert_info("File index (cell): {nrow(cell_index)} files")
cli_alert_info("File index (time): {nrow(time_index)} files")
cli_alert_info("Derived data path: {.path {explorer_data$derived_data_path}}")
cli_alert_info("Output: {.path {explorer_data_path}}")
cli_alert_info("Size: {round(file_size_mb, 2)} MB")
