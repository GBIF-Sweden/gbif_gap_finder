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

gap_data_path <- here("shiny_app", "gap_app", "data", "shiny_data.rds")
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

species_summary_files <- c(
  list.files(here(p_derived, "by_order", "species_summary"),
             pattern = "10km\\.csv$", full.names = TRUE),
  list.files(here(p_derived, "by_family", "species_summary"),
             pattern = "10km\\.csv$", full.names = TRUE)
)

if (length(species_summary_files) > 0) {
  cli_alert_info("Found {length(species_summary_files)} species summary files (by_order + by_family)")

  # Read and combine — only need key columns for aggregation
  species_lookup <- rbindlist(
    lapply(species_summary_files, function(f) {
      available <- names(fread(f, nrows = 0))
      use_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "occurrences"), available)
      fread(f, select = use_cols)
    }),
    use.names = TRUE, fill = TRUE
  )

  # One row per species: filter to grid10km + all basis, then aggregate
  species_lookup <- species_lookup[
    basisofrecord == "all" & grid == "grid10km",
    .(total_occurrences = max(occurrences, na.rm = TRUE)),
    by = .(specieskey, species)
  ]

  # Collapse any remaining duplicates (same species name, different specieskey)
  # by keeping the row with the most occurrences
  setorder(species_lookup, species, -total_occurrences)
  species_lookup <- species_lookup[!duplicated(species, fromLast = FALSE)]

  cli_alert_info("Aggregated {scales::comma(nrow(species_lookup))} unique species")

  # ---- Fill taxonomy from match summary ----
  match_summary_path <- here(p_data_proc, "gaps", "taxonomic_match_summary.csv")
  if (!file.exists(match_summary_path)) {
    match_summary_path <- list.files(
      here(p_data_proc), pattern = "taxonomic_match_summary",
      recursive = TRUE, full.names = TRUE
    )[1]
  }

  if (!is.na(match_summary_path) && file.exists(match_summary_path)) {
    ms <- fread(match_summary_path)
    ms_cols <- intersect(
      c("scientificName", "kingdom", "phylum", "class", "order", "family", "threatStatus",
        "establishmentMeans", "occurrenceStatus"),
      names(ms)
    )
    ms <- ms[, ..ms_cols]
    ms <- ms[!duplicated(ms, by = "scientificName")]
    cli_alert_info("Match summary: {scales::comma(nrow(ms))} taxa")

    # Add each taxonomy column via named-vector lookup
    tax_cols <- setdiff(ms_cols, "scientificName")
    for (col in tax_cols) {
      lookup_vec <- setNames(ms[[col]], ms$scientificName)
      species_lookup[[col]] <- unname(lookup_vec[species_lookup$species])
      n_ok <- sum(!is.na(species_lookup[[col]]) & species_lookup[[col]] != "", na.rm = TRUE)
      cli_alert_info("  {col}: {scales::comma(n_ok)} non-NA")
    }
    cli_alert_success("Taxonomy merge complete")
  } else {
    cli_alert_warning("No taxonomic_match_summary found — taxonomy columns will be missing")
  }

  setorder(species_lookup, -total_occurrences)
  explorer_data$species_lookup <- as_tibble(species_lookup)
  cli_alert_success("Species lookup: {scales::comma(nrow(species_lookup))} unique species")

  # ---- Add Swedish vernacular names from Dyntaxa ----
  vn_path <- here(raw_taxonomy_dir, "VernacularName.csv")
  tx_path <- here(raw_taxonomy_dir, "Taxon.csv")

  if (file.exists(vn_path) && file.exists(tx_path)) {
    cli_alert_info("Adding Swedish vernacular names...")
    vn <- fread(vn_path)
    tx <- fread(tx_path, select = c("taxonId", "scientificName"))

    # Filter to Swedish preferred names
    vn_sv <- vn[language == "sv" & isPreferredName == TRUE, .(taxonId, vernacularName)]
    # If no preferred, take first Swedish name
    vn_sv_fallback <- vn[language == "sv", .(taxonId, vernacularName)]
    vn_sv_fallback <- vn_sv_fallback[!duplicated(taxonId)]

    # Join to get scientific name
    vn_joined <- merge(vn_sv, tx, by = "taxonId", all.x = TRUE)
    if (nrow(vn_joined) == 0) {
      vn_joined <- merge(vn_sv_fallback, tx, by = "taxonId", all.x = TRUE)
    }
    vn_joined <- vn_joined[!is.na(scientificName) & scientificName != ""]
    vn_joined <- vn_joined[!duplicated(scientificName)]

    # Merge into species_lookup
    sl <- as.data.table(explorer_data$species_lookup)
    vn_lookup <- setNames(vn_joined$vernacularName, vn_joined$scientificName)
    sl[, vernacular_sv := unname(vn_lookup[species])]
    n_matched <- sum(!is.na(sl$vernacular_sv))
    cli_alert_success("Swedish names matched: {scales::comma(n_matched)} / {scales::comma(nrow(sl))} species")

    # Also add English names if available
    vn_en <- vn[language == "en" & isPreferredName == TRUE, .(taxonId, vernacularName)]
    if (nrow(vn_en) == 0) vn_en <- vn[language == "en"][!duplicated(taxonId), .(taxonId, vernacularName)]
    vn_en_joined <- merge(vn_en, tx, by = "taxonId", all.x = TRUE)
    vn_en_joined <- vn_en_joined[!is.na(scientificName)][!duplicated(scientificName)]
    vn_en_lookup <- setNames(vn_en_joined$vernacularName, vn_en_joined$scientificName)
    sl[, vernacular_en := unname(vn_en_lookup[species])]
    n_en <- sum(!is.na(sl$vernacular_en))
    cli_alert_info("English names matched: {scales::comma(n_en)} / {scales::comma(nrow(sl))} species")

    explorer_data$species_lookup <- as_tibble(sl)
    rm(vn, tx, vn_sv, vn_sv_fallback, vn_joined, vn_en, vn_en_joined, sl)
    gc()
  } else {
    cli_alert_warning("VernacularName.csv or Taxon.csv not found — skipping vernacular names")
  }

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

build_file_index <- function(subdir, prefix_regex) {
  # List files from both by_order and by_family directories
  dirs <- c(
    here(p_derived, "by_order", subdir),
    here(p_derived, "by_family", subdir)
  )
  dirs <- dirs[dir.exists(dirs)]

  if (length(dirs) == 0) return(data.table())

  files <- unlist(lapply(dirs, list.files, pattern = "10km\\.csv$", full.names = TRUE))
  if (length(files) == 0) return(data.table())

  dt <- data.table(filepath = files)
  dt[, filename := basename(filepath)]
  dt[, source_dir := fifelse(str_detect(filepath, "by_family"), "by_family", "by_order")]

  # Extract order (and optionally family) from filename
  dt[, parts := str_remove(filename, prefix_regex)]
  dt[, parts := str_remove(parts, "_10km\\.csv$")]

  # For by_family files: parts = "Order_Family"
  # For by_order files: parts = coded order name (may contain underscores)
  # We can only reliably parse by_family files for order/family
  dt[source_dir == "by_family", order_name := str_extract(parts, "^[^_]+")]
  dt[source_dir == "by_family", family_name := str_remove(parts, "^[^_]+_")]

  # For by_order files, the entire parts string is the order identifier
  dt[source_dir == "by_order", order_name := parts]
  dt[source_dir == "by_order", family_name := NA_character_]

  dt[, c("filename", "parts", "source_dir") := NULL]
  dt
}

cell_index <- build_file_index("species_cell", "^species_cell_")
time_index <- build_file_index("species_time", "^species_time_")

if (nrow(cell_index) > 0) {
  explorer_data$file_index_cell <- as_tibble(cell_index)
  cli_alert_success("Cell file index: {nrow(cell_index)} files")
}
if (nrow(time_index) > 0) {
  explorer_data$file_index_time <- as_tibble(time_index)
  cli_alert_success("Time file index: {nrow(time_index)} files")

  # Build species → time file lookup so the app can find temporal data
  # without needing taxonomy. Read species names from each time file once.
  cli_alert_info("Building species-to-time-file mapping...")
  species_time_map <- rbindlist(lapply(seq_len(nrow(time_index)), function(i) {
    f <- time_index$filepath[i]
    if (!file.exists(f)) return(NULL)
    sp <- tryCatch(
      unique(fread(f, select = "species")$species),
      error = function(e) character(0)
    )
    if (length(sp) == 0) return(NULL)
    if (i %% 200 == 0) cli_alert_info("  ... scanned {i}/{nrow(time_index)} time files")
    data.table(species = sp, time_filepath = f)
  }))

  # Deduplicate: if a species appears in multiple files, keep the by_family
  # version (which has proper taxonomy names) over by_order (coded names)
  species_time_map[, is_family := str_detect(time_filepath, "by_family")]
  setorder(species_time_map, species, -is_family)
  species_time_map <- species_time_map[!duplicated(species)]
  species_time_map[, is_family := NULL]

  explorer_data$species_time_map <- species_time_map
  cli_alert_success("Species-time mapping: {scales::comma(nrow(species_time_map))} species with temporal data")
}

# ===========================================================================
# 4b. BUILD CELL-SPECIES INDEX (for "What Lives Here?" tab)
# ===========================================================================

cli_h2("Building Cell-Species Index")

# Read all species_cell files, aggregate to cell × species totals.
# This avoids scanning all files on every click in the app.
# The full species_cell data is ~15 GB but aggregated to cell × species
# (dropping yearmonth, basisofrecord detail) should be much smaller.

if (nrow(cell_index) > 0) {
  cli_alert_info("Reading {nrow(cell_index)} species_cell files...")

  cell_species_parts <- lapply(seq_len(nrow(cell_index)), function(i) {
    f <- cell_index$filepath[i]
    if (!file.exists(f)) return(NULL)
    dt <- fread(f, select = c("grid", "basisofrecord", "specieskey", "species",
                                "eeacellcode", "occurrences"))
    # Filter to grid10km, all basis, then aggregate
    dt <- dt[grid == "grid10km" & basisofrecord == "all",
             .(occurrences = sum(as.numeric(occurrences), na.rm = TRUE)),
             by = .(specieskey, species, eeacellcode)]
    if (i %% 200 == 0) cli_alert_info("  ... processed {i}/{nrow(cell_index)} files")
    dt
  })

  cell_species_index <- rbindlist(cell_species_parts, use.names = TRUE)

  # Final aggregation (in case species spans multiple files)
  cell_species_index <- cell_species_index[,
    .(occurrences = sum(as.numeric(occurrences), na.rm = TRUE)),
    by = .(specieskey, species, eeacellcode)
  ]

  # Also compute n_cells per species and add to species_lookup
  cells_per_species <- cell_species_index[, .(n_cells = uniqueN(eeacellcode)), by = specieskey]
  if (!is.null(explorer_data$species_lookup)) {
    sl <- as.data.table(explorer_data$species_lookup)
    sl <- merge(sl, cells_per_species, by = "specieskey", all.x = TRUE)
    sl[is.na(n_cells), n_cells := 0L]
    explorer_data$species_lookup <- as_tibble(sl)
  }

  explorer_data$cell_species_index <- cell_species_index
  index_size_mb <- object.size(cell_species_index) / 1024^2
  cli_alert_success("Cell-species index: {scales::comma(nrow(cell_species_index))} rows ({round(index_size_mb, 1)} MB)")

  rm(cell_species_parts)
  gc()
} else {
  cli_alert_warning("No cell files found — 'What Lives Here?' tab will be unavailable")
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

explorer_output_dir <- here("shiny_app", "gbif_explorer", "data")
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
