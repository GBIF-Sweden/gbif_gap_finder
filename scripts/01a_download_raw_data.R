# scripts/01a_download_raw_data.R
# ============================================================================
# Download Raw Data Sources
# ============================================================================
# Downloads all external datasets for a country's gap analysis:
#   1. EEA reference grids (10km and 50km)
#   2. National taxonomy backbone (DwC-A via GBIF)
#   3. National red list (DwC-A via GBIF, optional)
#   3b. National invasive species registry (DwC-A, optional)
#   3c. Sensitive species list (DwC-A, optional)
#   4. GBIF occurrence cubes (SQL API — manual download, instructions printed)
#   5. Administrative boundaries (GADM via geodata package)
#
# All data stored in: data/{country_code}/raw/
#
# Usage:
#   source("scripts/01a_download_raw_data.R")
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# Script-specific packages (everything else loaded by 00_setup.R)
library(jsonlite)

if (!requireNamespace("rgbif", quietly = TRUE)) install.packages("rgbif")
library(rgbif)

# ============================================================================
# Configuration
# ============================================================================

country_code <- cfg_get("country.gbif_country_code", cfg_get("country.code", "SE"))
country_name <- cfg_get("country.name", "Country")

cli_h1("Download Raw Data \u2014 {country_name} ({country_code})")

# Ensure all directories exist
ensure_dirs()

# Logging
log_file <- here(p_logs, paste0("download_log_", Sys.Date(), ".txt"))

log_download <- function(msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", ts, "] ", msg, "\n"), file = log_file, append = TRUE)
  cli_alert_info(msg)
}

# ============================================================================
# Helper Functions
# ============================================================================

download_file_safely <- function(url, destfile, method = "auto") {
  cli_alert_info("Downloading: {.url {url}}")
  tryCatch({
    download.file(url, destfile, method = method, mode = "wb", quiet = FALSE)
    log_download(paste("Downloaded:", basename(destfile)))
    TRUE
  }, error = function(e) {
    cli_alert_danger("Download failed: {e$message}")
    FALSE
  })
}

unzip_safely <- function(zipfile, exdir) {
  cli_alert_info("Unzipping: {.path {basename(zipfile)}}")
  tryCatch({
    unzip(zipfile, exdir = exdir)
    log_download(paste("Extracted:", basename(zipfile)))
    TRUE
  }, error = function(e) {
    cli_alert_danger("Extraction failed: {e$message}")
    FALSE
  })
}

download_gbif_dataset <- function(dataset_key, export_url, dest_dir, label) {
  cli_h2("{label}")

  existing <- list.files(dest_dir, pattern = "\\.(txt|csv)$")
  if (length(existing) > 0) {
    cli_alert_info("{label}: {length(existing)} files already present \u2014 skipping")
    cli_alert_info("Delete files in {.path {dest_dir}} to force re-download")
    return(invisible(TRUE))
  }

  dataset_info <- tryCatch({
    if (packageVersion("rgbif") >= "3.7.9") {
      rgbif::dataset_get(dataset_key)
    } else {
      rgbif::datasets(uuid = dataset_key)
    }
  }, error = function(e) NULL)
  if (!is.null(dataset_info)) {
    title <- dataset_info$title %||% dataset_info$data$title %||% "(unknown)"
    cli_alert_success("Dataset: {title}")
  }

  zip_path <- file.path(dest_dir, paste0(label, "_dwca.zip"))
  ok <- download_file_safely(export_url, zip_path)

  if (ok && file.exists(zip_path)) {
    unzip_safely(zip_path, dest_dir)
    cli_alert_success("{label}: downloaded and extracted")
    return(invisible(TRUE))
  }

  cli_alert_warning("Download failed. Manual: https://www.gbif.org/dataset/{dataset_key}")
  invisible(FALSE)
}

# ============================================================================
# 1. EEA Reference Grids (shared across European countries)
# ============================================================================

cli_h1("1 \u2014 EEA Reference Grids (shared)")

grid_files <- list.files(raw_grid_dir, pattern = "\\.(shp|gpkg)$", recursive = TRUE)

if (length(grid_files) > 0) {
  cli_alert_info("Grids found in {.path {raw_grid_dir}}:")
  for (f in grid_files) cli_alert_info("  {f}")
} else {
  cli_alert_warning("No grid files found in {.path {raw_grid_dir}}")
  cli_alert_info("")
  cli_alert_info("Download EEA reference grids manually:")
  cli_alert_info("  10km (Europe): https://sdi.eea.europa.eu/datastore/public?path=/eea_v_3035_10_km_eea-ref-grid-europe_p_2011_v01_r00")
  cli_alert_info("  50km (Europe): https://sdi.eea.europa.eu/data/aac8379a-5c4e-445c-b2ef-23a6a2701ef0")
  cli_alert_info("")
  cli_alert_info("Place files in: {.path {raw_grid_dir}}")
  cli_alert_info("The pipeline clips to country extent automatically.")
}

# ============================================================================
# 2. National Taxonomy Backbone
# ============================================================================

cli_h1("2 \u2014 National Taxonomy Backbone")

taxonomy_key <- cfg_get("taxonomy.dataset_key", "")
taxonomy_url <- cfg_get("taxonomy.export_url", "")
taxonomy_doi <- cfg_get("taxonomy.doi", "")

if (taxonomy_key == "" || taxonomy_url == "") {
  cli_abort(c("Taxonomy not configured",
    "i" = "Set taxonomy.dataset_key and taxonomy.export_url in config"))
}

cli_alert_info("DOI: {taxonomy_doi}")
download_gbif_dataset(taxonomy_key, taxonomy_url, raw_taxonomy_dir,
  cfg_get("taxonomy.name", "taxonomy"))

# ============================================================================
# 3. National Red List (optional)
# ============================================================================

cli_h1("3 \u2014 National Red List")

redlist_enabled <- cfg_get("redlist.enabled", FALSE)
redlist_key <- cfg_get("redlist.dataset_key", "")
redlist_url <- cfg_get("redlist.export_url", "")
redlist_doi <- cfg_get("redlist.doi", "")

if (!redlist_enabled || redlist_key == "" || redlist_url == "") {
  cli_alert_info("Red list not configured or disabled \u2014 skipping")
} else {
  cli_alert_info("DOI: {redlist_doi}")
  download_gbif_dataset(redlist_key, redlist_url, raw_redlist_dir, "red_list")
}

# ============================================================================
# 3b. National Invasive Species Registry (optional)
# ============================================================================

cli_h1("3b \u2014 National Invasive Species Registry")

invasives_enabled <- cfg_get("invasives.enabled", FALSE)
invasives_key <- cfg_get("invasives.dataset_key", "")
invasives_url <- cfg_get("invasives.export_url", "")
invasives_doi <- cfg_get("invasives.doi", "")

if (!invasives_enabled || invasives_key == "" || invasives_url == "") {
  cli_alert_info("Invasive species registry not configured or disabled \u2014 skipping")
} else {
  dir.create(raw_invasives_dir, showWarnings = FALSE, recursive = TRUE)
  cli_alert_info("DOI: {invasives_doi}")
  download_gbif_dataset(invasives_key, invasives_url, raw_invasives_dir, "invasives")
}

# ============================================================================
# 3c. Sensitive Species List (optional)
# ============================================================================

cli_h1("3c \u2014 Sensitive Species List")

sensitive_enabled <- cfg_get("sensitive.enabled", FALSE)
sensitive_key <- cfg_get("sensitive.dataset_key", "")
sensitive_url <- cfg_get("sensitive.export_url", "")
sensitive_doi <- cfg_get("sensitive.doi", "")

if (!sensitive_enabled || sensitive_key == "" || sensitive_url == "") {
  cli_alert_info("Sensitive species list not configured or disabled \u2014 skipping")
} else {
  dir.create(raw_sensitive_dir, showWarnings = FALSE, recursive = TRUE)
  cli_alert_info("DOI: {sensitive_doi}")
  download_gbif_dataset(sensitive_key, sensitive_url, raw_sensitive_dir, "sensitive")
}

# ============================================================================
# 4. GBIF Occurrence Cubes (SQL API)
# ============================================================================

cli_h1("4 \u2014 GBIF Occurrence Cubes")

cube_files <- list.files(raw_gbif_cube_dir, pattern = "\\.(csv|parquet)$")

if (length(cube_files) >= 2) {
  cli_alert_info("Cubes found: {paste(cube_files, collapse = ', ')} \u2014 skipping")
} else {
  cli_alert_warning("Cubes not found in {.path {raw_gbif_cube_dir}}")
  cli_alert_info("")
  cli_alert_info("Download via GBIF SQL API: https://www.gbif.org/occurrence/download/sql")
  cli_alert_info("")

  sql_query <- glue::glue('
SELECT
  specieskey, species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode({{RESOLUTION}}, decimallatitude, decimallongitude, 0) AS eeacellcode,
  "year", "month",
  COUNT(*) AS occurrences
FROM occurrence
WHERE countrycode = \'{country_code}\'
  AND hascoordinate = TRUE AND hasgeospatialissues = FALSE
  AND occurrencestatus = \'PRESENT\' AND specieskey IS NOT NULL
GROUP BY specieskey, species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode({{RESOLUTION}}, decimallatitude, decimallongitude, 0),
  "year", "month"', .open = "{{", .close = "}}")

  cli_alert_info("SQL query (replace RESOLUTION with 10000 or 50000):")
  cat("\n", gsub("\\{\\{RESOLUTION\\}\\}", "10000", sql_query), "\n\n")
  cli_alert_info("After download, unzip and place CSVs as:")
  cli_alert_info("  {.path {here(raw_gbif_cube_dir, 'cube_10km.csv')}}")
  cli_alert_info("  {.path {here(raw_gbif_cube_dir, 'cube_50km.csv')}}")
}

# ============================================================================
# 5. Administrative Boundaries (GADM)
# ============================================================================

cli_h1("5 \u2014 Administrative Boundaries (GADM)")

admin_enabled <- cfg_get("admin_boundaries.enabled", FALSE)

if (!admin_enabled) {
  cli_alert_info("Admin boundaries disabled \u2014 skipping")
} else if (!requireNamespace("geodata", quietly = TRUE)) {
  cli_alert_warning("Package {.pkg geodata} needed: install.packages('geodata')")
} else {
  library(geodata)
  # sf already loaded by load_packages()
  dir.create(raw_admin_dir, showWarnings = FALSE, recursive = TRUE)

  iso3 <- cfg_get("admin_boundaries.country_code_iso3", "")
  if (iso3 == "") {
    iso2_to_iso3 <- c("SE"="SWE","NO"="NOR","FI"="FIN","DK"="DNK","DE"="DEU",
      "NL"="NLD","GB"="GBR","FR"="FRA","ES"="ESP","IT"="ITA","PL"="POL",
      "EE"="EST","LV"="LVA","LT"="LTU","ET"="ETH","KE"="KEN","ZA"="ZAF",
      "US"="USA","AU"="AUS","BR"="BRA","LU"="LUX")
    iso3 <- iso2_to_iso3[country_code]
  }

  levels <- cfg_get("admin_boundaries.levels", c(1, 2))
  force_dl <- cfg_get("admin_boundaries.force_download", FALSE)

  if (!is.na(iso3)) {
    for (lvl in levels) {
      out_path <- here(raw_admin_dir, paste0("admin_level", lvl, ".gpkg"))
      if (file.exists(out_path) && !force_dl) {
        cli_alert_info("Level {lvl} exists \u2014 skipping")
        next
      }
      tryCatch({
        gadm_data <- gadm(country = iso3, level = lvl, path = tempdir(),
                          version = "4.1", resolution = 1)
        admin_sf <- st_as_sf(gadm_data)
        name_col <- paste0("NAME_", lvl)
        gid_col <- paste0("GID_", lvl)
        type_col <- paste0("TYPE_", lvl)
        admin_sf <- admin_sf |>
          dplyr::mutate(
            admin_name = if (name_col %in% names(admin_sf)) .data[[name_col]] else NA_character_,
            admin_code = if (gid_col %in% names(admin_sf)) .data[[gid_col]] else NA_character_,
            admin_type = if (type_col %in% names(admin_sf)) .data[[type_col]] else NA_character_,
            admin_level = lvl, country = country_name) |>
          st_simplify(dTolerance = if (lvl == 1) 1000 else 500) |>
          dplyr::select(admin_name, admin_code, admin_type, admin_level, country) |>
          st_transform(4326)
        st_write(admin_sf, out_path, delete_dsn = TRUE, quiet = TRUE)
        cli_alert_success("Level {lvl}: {nrow(admin_sf)} units")
      }, error = function(e) {
        cli_alert_danger("Failed level {lvl}: {e$message}")
      })
    }
  } else {
    cli_alert_danger("Cannot determine ISO3 code for '{country_code}'")
  }
}

# ============================================================================
# Summary
# ============================================================================

cli_h1("Summary")

metadata <- list(
  country = country_code,
  date = as.character(Sys.Date()),
  data_dir = p_data,
  grids = list(dir = raw_grid_dir, files = list.files(raw_grid_dir, pattern = "\\.(shp|gpkg)$", recursive = TRUE)),
  taxonomy = list(doi = taxonomy_doi, files = list.files(raw_taxonomy_dir, pattern = "\\.(txt|csv)$")),
  redlist = list(doi = redlist_doi, files = list.files(raw_redlist_dir, pattern = "\\.(txt|csv)$")),
  invasives = list(
    doi = if (exists("invasives_doi")) invasives_doi else "",
    files = if (exists("raw_invasives_dir") && dir.exists(raw_invasives_dir))
      list.files(raw_invasives_dir, pattern = "\\.(txt|csv)$") else character(0)
  ),
  sensitive = list(
    doi = if (exists("sensitive_doi")) sensitive_doi else "",
    files = if (exists("raw_sensitive_dir") && dir.exists(raw_sensitive_dir))
      list.files(raw_sensitive_dir, pattern = "\\.(txt|csv)$") else character(0)
  ),
  cubes = list.files(raw_gbif_cube_dir, pattern = "\\.(csv|parquet)$"),
  admin = list.files(raw_admin_dir, pattern = "\\.gpkg$")
)
write_json(metadata, here(p_data_raw, "download_metadata.json"), pretty = TRUE, auto_unbox = TRUE)

checks <- c(
  `Grids (shared)` = length(metadata$grids$files) > 0,
  Taxonomy = length(metadata$taxonomy$files) > 0,
  `Red list` = !redlist_enabled || length(metadata$redlist$files) > 0,
  Invasives = !invasives_enabled || length(metadata$invasives$files) > 0,
  Sensitive = !sensitive_enabled || length(metadata$sensitive$files) > 0,
  Cubes = length(metadata$cubes) >= 2,
  Admin = !admin_enabled || length(metadata$admin) > 0
)
for (nm in names(checks)) {
  if (checks[[nm]]) cli_alert_success("{nm}: OK") else cli_alert_warning("{nm}: Missing")
}

cli_alert_success("Done! Next: source('scripts/02_ingest_grids.R')")
