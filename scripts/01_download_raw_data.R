# scripts/01_download_raw_data.R
# ==============================================================================
# Automated Download of Raw Data Sources
# ==============================================================================
# This script downloads all external datasets needed for the gap analysis:
# 1. EEA reference grids (10km and 50km)
# 2. Swedish Red List taxonomy (via GBIF)
# 3. GBIF Occurrence Cubes (filtered for Sweden, split by basisOfRecord)
#
# Run this script:
# - On initial setup (after 00_setup.R)
# - When updating to new versions of reference datasets
# - When GBIF occurrence data needs refreshing
#
# All downloads go to data_raw/ (never manually edited)
# Download metadata is logged for reproducibility

library(here)
library(dplyr)
library(readr)
library(cli)
library(httr)
library(jsonlite)

# rgbif for GBIF downloads
if (!requireNamespace("rgbif", quietly = TRUE)) {
  cli_alert_warning("Package {.pkg rgbif} not installed. Installing now...")
  install.packages("rgbif")
}
library(rgbif)

source(here("scripts", "00_setup.R"))


# GBIF credentials ---------------------------------------------------------
# Set these in your .Renviron file or run interactively before downloading:
#   GBIF_USER=your_username
#   GBIF_EMAIL=your_email@example.com
#   GBIF_PWD=your_password

if (Sys.getenv("GBIF_USER") != "") {
  
  options(gbif_user = Sys.getenv("GBIF_USER"))
  options(gbif_email = Sys.getenv("GBIF_EMAIL"))
  options(gbif_pwd = Sys.getenv("GBIF_PWD"))
  cli_alert_success("GBIF credentials loaded from environment")
} else {
  
  cli_alert_warning("GBIF credentials not found in environment variables")
  cli_alert_info("Set GBIF_USER, GBIF_EMAIL, GBIF_PWD in .Renviron or run:")
  cli_alert_info("  options(gbif_user = 'xxx', gbif_email = 'xxx', gbif_pwd = 'xxx')")
}

# Configuration (from config.yml) ------------------------------------------
dir_data_raw   <- here(cfg_get("paths.data_raw", "data_raw"))
dir_grids_10km <- here(cfg_get("paths.grid_10km_dir", "data_raw/eea_grid_10km"))
dir_grids_50km <- here(cfg_get("paths.grid_50km_dir", "data_raw/eea_grid_50km"))
dir_redlist    <- here(cfg_get("paths.redlist_se_dir", "data_raw/red_list_se"))
dir_cubes      <- here(cfg_get("paths.gbif_cube_dir", "data_raw/gbif_occurrence_cubes"))
dir_logs       <- here(cfg_get("paths.logs", "logs"))


# Create directories
for (d in c(dir_grids_10km, dir_grids_50km, dir_redlist, dir_cubes)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# Logging
log_dir <- here(cfg_get("paths.logs", "logs"))
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log_file <- here(log_dir, paste0("download_log_", Sys.Date(), ".txt"))

log_download <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste0("[", timestamp, "] ", msg)
  cat(message, "\n", file = log_file, append = TRUE)
  cli_alert_info(msg)
}

# Helper functions --------------------------------------------------------

download_file_safely <- function(url, destfile, method = "auto") {
  cli_alert_info("Downloading: {.url {url}}")
  cli_alert_info("Destination: {.path {destfile}}")
  
  tryCatch({
    download.file(url, destfile, method = method, mode = "wb", quiet = FALSE)
    log_download(paste("Successfully downloaded:", basename(destfile)))
    return(TRUE)
  }, error = function(e) {
    cli_alert_danger("Download failed: {e$message}")
    log_download(paste("ERROR:", e$message))
    return(FALSE)
  })
}

unzip_safely <- function(zipfile, exdir) {
  cli_alert_info("Unzipping: {.path {basename(zipfile)}}")
  
  tryCatch({
    unzip(zipfile, exdir = exdir)
    log_download(paste("Successfully extracted:", basename(zipfile)))
    return(TRUE)
  }, error = function(e) {
    cli_alert_danger("Extraction failed: {e$message}")
    log_download(paste("ERROR during extraction:", e$message))
    return(FALSE)
  })
}

# =========================================================================
# 1. EEA REFERENCE GRIDS
# =========================================================================

cli_h1("Downloading EEA Reference Grids")

# 1.1 EEA 10km Grid (Sweden) ------------------------------------------
cli_h2("EEA 10km Grid (Sweden)")

eea_10km_url <- "https://sdi.eea.europa.eu/datashare/s/10km_Grid_Sweden"
eea_10km_zip <- here(dir_grids_10km, "se_10km.zip")

cli_alert_info("Note: Direct download URL may require manual download")
cli_alert_info("If automated download fails, please download manually from:")
cli_alert_info("https://sdi.eea.europa.eu/geonetwork/srv/api/records/10ade0f6-5cf8-4bf8-8db8-313081857af3")
cli_alert_info("And place in: {.path {dir_grids_10km}}")

# Attempt download (may need manual intervention)
cat("\nDo you want to attempt automated download of 10km grid? (y/n): ")
if (interactive() && tolower(readline()) == "y") {
  download_file_safely(eea_10km_url, eea_10km_zip)
  
  if (file.exists(eea_10km_zip)) {
    unzip_safely(eea_10km_zip, dir_grids_10km)
    file.remove(eea_10km_zip)  # Clean up zip
  }
} else {
  cli_alert_info("Skipping automated download - add files manually if needed")
}

# 1.2 EEA 50km Grid (Europe, filtered for Sweden) ---------------------
cli_h2("EEA 50km Grid (Europe)")

eea_50km_url <- "https://sdi.eea.europa.eu/datashare/s/EEA_50km_Grid_v2024"
eea_50km_file <- here(dir_grids_50km, "EEA_50km_grid_v2024.gpkg")

cli_alert_info("Note: Direct download URL may require manual download")
cli_alert_info("If automated download fails, please download manually from:")
cli_alert_info("https://sdi.eea.europa.eu/geonetwork/srv/api/records/aac8379a-5c4e-445c-b2ef-23a6a2701ef0")
cli_alert_info("And place in: {.path {dir_grids_50km}}")

cat("\nDo you want to attempt automated download of 50km grid? (y/n): ")
if (interactive() && tolower(readline()) == "y") {
  download_file_safely(eea_50km_url, eea_50km_file)
} else {
  cli_alert_info("Skipping automated download - add files manually if needed")
}

# =========================================================================
# 2. SWEDISH RED LIST TAXONOMY (via GBIF)
# =========================================================================

cli_h1("Downloading Swedish Red List Taxonomy")

# Dataset key for Swedish Red List
# SLU Artdatabanken (2024). The Swedish Red List 2020. Version 1.8
# https://doi.org/10.15468/jhwkpq
redlist_dataset_key <- "fab88965-e69d-4491-a04d-e3198b626e52"

cli_alert_info("Dataset: Swedish Red List 2020 (SLU Artdatabanken)")
cli_alert_info("DOI: https://doi.org/10.15468/jhwkpq")
cli_alert_info("GBIF Dataset Key: {redlist_dataset_key}")

# Download using GBIF API
cli_h2("Downloading via GBIF Checklist Download")

# Get dataset metadata
dataset_info <- tryCatch({
  rgbif::datasets(uuid = redlist_dataset_key)
}, error = function(e) {
  cli_alert_danger("Could not retrieve dataset metadata: {e$message}")
  NULL
})

if (!is.null(dataset_info)) {
  cli_alert_success("Dataset found: {dataset_info$data$title}")
  cli_alert_info("Version: {dataset_info$data$version}")
  cli_alert_info("Published: {dataset_info$data$pubDate}")
}

# Download DwC-A (Darwin Core Archive)
dwca_url <- paste0("https://api.gbif.org/v1/occurrence/download/dataset/", redlist_dataset_key)
dwca_zip <- here(dir_redlist, "swedish_redlist_dwca.zip")

cli_alert_info("Attempting to download Darwin Core Archive...")

# Alternative: Direct dataset export URL
export_url <- paste0("https://ipt.gbif.se/archive.do?r=swedish-red-list&v=1.8")

download_success <- download_file_safely(export_url, dwca_zip)

if (download_success && file.exists(dwca_zip)) {
  # Extract the archive
  unzip_safely(dwca_zip, dir_redlist)
  
  # The archive typically contains:
  # - taxon.txt (taxonomy)
  # - distribution.txt (distribution & threat status)
  # - meta.xml (metadata)
  
  cli_alert_success("Swedish Red List taxonomy downloaded and extracted")
  
  # Verify expected files
  expected_files <- c("taxon.txt", "distribution.txt")
  found_files <- list.files(dir_redlist, pattern = "\\.txt$")
  
  for (f in expected_files) {
    if (f %in% found_files) {
      cli_alert_success("Found: {f}")
    } else {
      cli_alert_warning("Missing expected file: {f}")
    }
  }
  
} else {
  cli_alert_warning("Automated download failed. Manual download instructions:")
  cli_alert_info("1. Visit: https://www.gbif.org/dataset/fab88965-e69d-4491-a04d-e3198b626e52")
  cli_alert_info("2. Click 'Download' and select 'Darwin Core Archive'")
  cli_alert_info("3. Extract to: {.path {dir_redlist}}")
}

# =========================================================================
# 3. GBIF OCCURRENCE CUBES (Sweden, by basisOfRecord)
# =========================================================================

cli_h1("Downloading GBIF Occurrence Cubes")

cli_alert_info("This will download pre-computed occurrence cubes for Sweden")
cli_alert_info("Split by basisOfRecord at 10km and 50km resolution")
cli_alert_warning("Note: Downloads can be large (hundreds of MB) and take time")

# Sweden country code
sweden_code <- "SE"

# Basis of record types to download
basis_types <- c(
  "occurrence",           # All combined
  "observation",          # Generic observation
  "humanObservation",     # Citizen science
  "machineObservation",   # Camera traps, sensors
  "preservedSpecimen",    # Museum specimens
  "fossilSpecimen",       # Fossils
  "livingSpecimen",       # Living collections
  "materialSample",       # Tissue samples
  "materialCitation"      # Literature citations
)

# Grid resolutions
grid_resolutions <- c("10km", "50km")

cli_alert_info("Will download {length(basis_types)} basis types × {length(grid_resolutions)} grids = {length(basis_types) * length(grid_resolutions)} files")

cat("\nDo you want to proceed with GBIF Occurrence Cube downloads? (y/n): ")
if (!interactive() || tolower(readline()) == "y") {
  
  cli_h2("Requesting GBIF Occurrence Cube Downloads")
  
  # Note: GBIF occurrence cubes are requested via download API
  # They are pre-aggregated and faster than raw occurrence downloads
  
  cli_alert_warning("IMPORTANT: GBIF Occurrence Cube downloads require authentication")
  cli_alert_info("You need to set GBIF credentials:")
  cli_alert_info("1. Create account at https://www.gbif.org")
  cli_alert_info("2. Set credentials in R:")
  cli_alert_info("   options(gbif_user = 'your_username')")
  cli_alert_info("   options(gbif_email = 'your_email')")
  cli_alert_info("   options(gbif_pwd = 'your_password')")
  
  # Check credentials
  gbif_user <- getOption("gbif_user")
  gbif_email <- getOption("gbif_email")
  gbif_pwd <- getOption("gbif_pwd")
  
  if (is.null(gbif_user) || is.null(gbif_email) || is.null(gbif_pwd)) {
    cli_alert_danger("GBIF credentials not set!")
    cli_alert_info("Please set credentials and re-run this section")
    cli_alert_info("Or download manually from the DOIs listed in data_sources.Rmd")
  } else {
    cli_alert_success("GBIF credentials found")
    
    # Since GBIF Occurrence Cubes are special downloads, provide manual instructions
    # for now with the exact DOIs/download keys you already have
    
    cli_h2("GBIF Occurrence Cube Download Instructions")
    cli_alert_info("Occurrence cubes are specialized downloads.")
    cli_alert_info("Your existing downloads are documented in data_sources.Rmd")
    cli_alert_info("To refresh/update:")
    cli_alert_info("")
    cli_alert_info("Option 1: Use existing download keys")
    cli_alert_info("  - Download from: https://doi.org/10.15468/dl.wzv3uc (10km)")
    cli_alert_info("  - Download from: https://doi.org/10.15468/dl.qyp3uw (50km)")
    cli_alert_info("")
    cli_alert_info("Option 2: Create new cube downloads via GBIF web interface")
    cli_alert_info("  1. Visit: https://www.gbif.org/occurrence/download")
    cli_alert_info("  2. Filter by country: Sweden")
    cli_alert_info("  3. Select 'Occurrence Cube' format")
    cli_alert_info("  4. Choose grid resolution (EEA 10km or 50km)")
    cli_alert_info("  5. Optionally filter by basisOfRecord")
    cli_alert_info("")
    cli_alert_info("Option 3: Download programmatically (advanced)")
    cli_alert_info("  - See: https://docs.ropensci.org/rgbif/articles/downloading_occurrence_cubes.html")
    
    # Provide code template for future cube downloads
    cli_alert_info("")
    cli_alert_info("Example code for requesting new cube download:")
    cli_alert_info('
cube_download <- occ_download(
  pred("country", "SE"),
  pred("hasCoordinate", TRUE),
  format = "SPECIES_LIST",
  user = gbif_user,
  pwd = gbif_pwd,
  email = gbif_email
)

# Check status
occ_download_meta(cube_download)

# Once complete, download
occ_download_get(cube_download, path = dir_cubes)
    ')
  }
  
} else {
  cli_alert_info("Skipping GBIF Occurrence Cube downloads")
  cli_alert_info("Using existing files in {.path {dir_cubes}}")
}

# =========================================================================
# SUMMARY & METADATA
# =========================================================================

cli_h1("Download Summary")

# Create download metadata file

metadata <- list(
  download_date = as.character(Sys.Date()),
  download_timestamp = as.character(Sys.time()),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  rgbif_version = as.character(packageVersion("rgbif")),
  
  eea_grids = list(
    grid_10km_dir = dir_grids_10km,
    grid_10km_files = list.files(dir_grids_10km),
    grid_50km_dir = dir_grids_50km,
    grid_50km_files = list.files(dir_grids_50km)
  ),
  
  swedish_redlist = list(
    dir = dir_redlist,
    files = list.files(dir_redlist),
    dataset_key = redlist_dataset_key,
    doi = "https://doi.org/10.15468/jhwkpq"
  ),
  
  gbif_cubes = list(
    dir = dir_cubes,
    files = list.files(dir_cubes, pattern = "\\.csv$")
  )
)

metadata_file <- here(dir_data_raw, "download_metadata.json")
write_json(metadata, metadata_file, pretty = TRUE, auto_unbox = TRUE)

cli_alert_success("Download metadata saved to: {.path {metadata_file}}")
cli_alert_success("Download log saved to: {.path {log_file}}")

# Summary table
cli_h2("Files Summary")

summary_df <- tibble::tribble(
  ~Dataset, ~Location, ~Files,
  "EEA 10km Grid", dir_grids_10km, length(list.files(dir_grids_10km)),
  "EEA 50km Grid", dir_grids_50km, length(list.files(dir_grids_50km)),
  "Swedish Red List", dir_redlist, length(list.files(dir_redlist)),
  "GBIF Cubes", dir_cubes, length(list.files(dir_cubes, pattern = "\\.csv$"))
)

print(summary_df)

cli_alert_success("Download script complete!")
cli_alert_info("Next steps:")
cli_alert_info("  1. Verify downloaded files in data_raw/ directories")
cli_alert_info("  2. Update config.yml if filenames changed")
cli_alert_info("  3. Run: source('scripts/02_ingest_grids.R')")

# =========================================================================
# VERIFICATION CHECKS
# =========================================================================

cli_h1("Verification Checks")

# Check for critical files
critical_files <- list(
  "EEA 10km shapefile" = file.path(dir_grids_10km, "se_10km.shp"),
  "EEA 50km geopackage" = file.path(dir_grids_50km, "EEA_50km_grid_v2024.gpkg"),
  "Red List taxonomy" = file.path(dir_redlist, "taxon.txt"),
  "Red List distribution" = file.path(dir_redlist, "distribution.txt")
)

all_ok <- TRUE
for (label in names(critical_files)) {
  path <- critical_files[[label]]
  if (file.exists(path)) {
    cli_alert_success("{label}: Found")
  } else {
    cli_alert_warning("{label}: Missing - {.path {path}}")
    all_ok <- FALSE
  }
}

# Check cube files
cube_files <- list.files(dir_cubes, pattern = "\\.csv$", full.names = FALSE)
if (length(cube_files) > 0) {
  cli_alert_success("GBIF cubes: {length(cube_files)} CSV files found")
} else {
  cli_alert_warning("GBIF cubes: No CSV files found - may need manual download")
  all_ok <- FALSE
}

if (all_ok) {
  cli_alert_success("All critical files present - ready to proceed!")
} else {
  cli_alert_warning("Some files missing - please review and download manually if needed")
  cli_alert_info("See data_sources.Rmd for download instructions")
}
