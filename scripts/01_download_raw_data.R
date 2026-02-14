# scripts/01_download_raw_data.R
# ============================================================================
# Automated Download of Raw Data Sources
# ============================================================================
# Purpose:
#   Download all external datasets needed for the gap analysis:
#     1. EEA reference grids (10km and 50km)
#     2. National red list taxonomy (via GBIF)  [optional]
#     3. National taxonomy backbone (via GBIF)  [primary]
#     4. GBIF occurrence cubes (filtered by country, split by basisOfRecord)
#
# Inputs:  config.yml (paths, DOIs, dataset keys)
# Outputs: Raw files in data_raw/ sub-directories
#          logs/download_log_<date>.txt
#          data_raw/download_metadata.json
#
# When to run:
#   - Initial setup (after 00_setup.R)
#   - When updating to new reference dataset versions
#   - When GBIF occurrence data needs refreshing
#
# Note: All downloads go to data_raw/ (never manually edited).
# ============================================================================

library(here)
library(dplyr)
library(readr)
library(cli)
library(httr)
library(jsonlite)

if (!requireNamespace("rgbif", quietly = TRUE)) {
  cli_alert_warning(
    "Package {.pkg rgbif} not installed. Installing now..."
  )
  install.packages("rgbif")
}
library(rgbif)

source(here("scripts", "00_setup.R"))

# ============================================================================
# Country & Dataset Configuration (from config.yml)
# ============================================================================

country_code <- cfg_get("country.gbif_country_code",
                        cfg_get("country.code", "SE"))
country_name <- cfg_get("country.name", "Country")

cli_h1("Download Raw Data \u2014 {country_name} ({country_code})")

# Directories
dir_data_raw   <- here(cfg_get("paths.data_raw", "data_raw"))
dir_grids_10km <- here(raw_grid_10km_dir)
dir_grids_50km <- here(raw_grid_50km_dir)
dir_redlist    <- here(raw_redlist_dir)
dir_taxonomy   <- here(raw_taxonomy_dir)
dir_cubes      <- here(raw_gbif_cube_dir)
dir_logs       <- here(cfg_get("paths.logs", "logs"))

for (d in c(dir_grids_10km, dir_grids_50km, dir_redlist,
            dir_taxonomy, dir_cubes, dir_logs)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ============================================================================
# GBIF Credentials
# ============================================================================
# Set in .Renviron:
#   GBIF_USER=your_username
#   GBIF_EMAIL=your_email@example.com
#   GBIF_PWD=your_password

if (Sys.getenv("GBIF_USER") != "") {
  options(
    gbif_user  = Sys.getenv("GBIF_USER"),
    gbif_email = Sys.getenv("GBIF_EMAIL"),
    gbif_pwd   = Sys.getenv("GBIF_PWD")
  )
  cli_alert_success("GBIF credentials loaded from environment")
} else {
  cli_alert_warning(
    "GBIF credentials not found in environment variables"
  )
  cli_alert_info(
    "Set GBIF_USER, GBIF_EMAIL, GBIF_PWD in .Renviron"
  )
}

# ============================================================================
# Logging
# ============================================================================

log_file <- here(
  dir_logs,
  paste0("download_log_", Sys.Date(), ".txt")
)

log_download <- function(msg) {
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  out <- paste0("[", ts, "] ", msg)
  cat(out, "\n", file = log_file, append = TRUE)
  cli_alert_info(msg)
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Download a file with error handling
#'
#' @param url   Source URL
#' @param destfile Local destination path
#' @param method Download method (default "auto")
#' @return TRUE on success, FALSE on failure
download_file_safely <- function(url, destfile, method = "auto") {
  cli_alert_info("Downloading: {.url {url}}")
  cli_alert_info("Destination: {.path {destfile}}")

  tryCatch({
    download.file(
      url, destfile,
      method = method, mode = "wb", quiet = FALSE
    )
    log_download(paste("Downloaded:", basename(destfile)))
    TRUE
  }, error = function(e) {
    cli_alert_danger("Download failed: {e$message}")
    log_download(paste("ERROR:", e$message))
    FALSE
  })
}

#' Extract a zip archive with error handling
#'
#' @param zipfile Path to .zip file
#' @param exdir  Extraction directory
#' @return TRUE on success, FALSE on failure
unzip_safely <- function(zipfile, exdir) {
  cli_alert_info("Unzipping: {.path {basename(zipfile)}}")

  tryCatch({
    unzip(zipfile, exdir = exdir)
    log_download(paste("Extracted:", basename(zipfile)))
    TRUE
  }, error = function(e) {
    cli_alert_danger("Extraction failed: {e$message}")
    log_download(paste("ERROR:", e$message))
    FALSE
  })
}

#' Download and extract a GBIF DwC-A dataset
#'
#' @param dataset_key  GBIF dataset UUID
#' @param export_url   Direct DwC-A download URL (if known)
#' @param dest_dir     Directory for extracted files
#' @param label        Human-readable dataset name (for messages)
#' @param expected_files Character vector of files to verify
#' @return Invisible TRUE/FALSE
download_gbif_dataset <- function(dataset_key,
                                  export_url,
                                  dest_dir,
                                  label,
                                  expected_files = NULL) {
  cli_h2("Downloading {label}")

  # Retrieve metadata
 dataset_info <- tryCatch(
    rgbif::datasets(uuid = dataset_key),
    error = function(e) {
      cli_alert_danger(
        "Could not retrieve metadata: {e$message}"
      )
      NULL
    }
  )

  if (!is.null(dataset_info)) {
    cli_alert_success(
      "Dataset: {dataset_info$data$title}"
    )
    cli_alert_info(
      "Version: {dataset_info$data$version}"
    )
  }

  # Download archive
  zip_path <- file.path(dest_dir, paste0(label, "_dwca.zip"))
  ok <- download_file_safely(export_url, zip_path)

  if (ok && file.exists(zip_path)) {
    unzip_safely(zip_path, dest_dir)
    cli_alert_success("{label} downloaded and extracted")

    # Verify expected files
    if (!is.null(expected_files)) {
      found <- list.files(dest_dir)
      for (f in expected_files) {
        if (f %in% found) {
          cli_alert_success("Found: {f}")
        } else {
          cli_alert_warning("Missing expected file: {f}")
        }
      }
    }
    return(invisible(TRUE))
  }

  cli_alert_warning(
    "Automated download failed for {label}."
  )
  cli_alert_info(
    "Manual download: https://www.gbif.org/dataset/{dataset_key}"
  )
  invisible(FALSE)
}

# ============================================================================
# 1. EEA Reference Grids
# ============================================================================

cli_h1("1 \u2014 EEA Reference Grids")

# 10km grid
cli_h2("EEA 10km Grid")

eea_10km_url <- cfg_get(
  "downloads.grid_10km_url",
  ""
)
eea_10km_zip <- here(dir_grids_10km, "grid_10km.zip")

cli_alert_info(
  "Note: EEA grid downloads may require manual intervention."
)
cli_alert_info(
 "If automated download fails, see EEA GeoNetwork."
)

cat("\nAttempt automated download of 10km grid? (y/n): ")
if (interactive() && tolower(readline()) == "y") {
  download_file_safely(eea_10km_url, eea_10km_zip)
  if (file.exists(eea_10km_zip)) {
    unzip_safely(eea_10km_zip, dir_grids_10km)
    file.remove(eea_10km_zip)
  }
} else {
  cli_alert_info("Skipping \u2014 add files manually if needed")
}

# 50km grid
cli_h2("EEA 50km Grid")

eea_50km_url <- cfg_get(
  "downloads.grid_50km_url",
  "https://sdi.eea.europa.eu/datashare/s/EEA_50km_Grid_v2024"
)
eea_50km_file <- here(
  dir_grids_50km,
  cfg_get("files.grids.grid50km", "EEA_50km_grid_v2024.gpkg")
)

cat("\nAttempt automated download of 50km grid? (y/n): ")
if (interactive() && tolower(readline()) == "y") {
  download_file_safely(eea_50km_url, eea_50km_file)
} else {
  cli_alert_info("Skipping \u2014 add files manually if needed")
}

# ============================================================================
# 2. National Red List (optional, for threat status)
# ============================================================================

cli_h1("2 \u2014 National Red List (optional reference)")

redlist_dataset_key <- cfg_get(
  "redlist.dataset_key",
  "fab88965-e69d-4491-a04d-e3198b626e52"
)
redlist_doi <- cfg_get(
  "redlist.doi",
  "https://doi.org/10.15468/jhwkpq"
)
redlist_export_url <- cfg_get(
  "redlist.export_url",
  ""
)

cli_alert_info("DOI: {redlist_doi}")

download_gbif_dataset(
  dataset_key    = redlist_dataset_key,
  export_url     = redlist_export_url,
  dest_dir       = dir_redlist,
  label          = "national_redlist",
  expected_files = c("taxon.txt", "distribution.txt")
)

# ============================================================================
# 3. National Taxonomy Backbone (primary)
# ============================================================================

cli_h1("3 \u2014 National Taxonomy Backbone (primary)")

taxonomy_dataset_key <- cfg_get(
  "taxonomy.dataset_key",
  "de8934f4-a136-481c-a87a-b0b202b80a31"
)
taxonomy_doi <- cfg_get(
  "taxonomy.doi",
  "https://doi.org/10.15468/j43wfc"
)
taxonomy_export_url <- cfg_get(
  "taxonomy.export_url",
  ""
)

cli_alert_info("DOI: {taxonomy_doi}")

download_gbif_dataset(
  dataset_key    = taxonomy_dataset_key,
  export_url     = taxonomy_export_url,
  dest_dir       = dir_taxonomy,
  label          = "national_taxonomy",
  expected_files = c("Taxon.csv", "SpeciesDistribution.csv")
)

# ============================================================================
# 4. GBIF Occurrence Cubes
# ============================================================================

cli_h1("4 \u2014 GBIF Occurrence Cubes")

cli_alert_info(
 "Downloading pre-computed cubes for {country_name}"
)
cli_alert_info(
  "Split by basisOfRecord at 10km and 50km resolution"
)
cli_alert_warning(
  "Downloads can be large (hundreds of MB)"
)

basis_types <- c(
  "occurrence", "observation", "humanObservation",
  "machineObservation", "preservedSpecimen",
  "fossilSpecimen", "livingSpecimen",
  "materialSample", "materialCitation"
)

grid_resolutions <- c("10km", "50km")

cli_alert_info(
  "{length(basis_types)} basis types \u00d7 ",
  "{length(grid_resolutions)} grids = ",
  "{length(basis_types) * length(grid_resolutions)} files"
)

cat("\nProceed with cube downloads? (y/n): ")
if (!interactive() || tolower(readline()) == "y") {

  cli_h2("GBIF Occurrence Cube Downloads")

  gbif_user  <- getOption("gbif_user")
  gbif_email <- getOption("gbif_email")
  gbif_pwd   <- getOption("gbif_pwd")

  if (is.null(gbif_user) || is.null(gbif_pwd)) {
    cli_alert_danger("GBIF credentials not set!")
    cli_alert_info(
      "Set credentials and re-run, or download manually."
    )
  } else {
    cli_alert_success("GBIF credentials found")

    cli_h2("Download Instructions")
    cli_alert_info("Occurrence cubes are specialised downloads.")

    cube_dois <- cfg_get("cubes.resolutions", list())
    for (cube in cube_dois) {
      cli_alert_info(
        "  {cube$resolution}km: {cube$doi}"
      )
    }

    cli_alert_info("")
    cli_alert_info("Option 1: Download from DOIs above")
    cli_alert_info(
      "Option 2: New cube via GBIF web interface"
    )
    cli_alert_info(
      "Option 3: Programmatic (see rgbif vignette)"
    )
  }
} else {
  cli_alert_info("Skipping cube downloads")
  cli_alert_info("Using existing files in {.path {dir_cubes}}")
}

# ============================================================================
# Summary & Metadata
# ============================================================================

cli_h1("Download Summary")

metadata <- list(
  country_code       = country_code,
  country_name       = country_name,
  download_date      = as.character(Sys.Date()),
  download_timestamp = as.character(Sys.time()),
  r_version          = paste(
    R.version$major, R.version$minor, sep = "."
  ),
  rgbif_version = as.character(packageVersion("rgbif")),

  grids = list(
    grid_10km_dir   = dir_grids_10km,
    grid_10km_files = list.files(dir_grids_10km),
    grid_50km_dir   = dir_grids_50km,
    grid_50km_files = list.files(dir_grids_50km)
  ),

  redlist = list(
    dir         = dir_redlist,
    files       = list.files(dir_redlist),
    dataset_key = redlist_dataset_key,
    doi         = redlist_doi
  ),

  taxonomy = list(
    dir         = dir_taxonomy,
    files       = list.files(dir_taxonomy),
    dataset_key = taxonomy_dataset_key,
    doi         = taxonomy_doi
  ),

  cubes = list(
    dir   = dir_cubes,
    files = list.files(dir_cubes, pattern = "\\.csv$")
  )
)

metadata_file <- here(dir_data_raw, "download_metadata.json")
write_json(metadata, metadata_file, pretty = TRUE, auto_unbox = TRUE)

cli_alert_success(
  "Metadata saved: {.path {metadata_file}}"
)
cli_alert_success("Log saved: {.path {log_file}}")

# Files summary
cli_h2("Files Summary")

summary_df <- tibble::tribble(
  ~Dataset,             ~Location,       ~Files,
  "Grid 10km",          dir_grids_10km,
    length(list.files(dir_grids_10km)),
  "Grid 50km",          dir_grids_50km,
    length(list.files(dir_grids_50km)),
  "National Red List",  dir_redlist,
    length(list.files(dir_redlist)),
  "National Taxonomy",  dir_taxonomy,
    length(list.files(dir_taxonomy)),
  "GBIF Cubes",         dir_cubes,
    length(list.files(dir_cubes, pattern = "\\.csv$"))
)

print(summary_df)

# ============================================================================
# Verification Checks
# ============================================================================

cli_h1("Verification Checks")

# Build critical-file list from config
grid_10km_file <- cfg_get(
  "files.grids.grid10km", "se_10km.shp"
)
grid_50km_file <- cfg_get(
  "files.grids.grid50km", "EEA_50km_grid_v2024.gpkg"
)
redlist_taxon_file <- cfg_get(
  "files.redlist.redlist_taxon",
  cfg_get("files.redlist.redlist_taxon", "taxon.txt")
)
redlist_distr_file <- cfg_get(
  "files.redlist.redlist_distr",
  cfg_get("files.redlist.redlist_distr", "distribution.txt")
)
taxonomy_taxon_file <- cfg_get(
  "files.taxonomy.taxonomy_taxon",
  cfg_get("files.taxonomy.taxonomy_taxon", "Taxon.csv")
)
taxonomy_distr_file <- cfg_get(
  "files.taxonomy.taxonomy_distr",
  cfg_get("files.taxonomy.taxonomy_distr", "SpeciesDistribution.csv")
)

critical_files <- list(
  "Grid 10km"            = file.path(
    dir_grids_10km, grid_10km_file
  ),
  "Grid 50km"            = file.path(
    dir_grids_50km, grid_50km_file
  ),
  "Red list taxonomy"    = file.path(
    dir_redlist, redlist_taxon_file
  ),
  "Red list distribution" = file.path(
    dir_redlist, redlist_distr_file
  ),
  "Taxonomy backbone"    = file.path(
    dir_taxonomy, taxonomy_taxon_file
  ),
  "Taxonomy distribution" = file.path(
    dir_taxonomy, taxonomy_distr_file
  )
)

all_ok <- TRUE
for (label in names(critical_files)) {
  path <- critical_files[[label]]
  if (file.exists(path)) {
    cli_alert_success("{label}: Found")
  } else {
    cli_alert_warning("{label}: Missing \u2014 {.path {path}}")
    all_ok <- FALSE
  }
}

cube_files <- list.files(
  dir_cubes, pattern = "\\.csv$", full.names = FALSE
)
if (length(cube_files) > 0) {
  cli_alert_success(
    "GBIF cubes: {length(cube_files)} CSV files found"
  )
} else {
  cli_alert_warning(
    "GBIF cubes: No CSV files \u2014 may need manual download"
  )
  all_ok <- FALSE
}

if (all_ok) {
  cli_alert_success("All critical files present!")
} else {
  cli_alert_warning(
    "Some files missing \u2014 check logs for details"
  )
}

cli_alert_success("Download script complete!")
cli_alert_info("Next: source('scripts/02_ingest_grids.R')")
