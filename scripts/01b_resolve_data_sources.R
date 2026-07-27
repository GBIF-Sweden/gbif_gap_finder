# scripts/01b_resolve_data_sources.R
# ============================================================================
# GBIF Gap Finder — Resolve & Validate Data-Source Citations
# ============================================================================
# Purpose:
#   Build ONE authoritative record of every external data source's citation,
#   derived from identifiers that are hard to get wrong:
#     - GBIF occurrence cubes  -> resolved from their DOWNLOAD KEY via the GBIF
#       download API (DOI, citation string, record count, created date).
#     - National checklists (taxonomy / red list / invasives / sensitive)
#       -> validated from their GBIF DATASET KEY via the dataset API, and the
#       config DOI is cross-checked against GBIF's.
#
#   Everything downstream reads the single object this produces — the eight
#   analysis reports, data/{CC}/data_sources.Rmd, and the Gap Finder app bundle
#   (script 11) — so a DOI can never drift between config, docs and app.
#
#   It also VALIDATES that every key resolves. A dead or test-instance cube key
#   fails HERE, at the start of a run, rather than after the full pipeline or in
#   front of a user.
#
# Inputs:
#   - configs/config_{CC}.yml (cube download keys + checklist dataset keys)
#   - GBIF download & dataset APIs (via jsonlite; no login required)
#
# Outputs:
#   - data/{CC}/proc/data_sources_meta.rds   (also returned invisibly)
#
# Run standalone:  source("scripts/01b_resolve_data_sources.R")
# Dependencies:    globals already loaded by 00_setup.R
#                  (cfg_get, COUNTRY_CODE, p_data_proc, %||%, cli_*).
#                  Uses jsonlite for the GBIF API — no GBIF login required.
# ============================================================================

suppressPackageStartupMessages({
  library(cli)
  library(jsonlite)
})

# ----------------------------------------------------------------------------
# Bootstrap project globals if run standalone
# ----------------------------------------------------------------------------
# Run via tar_make(data_sources_meta) or after source("scripts/00_setup.R"),
# the globals (cfg_get, COUNTRY_CODE, p_data_proc, %||%) already exist. If this
# script is sourced on its own, load them first.
if (!exists("COUNTRY_CODE") || !exists("cfg_get")) {
  if (requireNamespace("here", quietly = TRUE) &&
      file.exists(here::here("scripts", "00_setup.R"))) {
    source(here::here("scripts", "00_setup.R"))
  } else {
    stop("Project globals not loaded \u2014 run source(\"scripts/00_setup.R\") first.")
  }
}

GBIF_API <- "https://api.gbif.org/v1"

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

# GET + parse JSON from a GBIF API endpoint; return NULL on any failure so a
# transient network problem degrades gracefully rather than crashing the run.
.gbif_get <- function(url) {
  tryCatch(
    jsonlite::fromJSON(url, simplifyVector = TRUE),
    error = function(e) {
      cli_alert_warning("Could not reach {.url {url}} ({conditionMessage(e)})")
      NULL
    }
  )
}

# Resolve a GBIF occurrence/cube DOWNLOAD from its key.
resolve_cube <- function(download_key, label) {
  if (is.null(download_key) || !nzchar(download_key)) {
    cli_alert_warning("{label}: no download_key in config — skipping")
    return(list(label = label, download_key = download_key,
                resolves = FALSE, reason = "no download_key in config"))
  }
  meta <- .gbif_get(file.path(GBIF_API, "occurrence", "download", download_key))
  if (is.null(meta) || is.null(meta$doi)) {
    return(list(label = label, download_key = download_key, resolves = FALSE,
                reason = "key not found on GBIF (is it a production gbif.org download?)"))
  }
  doi_url <- paste0("https://doi.org/", meta$doi)
  list(
    label        = label,
    download_key = download_key,
    doi          = doi_url,
    created      = if (!is.null(meta$created)) as.Date(meta$created) else NA,
    records      = meta$totalRecords %||% NA_integer_,
    license      = meta$license %||% NA_character_,
    citation     = sprintf("GBIF Occurrence Download %s accessed via GBIF.org on %s",
                           doi_url, Sys.Date()),
    resolves     = TRUE
  )
}

# Validate a GBIF checklist DATASET from its key; cross-check the config DOI.
resolve_dataset <- function(dataset_key, config_doi, name, label) {
  if (is.null(dataset_key) || !nzchar(dataset_key)) {
    return(list(label = label, name = name, resolves = FALSE,
                reason = "no dataset_key in config"))
  }
  ds <- .gbif_get(file.path(GBIF_API, "dataset", dataset_key))
  if (is.null(ds) || is.null(ds$key)) {
    return(list(label = label, name = name, dataset_key = dataset_key,
                resolves = FALSE, reason = "dataset key not found on GBIF"))
  }
  gbif_doi   <- if (!is.null(ds$doi)) paste0("https://doi.org/", ds$doi) else config_doi
  cfg_bare   <- tolower(sub("https?://doi.org/", "", config_doi %||% ""))
  matches    <- is.null(ds$doi) || !nzchar(cfg_bare) ||
                identical(cfg_bare, tolower(ds$doi))
  list(
    label              = label,
    name               = name %||% ds$title,
    dataset_key        = dataset_key,
    doi                = gbif_doi,
    config_doi         = config_doi,
    doi_matches_config = matches,
    title              = ds$title %||% name,
    citation           = sprintf("%s. Dataset accessed via GBIF.org. %s",
                                 ds$title %||% (name %||% label), gbif_doi),
    resolves           = TRUE
  )
}

# List the datasets that contributed to a cube download (title, DOI, records),
# from the GBIF download .../datasets endpoint (paginated). Returns a data.frame
# sorted by record count, or NULL. Both cube resolutions draw from the same
# occurrences, so the 10 km download's dataset list is representative.
resolve_contributing_datasets <- function(download_key, max_pages = 60L, page = 100L) {
  if (is.null(download_key) || !nzchar(download_key)) return(NULL)
  base <- file.path(GBIF_API, "occurrence", "download", download_key, "datasets")
  acc <- list(); offset <- 0L
  for (i in seq_len(max_pages)) {
    pg  <- .gbif_get(sprintf("%s?limit=%d&offset=%d", base, page, offset))
    res <- pg$results
    if (is.null(res) || (is.data.frame(res) && nrow(res) == 0L)) break
    acc[[length(acc) + 1L]] <- as.data.frame(res, stringsAsFactors = FALSE)
    if (isTRUE(pg$endOfRecords)) break
    offset <- offset + page
  }
  if (!length(acc)) return(NULL)
  raw  <- data.table::rbindlist(acc, fill = TRUE)
  pick <- function(nm) if (nm %in% names(raw)) raw[[nm]] else NA
  out <- data.frame(
    dataset_key = pick("datasetKey"),
    title       = pick("datasetTitle"),
    doi         = {
      d <- pick("datasetDOI")
      ifelse(is.na(d) | d == "", NA_character_, paste0("https://doi.org/", d))
    },
    records     = suppressWarnings(as.numeric(pick("numberRecords"))),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$dataset_key), , drop = FALSE]
  out[order(-out$records), , drop = FALSE]
}

# Count the publishing organisations behind a cube download. Publishers live on
# a separate endpoint from datasets (.../organizations -> OrganizationOccurrence
# DownloadUsage); the paging `count` is exactly the number of contributors.
resolve_publisher_count <- function(download_key) {
  if (is.null(download_key) || !nzchar(download_key)) return(NA_integer_)
  url <- sprintf("%s?limit=1",
                 file.path(GBIF_API, "occurrence", "download", download_key, "organizations"))
  pg <- .gbif_get(url)
  if (is.null(pg) || is.null(pg$count)) return(NA_integer_)
  as.integer(pg$count)
}

# ----------------------------------------------------------------------------
# Resolve every source
# ----------------------------------------------------------------------------

cli_h1("Resolving & validating data-source citations ({COUNTRY_CODE})")

cubes <- list(
  grid10km = resolve_cube(cube_download_key("grid10km"), "GBIF cube \u2014 10 km"),
  grid50km = resolve_cube(cube_download_key("grid50km"), "GBIF cube \u2014 50 km")
)

checklists <- list()
checklists$taxonomy <- resolve_dataset(
  cfg_get("taxonomy.dataset_key"), cfg_get("taxonomy.doi"),
  cfg_get("taxonomy.name", "National taxonomy"), "Taxonomy backbone"
)
for (src in c("redlist", "invasives", "sensitive")) {
  if (isTRUE(cfg_get(paste0(src, ".enabled"), FALSE))) {
    checklists[[src]] <- resolve_dataset(
      cfg_get(paste0(src, ".dataset_key")),
      cfg_get(paste0(src, ".doi")),
      cfg_get(paste0(src, ".name"), src),
      tools::toTitleCase(src)
    )
  }
}

cli_alert_info("Fetching contributing datasets from the 10 km cube download \u2026")
contributing <- resolve_contributing_datasets(cube_download_key("grid10km"))
n_contrib    <- if (is.null(contributing)) 0L else nrow(contributing)
n_publishers <- resolve_publisher_count(cube_download_key("grid10km"))
cli_alert_success("Contributing datasets: {n_contrib} from {n_publishers %||% '?'} publisher(s)")

data_sources_meta <- list(
  resolved_at = Sys.time(),
  country     = list(code = cfg_get("country.code", COUNTRY_CODE),
                     name = cfg_get("country.name", COUNTRY_CODE)),
  cubes       = cubes,
  checklists  = checklists,
  contributing_datasets   = contributing,
  n_contributing_datasets = n_contrib,
  n_publishers            = n_publishers
)

# ----------------------------------------------------------------------------
# Validation report
# ----------------------------------------------------------------------------

all_items <- c(cubes, checklists)
report <- data.frame(
  Source   = vapply(all_items, function(x) x$label %||% x$name %||% "?", character(1)),
  Resolves = vapply(all_items, function(x) isTRUE(x$resolves), logical(1)),
  Detail   = vapply(all_items, function(x) {
    if (isTRUE(x$resolves)) x$doi %||% "ok" else (x$reason %||% "failed")
  }, character(1)),
  row.names = NULL, stringsAsFactors = FALSE
)
print(report)

# Surface any config-vs-GBIF DOI drift among the checklists.
for (m in checklists) {
  if (isFALSE(m$doi_matches_config %||% TRUE)) {
    cli_alert_warning(
      "{m$label}: config DOI ({m$config_doi}) does not match GBIF's ({m$doi}) \\
       \u2014 update config.")
  }
}

# ----------------------------------------------------------------------------
# Save + fail loudly on unresolved cubes
# ----------------------------------------------------------------------------

out_path <- file.path(p_data_proc, "data_sources_meta.rds")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(data_sources_meta, out_path)
cli_alert_success("Saved {.path {out_path}}")

bad_cubes <- Filter(function(x) !isTRUE(x$resolves), cubes)
if (length(bad_cubes)) {
  cli_alert_danger("{length(bad_cubes)} cube download key(s) did not resolve on GBIF.")
  cli_alert_info(
    "Check the download_key in configs/config_{COUNTRY_CODE}.yml is a \\
     *production* gbif.org download.")
  stop("Data-source validation failed: unresolved GBIF cube download key(s).")
}

cli_alert_success("All data-source citations resolved and validated.")
invisible(data_sources_meta)