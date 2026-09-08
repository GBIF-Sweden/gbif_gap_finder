# scripts/00_preflight.R
# ============================================================================
# Preflight — verify every external dependency before the pipeline runs
# ============================================================================
# Purpose:
#   Catch the failure mode that has bitten this project twice: an external
#   dependency changing under us, silently, discovered only when a number moved
#   or a map broke.
#     - 2026-07  GBIF switched its backbone to Catalogue of Life. Tier 4 collapsed
#                3,179 -> 92 and missing-threatened inflated 227 -> 374. Green build.
#     - 2026-09  CARTO began requiring an API key for its raster basemaps. Every
#                map rendered an "API KEY REQUIRED" watermark. Nothing in the repo
#                had changed.
#   Neither was detectable from inside the build, because `targets` protects us
#   from OUR code changing and `renv.lock` from PACKAGES changing, but nothing
#   watched the services those packages talk to. This script does.
#
# Design contract:
#   FAIL  a dependency contract is broken — the pipeline would produce wrong
#         numbers, or none. Gates the build.
#   WARN  worth knowing, does not gate: transient network, cache age, app-side
#         concerns, advisory drift.
#   The distinction matters. A gating check that cries wolf is worse than no
#   check at all, so anything that could be a passing blip is a WARN.
#
# Usage:
#   Rscript scripts/00_preflight.R              # all checks; exit 1 on any FAIL
#   Rscript scripts/00_preflight.R --offline    # local checks only (CI, no network)
#   run_preflight()                             # from run.R
#   tar_make()                                  # runs as the gating `preflight` target
#
# Inputs:  configs/config_{CC}.yml, data/shared/grids/, data/{CC}/raw/, caches
# Outputs: a printed report; invisibly a data.frame of results. stop()s on FAIL.
# ============================================================================

if (!exists("COUNTRY_CODE")) source(here::here("scripts", "00_setup.R"))

# --- mode -------------------------------------------------------------------
.pf_offline <- isTRUE(get0("preflight_offline", ifnotfound = FALSE)) ||
  "--offline" %in% commandArgs(trailingOnly = TRUE) ||
  nzchar(Sys.getenv("PREFLIGHT_OFFLINE"))

.PF_TIMEOUT <- 15  # seconds per network call; a gate must not hang a build

# --- result plumbing --------------------------------------------------------
.pf_results <- list()

pf_add <- function(id, status, detail) {
  .pf_results[[length(.pf_results) + 1L]] <<-
    data.frame(check = id, status = status, detail = detail,
               stringsAsFactors = FALSE)
  switch(status,
    OK   = cli_alert_success("{id}: {detail}"),
    WARN = cli_alert_warning("{id}: {detail}"),
    FAIL = cli_alert_danger("{id}: {detail}")
  )
  invisible(NULL)
}

# Run one check. Any unexpected error inside a check becomes that check's
# result rather than crashing the run — a broken check must not look like a
# broken dependency.
pf_check <- function(id, fn, network = FALSE) {
  if (network && .pf_offline) {
    return(pf_add(id, "OK", "skipped (--offline)"))
  }
  res <- tryCatch(fn(), error = function(e) {
    list(status = "FAIL", detail = paste("check errored:", conditionMessage(e)))
  })
  pf_add(id, res$status, res$detail)
}

ok   <- function(d) list(status = "OK",   detail = d)
warn <- function(d) list(status = "WARN", detail = d)
fail <- function(d) list(status = "FAIL", detail = d)

# GET + parse JSON, NULL on any failure (same contract as 01b's .gbif_get).
.pf_json <- function(url) {
  tryCatch({
    r <- httr::GET(url, httr::timeout(.PF_TIMEOUT))
    if (httr::status_code(r) != 200L) return(NULL)
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                       simplifyVector = TRUE)
  }, error = function(e) NULL)
}

# Is a URL alive, without downloading what is behind it?
#
# HEAD is not universally supported. Dyntaxa's Azure endpoint answers 404 to a
# HEAD request while serving the archive perfectly well over GET — its API
# operation is defined for GET only, so HEAD matches no route. Treating that as
# "unreachable" made this check cry wolf on a healthy endpoint on its very first
# real run.
#
# A ranged GET is not a safe fallback either: that same endpoint ignores
# `Range: bytes=0-0` and answers 200 with the full body, so a naive fallback
# would pull a ~100 MB archive on every tar_make(). Instead, start the GET and
# abort it from the write callback as soon as the first bytes arrive. The status
# line has already been received by then, and curl::handle_data() still reports
# it for the aborted transfer — so we learn whether the endpoint is alive having
# transferred essentially nothing.
.pf_alive <- function(url) {
  st <- tryCatch(httr::status_code(httr::HEAD(url, httr::timeout(.PF_TIMEOUT))),
                 error = function(e) NA_integer_)
  if (!is.na(st) && st < 400L) {
    return(list(alive = TRUE, how = sprintf("HEAD %d", st)))
  }

  h <- curl::new_handle(timeout = .PF_TIMEOUT, range = "0-0", followlocation = TRUE)
  tryCatch(
    curl::curl_fetch_stream(url, handle = h, fun = function(chunk) {
      stop("preflight: enough bytes")  # abort the transfer immediately
    }),
    error = function(e) NULL
  )
  st2 <- tryCatch(curl::handle_data(h)$status_code, error = function(e) NA_integer_)

  if (!is.na(st2) && st2 > 0L && st2 < 400L) {
    return(list(alive = TRUE,
                how = sprintf("GET %d (HEAD unsupported: %s)", st2,
                              if (is.na(st)) "error" else st)))
  }
  list(alive = FALSE,
       how = sprintf("HEAD %s, GET %s",
                     if (is.na(st)) "error" else st,
                     if (is.na(st2)) "error" else st2))
}

GBIF_V1 <- "https://api.gbif.org/v1"
GBIF_V2 <- "https://api.gbif.org/v2"

cli_h1("Preflight — external dependencies")
if (.pf_offline) cli_alert_info("Offline mode: network checks skipped")

# ============================================================================
# Local checks — always run
# ============================================================================

pf_check("config", function() {
  tax_key <- cfg_get("taxonomy.dataset_key", "")
  col_key <- cfg_get("parameters.taxonomic.col_checklist_key", "")
  missing <- c(
    if (!nzchar(tax_key)) "taxonomy.dataset_key",
    if (!nzchar(col_key)) "parameters.taxonomic.col_checklist_key"
  )
  if (length(missing)) return(fail(paste("missing config keys:",
                                         paste(missing, collapse = ", "))))
  ok(sprintf("country %s; COL checklist pinned", COUNTRY_CODE))
})

pf_check("grids", function() {
  if (!dir.exists(raw_grid_dir)) {
    return(fail(sprintf("grid directory not found: %s", raw_grid_dir)))
  }
  want <- c(cfg_get("files.grids.grid10km", "Grid_ETRS89-LAEA_10K.shp"),
            cfg_get("files.grids.grid50km", "EEA_50km_grid_v2024.gpkg"))
  have <- list.files(raw_grid_dir, recursive = TRUE)
  miss <- want[!basename(want) %in% basename(have)]
  if (length(miss)) {
    # WARN, not FAIL: the EEA files are hand-placed and their names vary by
    # download. Script 02 aborts properly if they are genuinely unusable.
    return(warn(sprintf("configured grid file(s) not found by name: %s (%d files present)",
                        paste(miss, collapse = ", "), length(have))))
  }
  ok(sprintf("both reference grids present in %s", basename(raw_grid_dir)))
})

pf_check("taxonomy_file", function() {
  tax <- file.path(raw_taxonomy_dir,
                   cfg_get("files.taxonomy.taxonomy_taxon", "Taxon.csv"))
  if (!file.exists(tax)) {
    return(warn("Taxon.csv not downloaded yet — run 01a"))
  }
  hdr <- tryCatch(
    names(data.table::fread(tax, nrows = 1L, showProgress = FALSE)),
    error = function(e) character()
  )
  hdr <- sub("^﻿", "", hdr)  # Dyntaxa ships a UTF-8 BOM
  need <- c("taxonId", "scientificName", "taxonRank", "taxonomicStatus", "kingdom")
  miss <- setdiff(need, hdr)
  if (length(miss)) {
    return(fail(sprintf("Taxon.csv is missing required column(s): %s — the DwC-A layout changed",
                        paste(miss, collapse = ", "))))
  }
  # The LSID scheme is parsed by an unasserted regex in 03 (extract_numeric_id).
  # A scheme change yields NA ids across the whole backbone, with no error.
  smp <- tryCatch(
    data.table::fread(tax, nrows = 50L, select = "taxonId", showProgress = FALSE)[[1]],
    error = function(e) character()
  )
  if (length(smp) && !any(grepl("^urn:lsid:[^:]+:Taxon:[0-9]+$", smp))) {
    return(fail(sprintf("taxonId no longer matches the expected LSID scheme (e.g. '%s')",
                        smp[1])))
  }
  ok(sprintf("%d required columns present; LSID scheme intact", length(need)))
})

pf_check("caches", function() {
  max_age <- as.numeric(cfg_get("parameters.cache.max_age_days", 90))
  files <- Sys.glob(file.path(p_data_proc, "*cache*.rds"))
  if (!length(files)) return(ok("no caches on disk"))
  notes <- character()
  for (f in files) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "days"))
    if (age > max_age) {
      notes <- c(notes, sprintf("%s is %.0fd old", basename(f), age))
    }
    # Negative caching is how Tier 4 collapsed in July: failed lookups stored as
    # "no result" and never retried. Report the share of empty entries.
    if (grepl("publisher_name_cache", f)) {
      cc <- tryCatch(readRDS(f), error = function(e) NULL)
      if (length(cc)) {
        na_share <- mean(vapply(cc, function(x) is.na(x$title %||% NA), logical(1)))
        if (na_share > 0.05) {
          notes <- c(notes, sprintf("publisher_name_cache: %.0f%% of %d entries are NA (failed lookups cached permanently)",
                                    100 * na_share, length(cc)))
        }
      }
    }
  }
  if (length(notes)) return(warn(paste(notes, collapse = "; ")))
  ok(sprintf("%d cache file(s), all fresh", length(files)))
})

pf_check("marine", function() {
  if (!isTRUE(cfg_get("marine.enabled", FALSE))) return(ok("marine disabled"))
  ez <- cfg_get("marine.eez_file", NULL)
  if (!is.null(ez) && nzchar(ez) && (file.exists(ez) || file.exists(here(ez)))) {
    return(ok("local EEZ file present"))
  }
  cached <- Sys.glob(file.path(p_data_raw, "marine", "*.gpkg"))
  if (length(cached)) return(ok(sprintf("cached marine zone (%s)", basename(cached[1]))))
  if (!requireNamespace("mregions2", quietly = TRUE)) {
    return(fail("marine.enabled but mregions2 is not installed and no local/cached EEZ exists"))
  }
  ok("mregions2 available for EEZ download")
})

# ============================================================================
# Network checks
# ============================================================================

pf_check("gbif_api", function() {
  r <- .pf_json(file.path(GBIF_V1, "dataset", "search?limit=1"))
  if (is.null(r)) return(fail("api.gbif.org is not answering — the pipeline cannot run"))
  ok("api.gbif.org reachable")
}, network = TRUE)

pf_check("col_checklist", function() {
  col_key <- cfg_get("parameters.taxonomic.col_checklist_key",
                     "7ddf754f-d193-4cc9-b351-99906754a03b")
  d <- .pf_json(file.path(GBIF_V1, "dataset", col_key))
  if (is.null(d) || is.null(d$title)) {
    return(fail(sprintf("pinned COL checklist %s not found on GBIF", col_key)))
  }
  # The release behind a stable dataset key is what actually drifts; surface it
  # so a bump is visible in the log rather than only in the numbers.
  ver <- d$pubDate %||% d$modified %||% "unknown"
  ok(sprintf("%s (published %s)", substr(d$title, 1, 60), substr(ver, 1, 10)))
}, network = TRUE)

pf_check("tier4_roundtrip", function() {
  # The exact call chain that 09a Tier 4 depends on, and the exact one that
  # returned HTTP 400 on every key after the July backbone migration.
  col_key <- cfg_get("parameters.taxonomic.col_checklist_key",
                     "7ddf754f-d193-4cc9-b351-99906754a03b")
  probe <- cfg_get("parameters.taxonomic.preflight_taxon_id", "6VFN8")
  s <- .pf_json(sprintf("%s/species?datasetKey=%s&sourceId=%s", GBIF_V1, col_key, probe))
  keys <- if (!is.null(s) && !is.null(s$results) && length(s$results)) s$results$key else NULL
  if (is.null(keys) || !length(keys)) {
    return(fail(sprintf("COL taxonID '%s' no longer resolves to a usage key — Tier 4 is broken", probe)))
  }
  syn <- .pf_json(sprintf("%s/species/%s/synonyms", GBIF_V1, keys[1]))
  n <- if (!is.null(syn) && !is.null(syn$results)) NROW(syn$results) else 0L
  if (n < 1L) {
    return(warn(sprintf("usage key %s resolved but returned no synonyms (Tier 4 will match less)", keys[1])))
  }
  ok(sprintf("taxonID %s -> usage key %s -> %d synonym(s)", probe, keys[1], n))
}, network = TRUE)

pf_check("species_match_v2", function() {
  # v2 is a moving surface; Tier 5 silently rescues nothing if it changes shape.
  col_key <- cfg_get("parameters.taxonomic.col_checklist_key",
                     "7ddf754f-d193-4cc9-b351-99906754a03b")
  nm <- cfg_get("parameters.taxonomic.preflight_taxon_name", "Bellis perennis")
  m <- .pf_json(sprintf("%s/species/match?checklistKey=%s&scientificName=%s",
                        GBIF_V2, col_key, utils::URLencode(nm, reserved = TRUE)))
  key <- if (is.null(m)) NULL else (m$usageKey %||% m$usage$key %||% NULL)
  if (is.null(key) || !length(key) || is.na(key[1])) {
    return(fail(sprintf("/v2/species/match returned no usage key for '%s' — the Tier 5 crosswalk will find nothing", nm)))
  }
  ok(sprintf("'%s' matched (usage key %s)", nm, key[1]))
}, network = TRUE)

pf_check("checklist_archives", function() {
  # Endpoints are resolved from the GBIF registry (globals::resolve_dwca_url),
  # so this checks that the registry still advertises a reachable archive.
  srcs <- c("taxonomy",
            if (isTRUE(cfg_get("redlist.enabled", FALSE)))    "redlist",
            if (isTRUE(cfg_get("invasives.enabled", FALSE)))  "invasives",
            if (isTRUE(cfg_get("sensitive.enabled", FALSE)))  "sensitive")
  bad <- character(); okd <- character()
  for (s in srcs) {
    u <- tryCatch(resolve_dwca_url(cfg_get(paste0(s, ".dataset_key"), ""), s),
                  error = function(e) "")
    if (!nzchar(u)) { bad <- c(bad, paste0(s, " (no URL)")); next }
    res <- .pf_alive(u)
    if (isTRUE(res$alive)) okd <- c(okd, s) else bad <- c(bad, sprintf("%s (%s)", s, res$how))
  }
  if (length(bad)) {
    return(warn(sprintf("archive endpoint unreachable: %s (raw data on disk is still usable)",
                        paste(bad, collapse = ", "))))
  }
  ok(sprintf("%s archive endpoint(s) reachable", paste(okd, collapse = ", ")))
}, network = TRUE)

pf_check("basemap", function() {
  # App-side, so never gating — but this is the check that would have caught the
  # CARTO break before a user did.
  prov <- Sys.getenv("GAP_FINDER_BASEMAP", "Esri.WorldGrayCanvas")
  if (grepl("^CartoDB\\.", prov)) {
    return(warn(sprintf("basemap '%s' uses CARTO raster tiles, which now require an API key and render an 'API KEY REQUIRED' watermark", prov)))
  }
  tiles <- c(
    "Esri.WorldGrayCanvas" = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/6/18/34",
    "OpenStreetMap"        = "https://tile.openstreetmap.org/6/34/18.png"
  )
  if (!prov %in% names(tiles)) return(warn(sprintf("no tile probe defined for basemap '%s'", prov)))
  r <- tryCatch(httr::GET(tiles[[prov]], httr::timeout(.PF_TIMEOUT)),
                error = function(e) NULL)
  if (is.null(r) || httr::status_code(r) != 200L) {
    return(warn(sprintf("basemap '%s' tile request failed (HTTP %s)", prov,
                        if (is.null(r)) "no response" else httr::status_code(r))))
  }
  ct <- httr::headers(r)[["content-type"]] %||% ""
  if (!grepl("^image/", ct)) {
    return(warn(sprintf("basemap '%s' returned %s, not an image — the provider likely changed", prov, ct)))
  }
  ok(sprintf("basemap '%s' serving tiles (%s)", prov, ct))
}, network = TRUE)

pf_check("ci_actions", function() {
  # F12: on 2026-09-08 every action in both workflows still ran on Node 20,
  # 15 days before GitHub removed it from the runners. Nothing was watching.
  wf <- Sys.glob(here(".github", "workflows", "*.yml"))
  if (!length(wf)) return(ok("no workflows"))
  uses <- unique(unlist(lapply(wf, function(f) {
    ln <- grep("uses:", readLines(f, warn = FALSE), value = TRUE)
    trimws(sub(".*uses:\\s*", "", ln))
  })))
  uses <- uses[grepl("^[^/]+/[^@]+@", uses)]
  stale <- character()
  for (u in uses) {
    repo <- sub("@.*", "", u); ref <- sub(".*@", "", u)
    y <- tryCatch({
      r <- httr::GET(sprintf("https://raw.githubusercontent.com/%s/%s/action.yml", repo, ref),
                     httr::timeout(.PF_TIMEOUT))
      if (httr::status_code(r) == 200L) httr::content(r, "text", encoding = "UTF-8") else NA_character_
    }, error = function(e) NA_character_)
    if (!is.na(y) && grepl("using:\\s*['\"]?node20", y)) stale <- c(stale, u)
  }
  if (length(stale)) {
    return(warn(sprintf("action(s) still on Node 20, which GitHub runners no longer support: %s",
                        paste(stale, collapse = ", "))))
  }
  ok(sprintf("%d action(s) on a supported runtime", length(uses)))
}, network = TRUE)

# ============================================================================
# Report
# ============================================================================

pf_report <- do.call(rbind, .pf_results)
n_fail <- sum(pf_report$status == "FAIL")
n_warn <- sum(pf_report$status == "WARN")

cli_h2("Preflight summary")
print(pf_report, row.names = FALSE, right = FALSE)

if (n_fail > 0) {
  cli_alert_danger("{n_fail} dependency check(s) FAILED, {n_warn} warning(s)")
  stop(sprintf(
    "Preflight failed: %s. Fix the dependency, or run tar_make() after setting PREFLIGHT_OFFLINE=1 to build on local data only.",
    paste(pf_report$check[pf_report$status == "FAIL"], collapse = ", ")
  ), call. = FALSE)
}

if (n_warn > 0) {
  cli_alert_warning("Preflight passed with {n_warn} warning(s) — see above")
} else {
  cli_alert_success("Preflight passed: every external dependency behaved as expected")
}

invisible(pf_report)
