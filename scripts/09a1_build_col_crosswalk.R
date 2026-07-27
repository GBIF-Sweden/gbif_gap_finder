# scripts/09a1_build_col_crosswalk.R
# ============================================================================
# Build + validate the authoritative Dyntaxa <-> COL crosswalk (Section B spike)
# ============================================================================
# WHY
#   GBIF permanently switched its default backbone to the Catalogue of Life
#   Extended Release (COL XR). The occurrence cube's `specieskey` is a COL
#   taxonID (alphanumeric, e.g. "DGND"). This builds the modern reconciliation
#   authority: match every national-taxonomy (Dyntaxa) name to COL via GBIF's v2
#   match API (checklistKey = COL) and record the COL key it resolves to.
#
#   Verified against the live API (2026-07-27): the v2 match `usage.key` — or
#   `acceptedUsage.key` when the Dyntaxa name is a COL synonym — IS the same
#   alphanumeric id the cube stores in `specieskey`
#     Anas crecca      -> usage.key       "DGND"  (in cube)
#     Sylvia communis  -> acceptedUsage.key "DDBNS" (Curruca communis; in cube)
#   So a Dyntaxa taxon is "present in GBIF" iff its COL key is in the cube's
#   specieskey set — no integer-usageKey hop, and COL-name divergence
#   (Sylvia->Curruca) is resolved in one step.
#
# WHAT THIS DOES  (it does NOT modify 09a — it is the validated input for the
#                  09a tier-replacement, delivered as the next patch)
#   1. Loads Dyntaxa (03) + the cube species universe (06b species_summary).
#   2. Matches each unique Dyntaxa name -> COL key (cached + resumable). Fast
#      bulk path via rgbif::name_backbone_checklist(); serial httr2 fallback with
#      the exact verified /v2/species/match shape.
#   3. Writes the crosswalk  data/{CC}/proc/col_crosswalk.rds
#      (dyntaxa_taxonID, col_key, match_status, match_type, confidence, name_used).
#   4. VALIDATES coverage vs the cube and the confirmed baseline
#      (matched 76.4% / occ 99.72% / missing-threatened 225) and writes
#      data/{CC}/proc/gaps/col_crosswalk_validation.md so the full 09a swap can
#      be green-lit on real numbers first.
#
# RUN:  source("scripts/09a1_build_col_crosswalk.R")     (after 03 + 06b)
# Config (all optional; safe defaults, NO config-file change required):
#   parameters.taxonomic.col_checklist_key            (pinned by the provenance patch)
#   parameters.taxonomic.crosswalk_include_synonyms   default TRUE
#   parameters.taxonomic.crosswalk_min_fuzzy_confidence default 90
#   parameters.taxonomic.crosswalk_use_bulk_rgbif     default TRUE (fast); FALSE = serial httr2
#   parameters.taxonomic.api_max_requests / api_max_batches  (serial batch caps; as 09a)
# Dependencies: data.table, stringr, here, cli; rgbif (bulk) and/or httr2 (serial).
# ============================================================================

source(here::here("scripts", "00_setup.R"))

timer_start <- Sys.time()
cli_h1("09a1 -- Build + validate Dyntaxa<->COL crosswalk (Section B spike)")

# ----------------------------------------------------------------------------
# Config (defaults are safe; no config file change required)
# ----------------------------------------------------------------------------
col_checklist_key <- cfg_get("parameters.taxonomic.col_checklist_key",
                             "7ddf754f-d193-4cc9-b351-99906754a03b")
include_synonyms  <- isTRUE(cfg_get("parameters.taxonomic.crosswalk_include_synonyms", TRUE))
min_fuzzy_conf    <- cfg_get("parameters.taxonomic.crosswalk_min_fuzzy_confidence", 90)
use_bulk          <- isTRUE(cfg_get("parameters.taxonomic.crosswalk_use_bulk_rgbif", FALSE))  # default off: the parallel httr2 path below is fast + rgbif-version-independent
api_rate_limit    <- 10
api_max_requests  <- cfg_get("parameters.taxonomic.api_max_requests", 5000)
api_max_batches   <- cfg_get("parameters.taxonomic.api_max_batches", Inf)

cache_file     <- here(p_data_proc, "col_crosswalk_cache.rds")
crosswalk_file <- here(p_data_proc, "col_crosswalk.rds")
report_file    <- here(p_gaps, "col_crosswalk_validation.md")
THREATENED_CODES <- c("CR", "EN", "VU", "NT")

# Confirmed regression baseline (both repair patches, 2026-07-27 rerun).
BASE_MATCH_PCT <- 76.4
BASE_OCC_PCT   <- 99.72
BASE_MISSING_THREATENED <- 225

# ============================================================================
# Step 1: Load Dyntaxa reference + classify accepted/synonym
# ============================================================================
cli_h2("Loading Dyntaxa reference")
taxa_file <- here(p_data_proc, "taxa_reference_current.rds")
if (!file.exists(taxa_file)) {
  cli_abort("Taxonomy reference not found: {.path {taxa_file}}. Run script 03 first.")
}
taxa <- as.data.table(readRDS(taxa_file))
taxa[, name_std := tolower(trimws(scientificName))]
taxa <- classify_accepted(taxa)
taxa <- resolve_threat_status(taxa,
  c("threatStatus_redlist", "threatStatus_backbone", "threatStatus"))

accepted <- taxa[is_accepted == TRUE]
cli_alert_info(
  "Dyntaxa accepted: {scales::comma(nrow(accepted))}  \\
   synonyms: {scales::comma(sum(!taxa$is_accepted))}"
)

# Map every candidate name -> its accepted Dyntaxa taxonID.
# Accepted rows map to themselves; synonym rows map to acceptedNameUsageID, so a
# COL match found via a Dyntaxa synonym still credits the accepted taxon.
has_syn_col <- "acceptedNameUsageID" %in% names(taxa)
name_map <- rbindlist(list(
  accepted[, .(name_std, accepted_taxonID = as.character(taxonID), is_syn = FALSE)],
  if (include_synonyms && has_syn_col) {
    taxa[is_accepted == FALSE &
           !is.na(acceptedNameUsageID) & acceptedNameUsageID != "",
         .(name_std, accepted_taxonID = as.character(acceptedNameUsageID), is_syn = TRUE)]
  } else NULL
), use.names = TRUE, fill = TRUE)
name_map <- name_map[!is.na(name_std) & name_std != ""]

# Threat status + rank per accepted taxon (for prioritised querying + validation)
acc_sel  <- c("taxonID", "threatStatus", if ("taxonRank" %in% names(accepted)) "taxonRank")
acc_info <- accepted[, ..acc_sel]
setnames(acc_info, "taxonID", "accepted_taxonID")
acc_info[, accepted_taxonID := as.character(accepted_taxonID)]
acc_info <- acc_info[, .SD[1], by = accepted_taxonID]
name_map <- merge(name_map, acc_info, by = "accepted_taxonID", all.x = TRUE)

# Unique names to query, threatened-accepted first so a capped/partial run still
# validates the "missing threatened" metric, then accepted before synonyms.
cand <- unique(
  name_map[, .(name_std,
               is_threat = threatStatus %in% THREATENED_CODES,
               is_syn)],
  by = "name_std"
)
setorder(cand, -is_threat, is_syn)
cli_alert_info(
  "Unique Dyntaxa names to match: {scales::comma(nrow(cand))} \\
   ({scales::comma(sum(cand$is_threat))} threatened)"
)

# ============================================================================
# Step 2: Load the cube species universe (COL keys actually observed)
# ============================================================================
cli_h2("Loading cube species universe (for validation)")
sum_files <- list.files(
  here(p_data_proc, "derived"),
  pattern = "species_summary.*10km\\.csv$", recursive = TRUE, full.names = TRUE
)
cube_sp <- NULL
if (length(sum_files) == 0) {
  cli_alert_warning(
    "No species_summary files (run 06b) -- crosswalk will build but cube \\
     validation is skipped."
  )
} else {
  gbif_raw <- rbindlist(lapply(sum_files, fread,
    select = c("specieskey", "occurrences", "basisofrecord")), fill = TRUE)
  gbif_raw[, specieskey := as.character(specieskey)]
  cube_sp <- gbif_raw[basisofrecord == "all",
    .(total_occ = sum(as.numeric(occurrences), na.rm = TRUE)), by = specieskey]
  cli_alert_success(
    "Cube species: {scales::comma(nrow(cube_sp))}  \\
     occ: {scales::comma(sum(cube_sp$total_occ))}"
  )
}
cube_keys <- if (!is.null(cube_sp)) unique(cube_sp$specieskey) else character(0)

# ============================================================================
# Step 3: Match Dyntaxa names -> COL keys (cached, resumable)
# ============================================================================
cli_h2("Matching Dyntaxa names against COL (v2 match API)")

# Cache: name_std -> list(col_key, status, mtype, conf, col_name). Empty/NONE
# results ARE cached (valid answer). Transport errors are NOT cached, so they
# retry on the next run rather than becoming permanent negatives.
api_cache <- if (file.exists(cache_file)) readRDS(cache_file) else list()
cli_alert_info("Cache entries: {scales::comma(length(api_cache))}")

# Normalise one v2 match response object into a flat record.
extract_match <- function(usage, acc, diag) {
  usage <- usage %||% list(); acc <- acc %||% list(); diag <- diag %||% list()
  list(
    col_key  = acc$key %||% usage$key %||% NA_character_,
    status   = usage$status %||% NA_character_,
    mtype    = diag$matchType %||% "NONE",
    conf     = diag$confidence %||% NA_integer_,
    col_name = acc$canonicalName %||% usage$canonicalName %||% NA_character_
  )
}

# --- Fast path: rgbif::name_backbone_checklist (parallel bulk) ---------------
# Populates the cache for as many names as it can; the serial path mops up the
# rest. Column names vary by rgbif version, so extract defensively and only
# trust a clean one-row-per-input result (else defer everything to serial).
bulk_fill_cache <- function(names_vec) {
  if (!requireNamespace("rgbif", quietly = TRUE)) {
    cli_alert_warning("rgbif not installed -- skipping bulk path")
    return(invisible(FALSE))
  }
  res <- tryCatch(
    rgbif::name_backbone_checklist(
      name_data    = data.frame(name = names_vec, stringsAsFactors = FALSE),
      checklistKey = col_checklist_key
    ),
    error = function(e) {
      cli_alert_warning("name_backbone_checklist failed ({conditionMessage(e)}) -- using serial path")
      NULL
    }
  )
  if (is.null(res)) return(invisible(FALSE))
  res <- as.data.frame(res)
  if (nrow(res) != length(names_vec)) {
    cli_alert_warning(
      "Bulk match returned {nrow(res)} rows for {length(names_vec)} names \\
       (alternatives?) -- deferring to serial path")
    return(invisible(FALSE))
  }
  getcol <- function(nm) if (nm %in% names(res)) res[[nm]] else NA
  key_col <- if ("usageKey" %in% names(res)) "usageKey" else
             if ("usagekey" %in% names(res)) "usagekey" else NA_character_
  if (is.na(key_col)) {
    cli_alert_warning("Bulk result has no usageKey column -- deferring to serial path")
    return(invisible(FALSE))
  }
  usage_key <- as.character(res[[key_col]])
  acc_key   <- as.character(getcol("acceptedUsageKey"))
  col_key   <- ifelse(!is.na(acc_key) & nzchar(acc_key), acc_key, usage_key)
  status    <- as.character(getcol("status"))
  mtype     <- as.character(getcol("matchType")); mtype[is.na(mtype)] <- "NONE"
  conf      <- suppressWarnings(as.integer(getcol("confidence")))
  col_name  <- as.character(getcol("canonicalName"))
  for (i in seq_along(names_vec)) {
    ck <- col_key[i]
    api_cache[[names_vec[i]]] <<- list(
      col_key  = if (!is.na(ck) && nzchar(ck)) ck else NA_character_,
      status   = status[i], mtype = mtype[i], conf = conf[i], col_name = col_name[i]
    )
  }
  cli_alert_success("Bulk matched {scales::comma(length(names_vec))} names via name_backbone_checklist")
  invisible(TRUE)
}

to_query <- cand$name_std[!(cand$name_std %in% names(api_cache))]
if (use_bulk && length(to_query) > 0) {
  ok <- bulk_fill_cache(to_query)
  if (isTRUE(ok)) saveRDS(api_cache, cache_file)
  to_query <- cand$name_std[!(cand$name_std %in% names(api_cache))]
}

# --- Parallel matcher: httr2 /v2/species/match (verified response shape) -----
# Concurrency-capped parallel requests. GBIF's own name_backbone_checklist uses
# bucket_size 300, so a cap of ~20 is very conservative and ~10-20x the old
# per-name serial loop. Resumes from cache; transport errors are NOT cached, so
# they retry on the next run. Tunable: parameters.taxonomic.crosswalk_parallel_conc.
if (length(to_query) > 0) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli_alert_warning(
      "{length(to_query)} names unresolved and httr2 not installed -- \\
       install.packages('httr2') and rerun (cache persists).")
  } else {
    library(httr2)
    max_conc  <- cfg_get("parameters.taxonomic.crosswalk_parallel_conc", 20)
    chunk_sz  <- cfg_get("parameters.taxonomic.crosswalk_parallel_chunk", 500)
    max_names <- cfg_get("parameters.taxonomic.crosswalk_max_names_per_run", Inf)
    to_run <- if (is.finite(max_names)) utils::head(to_query, max_names) else to_query

    # Version-robust concurrency: newer httr2 takes `max_active`, older a `pool`.
    perform_parallel <- function(reqs) {
      fmls <- names(formals(httr2::req_perform_parallel))
      args <- list(reqs, on_error = "continue")
      if ("max_active" %in% fmls) {
        args$max_active <- max_conc
      } else if (requireNamespace("curl", quietly = TRUE)) {
        args$pool <- curl::new_pool(total_con = max_conc, host_con = max_conc)
      }
      do.call(httr2::req_perform_parallel, args)
    }

    n_run   <- length(to_run)
    est_min <- max(1, round(n_run / max_conc / 5))   # ~5 resolved/sec per active slot
    cli_alert_info(
      "Parallel match: {scales::comma(n_run)} names, up to {max_conc} concurrent \\
       (~{est_min} min). Cache resumes across runs."
    )
    total_err <- 0L
    pb <- cli_progress_bar("COL match (parallel)", total = n_run)
    i0 <- 0L
    while (i0 < n_run) {
      idx  <- seq.int(i0 + 1L, min(i0 + chunk_sz, n_run))
      nms  <- to_run[idx]
      reqs <- lapply(nms, function(nm) {
        request("https://api.gbif.org/v2/species/match") |>
          req_url_query(checklistKey = col_checklist_key, scientificName = nm) |>
          req_retry(max_tries = 3, backoff = ~ 2) |>
          req_error(is_error = function(resp) FALSE)   # keep 4xx as a response
      })
      resps <- tryCatch(perform_parallel(reqs), error = function(e) {
        cli_alert_warning("Parallel chunk failed ({conditionMessage(e)}) -- retrying serially")
        lapply(reqs, function(rq) tryCatch(req_perform(rq), error = function(e) e))
      })
      for (k in seq_along(nms)) {
        r <- resps[[k]]
        if (inherits(r, "httr2_response")) {
          j <- tryCatch(resp_body_json(r), error = function(e) NULL)
          if (!is.null(j)) {
            api_cache[[nms[k]]] <- extract_match(j$usage, j$acceptedUsage, j$diagnostics)
          } else total_err <- total_err + 1L
        } else {
          total_err <- total_err + 1L   # NOT cached -> retried next run
        }
      }
      saveRDS(api_cache, cache_file)
      i0 <- i0 + length(idx)
      cli_progress_update(id = pb, set = i0)
    }
    cli_progress_done(id = pb)
    cli_alert_success(
      "Parallel match done: {scales::comma(n_run)} names queried \\
       ({total_err} transport errors, retried next run)"
    )
    if (is.finite(max_names) && length(to_query) > n_run) {
      cli_alert_info(
        "{scales::comma(length(to_query) - n_run)} names deferred \\
         (crosswalk_max_names_per_run) -- rerun to continue.")
    }
  }
}
saveRDS(api_cache, cache_file)

# ============================================================================
# Step 4: Assemble the crosswalk (name -> accepted Dyntaxa taxon, COL key)
# ============================================================================
cli_h2("Assembling crosswalk")

cache_dt <- rbindlist(lapply(names(api_cache), function(nm) {
  e <- api_cache[[nm]]
  data.table(name_std = nm,
             col_key  = e$col_key %||% NA_character_,
             status   = e$status  %||% NA_character_,
             match_type = e$mtype %||% "NONE",
             confidence = as.integer(e$conf %||% NA_integer_),
             col_name = e$col_name %||% NA_character_)
}), fill = TRUE)

# Accept a COL key only for EXACT matches, or FUZZY at/above the confidence
# floor. HIGHERRANK / NONE and low-confidence fuzzies carry no key. (A key that
# is a higher-rank COL id simply is not in the cube, so the membership test is
# self-cleaning — this filter just keeps the crosswalk honest.)
cache_dt[, keep := !is.na(col_key) & nzchar(col_key) &
  (match_type == "EXACT" |
     (match_type == "FUZZY" & !is.na(confidence) & confidence >= min_fuzzy_conf))]
cache_dt[keep == FALSE, col_key := NA_character_]

# Join names -> accepted taxon; one row per (accepted taxon, COL key), keeping
# the best supporting name (accepted-row over synonym, then higher confidence).
xwalk <- merge(
  name_map,
  cache_dt[keep == TRUE, .(name_std, col_key, status, match_type, confidence)],
  by = "name_std"
)
setorder(xwalk, accepted_taxonID, col_key, is_syn, -confidence)
crosswalk <- xwalk[, .SD[1], by = .(accepted_taxonID, col_key),
                   .SDcols = c("status", "match_type", "confidence", "name_std", "is_syn")]
setnames(crosswalk,
         c("accepted_taxonID", "status", "name_std", "is_syn"),
         c("dyntaxa_taxonID", "match_status", "name_used", "via_synonym"))
crosswalk <- unique(crosswalk, by = c("dyntaxa_taxonID", "col_key"))

saveRDS(crosswalk, crosswalk_file)
cli_alert_success(
  "Saved crosswalk: {.path {crosswalk_file}} \\
   ({scales::comma(nrow(crosswalk))} taxon-key links)"
)

# ============================================================================
# Step 5: Validate coverage vs cube + baseline
# ============================================================================
cli_h2("Validating against cube + baseline")

mtype_tab <- cache_dt[, .N, by = match_type][order(-N)]

report <- c(
  "# COL crosswalk validation", "",
  sprintf("_Generated %s_", format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("COL checklist: `%s`", col_checklist_key),
  sprintf("Include Dyntaxa synonyms: %s | fuzzy confidence floor: %s | bulk rgbif: %s",
          include_synonyms, min_fuzzy_conf, use_bulk),
  "",
  "## Matching",
  sprintf("- Unique Dyntaxa names queried/cached: %s", scales::comma(nrow(cache_dt))),
  sprintf("- Names with a kept COL key: %s", scales::comma(sum(!is.na(cache_dt$col_key)))),
  sprintf("- Taxon<->key links in crosswalk: %s", scales::comma(nrow(crosswalk))),
  "",
  "### Match-type breakdown",
  paste0("- ", mtype_tab$match_type, ": ", vapply(mtype_tab$N, scales::comma, character(1)))
)

if (length(cube_keys) > 0) {
  # Metric 1: matched cube species = cube keys that are some Dyntaxa taxon's COL key.
  cube_matched <- intersect(cube_keys, unique(crosswalk$col_key))
  pct_species  <- round(100 * length(cube_matched) / length(cube_keys), 1)
  occ_matched  <- cube_sp[specieskey %in% cube_matched, sum(total_occ)]
  occ_total    <- sum(cube_sp$total_occ)
  pct_occ      <- round(100 * occ_matched / occ_total, 2)

  # Metric 2: which accepted Dyntaxa taxa are present in GBIF (col_key in cube).
  cube_key_set <- unique(cube_keys)
  taxon_in_gbif <- crosswalk[col_key %in% cube_key_set, unique(dyntaxa_taxonID)]
  acc_all <- accepted[, .(taxonID = as.character(taxonID), threatStatus)]
  acc_all <- acc_all[, .SD[1], by = taxonID]
  acc_all[, in_gbif := taxonID %in% taxon_in_gbif]
  n_threat        <- acc_all[threatStatus %in% THREATENED_CODES, .N]
  n_threat_ingbif <- acc_all[threatStatus %in% THREATENED_CODES & in_gbif == TRUE, .N]
  missing_threat  <- n_threat - n_threat_ingbif

  cmp <- function(label, val, base, unit = "", better = c("higher", "lower")) {
    better <- match.arg(better)
    delta  <- round(val - base, 2)
    arrow  <- if (delta == 0) "=" else if (delta > 0) "+" else ""
    ok     <- if (better == "higher") val >= base else val <= base
    sprintf("- %s: **%s%s** (baseline %s%s, %s%s) %s", label, val, unit, base, unit,
            arrow, delta, if (ok) "OK" else "REGRESSED -- investigate")
  }

  report <- c(report, "",
    "## Coverage vs baseline (must not regress)",
    cmp("Matched cube species %", pct_species, BASE_MATCH_PCT, "%", "higher"),
    cmp("Occurrence coverage %", pct_occ, BASE_OCC_PCT, "%", "higher"),
    sprintf("- Matched cube species: %s / %s",
            scales::comma(length(cube_matched)), scales::comma(length(cube_keys))),
    "",
    "## Missing threatened (must not regress)",
    sprintf("- Threatened accepted Dyntaxa taxa: %s", scales::comma(n_threat)),
    sprintf("- ...present in GBIF: %s", scales::comma(n_threat_ingbif)),
    cmp("Missing threatened taxa", missing_threat, BASE_MISSING_THREATENED, "", "lower")
  )

  cli_alert_success("Matched cube species: {pct_species}% (baseline {BASE_MATCH_PCT}%)")
  cli_alert_success("Occurrence coverage: {pct_occ}% (baseline {BASE_OCC_PCT}%)")
  cli_alert_success("Missing threatened: {missing_threat} (baseline {BASE_MISSING_THREATENED})")
} else {
  report <- c(report, "", "## Coverage",
              "_Cube species_summary not found — rerun after 06b to validate coverage._")
}

dir.create(dirname(report_file), showWarnings = FALSE, recursive = TRUE)
writeLines(report, report_file)
cli_alert_success("Wrote validation report: {.path {report_file}}")

elapsed <- round(difftime(Sys.time(), timer_start, units = "mins"), 1)
cli_alert_info("Elapsed: {elapsed} minutes")
cli_alert_info("Review {.path {report_file}}; if coverage holds, the 09a tier-replacement is next.")
