# scripts/09a1_build_col_crosswalk.R
# ============================================================================
# Build + validate the authoritative Dyntaxa <-> COL crosswalk
# ============================================================================
# WHY
#   GBIF interprets names against the Catalogue of Life Extended Release
#   (COL XR) backbone, so the occurrence cube's `specieskey` is a COL taxonID
#   (alphanumeric, e.g. "DGND"). This builds the reconciliation authority:
#   match every national-taxonomy (Dyntaxa) name to COL via GBIF's v2 match API
#   (checklistKey = COL) and record the COL key it resolves to.
#
#   The v2 match `usage.key` — or `acceptedUsage.key` when the Dyntaxa name is
#   a COL synonym — IS the same alphanumeric id the cube stores in `specieskey`
#     Anas crecca      -> usage.key       "DGND"  (in cube)
#     Sylvia communis  -> acceptedUsage.key "DDBNS" (Curruca communis; in cube)
#   So a Dyntaxa taxon is "present in GBIF" iff its COL key is in the cube's
#   specieskey set — no integer-usageKey hop, and COL-name divergence
#   (Sylvia->Curruca) is resolved in one step.
#
# WHAT THIS DOES  (it does NOT modify 09a — it builds the crosswalk that 09a reads
#                  as the ADDITIVE Tier 5; augment, not replacement: it catches
#                  species Tiers 1-4 miss and works whatever the key format.)
#   1. Loads Dyntaxa (03) + the cube species universe (06b species_summary).
#   2. Matches each unique Dyntaxa name -> COL key (cached + resumable) with
#      parallel httr2 requests to /v2/species/match; an optional bulk path via
#      rgbif::name_backbone_checklist() can fill the cache first.
#   3. Writes the crosswalk  data/{CC}/proc/col_crosswalk.rds
#      (dyntaxa_taxonID, col_key, match_status, match_type, confidence, name_used,
#      via_synonym, via_nominate). A name that COL files under a nominate
#      infraspecific taxon is linked to the cube key of that taxon's species
#      (via_nominate).
#   4. VALIDATES coverage vs the cube and the configured baseline
#      (parameters.taxonomic.crosswalk_baseline) and writes
#      data/{CC}/proc/gaps/col_crosswalk_validation.md, plus
#      col_crosswalk_infraspecific.csv: the species-rank taxa that COL files
#      under another infraspecific taxon.
#
# RUN:  source("scripts/09a1_build_col_crosswalk.R")     (after 03 + 06b)
# Config (all optional; safe defaults, NO config-file change required):
#   parameters.taxonomic.col_checklist_key            (fallback in R/globals.R)
#   parameters.taxonomic.crosswalk_include_synonyms   default TRUE
#   parameters.taxonomic.crosswalk_min_fuzzy_confidence default 90
#   parameters.taxonomic.crosswalk_use_bulk_rgbif     default FALSE; TRUE = rgbif bulk first
#   parameters.taxonomic.crosswalk_parallel_conc / crosswalk_parallel_chunk /
#     crosswalk_max_names_per_run                     (httr2 caps; default 20 / 500 / Inf)
# Dependencies: data.table, stringr, here, cli; httr2 (parallel) and/or rgbif (bulk).
# ============================================================================

source(here::here("scripts", "00_setup.R"))

timer_start <- Sys.time()
cli_h1("09a1 -- Build + validate Dyntaxa<->COL crosswalk")

# ----------------------------------------------------------------------------
# Config (defaults are safe; no config file change required)
# ----------------------------------------------------------------------------
col_checklist_key <- get_col_checklist_key()
include_synonyms  <- isTRUE(cfg_get("parameters.taxonomic.crosswalk_include_synonyms", TRUE))
min_fuzzy_conf    <- cfg_get("parameters.taxonomic.crosswalk_min_fuzzy_confidence", 90)
use_bulk          <- isTRUE(cfg_get("parameters.taxonomic.crosswalk_use_bulk_rgbif", FALSE))  # default off: the parallel httr2 path below is fast + rgbif-version-independent
api_rate_limit    <- 10
api_max_requests  <- cfg_get("parameters.taxonomic.api_max_requests", 5000)
api_max_batches   <- cfg_get("parameters.taxonomic.api_max_batches", Inf)

cache_file     <- here(p_data_proc, "col_crosswalk_cache.rds")
crosswalk_file <- here(p_data_proc, "col_crosswalk.rds")
report_file    <- here(p_gaps, "col_crosswalk_validation.md")
# THREATENED_CODES comes from R/globals.R (config-driven, one definition).

# Regression baseline: config, not code (parameters.taxonomic.crosswalk_baseline).
# A hardcoded baseline goes stale with every Dyntaxa/COL/cube refresh and then
# flags a false REGRESSED. NA = not configured: the report then prints the
# values without comparing.
.xw_base <- cfg_get("parameters.taxonomic.crosswalk_baseline", list())
BASE_MATCH_PCT          <- as.numeric(.xw_base$match_pct          %||% NA_real_)
BASE_OCC_PCT            <- as.numeric(.xw_base$occ_pct            %||% NA_real_)
# `missing_threatened` is accepted as an alias of `threatened_not_crosswalked`.
BASE_MISSING_THREATENED <- as.numeric(
  .xw_base$threatened_not_crosswalked %||% .xw_base$missing_threatened %||% NA_real_)

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
  p_derived,
  pattern = "species_summary.*10km\\.csv$", recursive = TRUE, full.names = TRUE
)
cube_sp <- NULL
cube_names <- data.table(name_std = character(0), specieskey = character(0))
if (length(sum_files) == 0) {
  cli_alert_warning(
    "No species_summary files (run 06b) -- crosswalk will build but cube \\
     validation is skipped."
  )
} else {
  gbif_raw <- rbindlist(lapply(sum_files, fread,
    select = c("specieskey", "species", "occurrences", "basisofrecord")), fill = TRUE)
  gbif_raw[, specieskey := as.character(specieskey)]
  cube_sp <- gbif_raw[basisofrecord == "all",
    .(total_occ = sum(as.numeric(occurrences), na.rm = TRUE)), by = specieskey]
  # Species name -> cube key, for the names that stand for one key. It gives the
  # cube key of a species from the name of its nominate subspecies (Step 4).
  cube_names <- unique(gbif_raw[basisofrecord == "all" & !is.na(species) & species != "",
                                .(name_std = tolower(trimws(species)), specieskey)])
  cube_names <- cube_names[, if (.N == 1L) .SD, by = name_std]
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
# max_age_days = NULL: like the synonym cache, this is tens of thousands of
# match calls. Refresh deliberately via parameters.cache.force_refresh.
api_cache <- load_cache(cache_file, "COL crosswalk cache", max_age_days = NULL)

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
# Populates the cache for as many names as it can; the httr2 path mops up the
# rest. Column names vary by rgbif version, so extract defensively and only
# trust a clean one-row-per-input result (else defer everything to httr2).
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
# bucket_size 300, so a cap of ~20 is very conservative and still ~10-20x faster
# than a per-name serial loop. Resumes from cache; transport errors are NOT
# cached, so they retry on the next run. Tunable:
# parameters.taxonomic.crosswalk_parallel_conc.
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
          req_error(is_error = function(resp) FALSE)   # HTTP errors come back as responses
      })
      resps <- tryCatch(perform_parallel(reqs), error = function(e) {
        cli_alert_warning("Parallel chunk failed ({conditionMessage(e)}) -- retrying serially")
        lapply(reqs, function(rq) tryCatch(req_perform(rq), error = function(e) e))
      })
      for (k in seq_along(nms)) {
        r <- resps[[k]]
        # Only a successful reply is an answer. An HTTP error (4xx/5xx, e.g. a
        # rate limit or an outage) can carry a JSON body that would parse as
        # "no match"; it is not cached, so the name is retried on the next run.
        ok <- inherits(r, "httr2_response") && resp_status(r) < 400
        j  <- if (ok) tryCatch(resp_body_json(r), error = function(e) NULL) else NULL
        if (!is.null(j)) {
          api_cache[[nms[k]]] <- extract_match(j$usage, j$acceptedUsage, j$diagnostics)
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
# self-cleaning — this filter just keeps the crosswalk honest.) The v2 API
# reports spelling variants as VARIANT, not FUZZY. They carry no key either:
# some of them join two species (Muscicapa albicilla to Ficedula albicollis).
cache_dt[, keep := !is.na(col_key) & nzchar(col_key) &
  (match_type == "EXACT" |
     (match_type == "FUZZY" & !is.na(confidence) & confidence >= min_fuzzy_conf))]
cache_dt[keep == FALSE, col_key := NA_character_]

# A name that COL files under a nominate infraspecific taxon ("Spinulum
# annotinum annotinum" for Lycopodium annotinum) is, at species rank, the
# species itself. The cube is keyed at species rank, so its records sit under
# the species' key, and the name is linked to that key (via_nominate). A name
# filed under any other infraspecific taxon ("Dactylis glomerata lobata" for
# Dactylis polygama) keeps that taxon's own key: its records are inside the
# species' key and cannot be told apart there. The validation report counts
# both. An infraspecific COL name has three words and no hybrid sign; the first
# two are the species.
col_words <- strsplit(tolower(cache_dt$col_name), " ", fixed = TRUE)
is_infra  <- cache_dt$keep & lengths(col_words) == 3L & !grepl("\u00d7", cache_dt$col_name)
is_infra[is.na(is_infra)] <- FALSE
col_species <- rep(NA_character_, nrow(cache_dt))
col_species[is_infra] <- vapply(col_words[is_infra],
                                function(w) paste(w[1:2], collapse = " "), character(1))
is_nominate <- is_infra
is_nominate[is_infra] <- vapply(col_words[is_infra], function(w) w[2] == w[3], logical(1))

cache_dt[, `:=`(col_infraspecific = is_infra,
                col_nominate      = is_nominate,
                col_species       = col_species,
                species_cube_key  = NA_character_)]
cache_dt[cube_names, on = c(col_species = "name_std"), species_cube_key := i.specieskey]
cache_dt[, via_nominate := col_nominate & !is.na(species_cube_key)]
cache_dt[via_nominate == TRUE, col_key := species_cube_key]
cli_alert_info(
  "Names under a nominate infraspecific COL taxon: \\
   {scales::comma(sum(cache_dt$col_nominate))} \\
   ({scales::comma(sum(cache_dt$via_nominate))} linked to the species' cube key)"
)

# Join names -> accepted taxon; one row per (accepted taxon, COL key), keeping
# the best supporting name (accepted-row over synonym, then higher confidence).
xwalk <- merge(
  name_map,
  cache_dt[keep == TRUE, .(name_std, col_key, status, match_type, confidence, via_nominate)],
  by = "name_std"
)
setorder(xwalk, accepted_taxonID, col_key, via_nominate, is_syn, -confidence)
crosswalk <- xwalk[, .SD[1], by = .(accepted_taxonID, col_key),
                   .SDcols = c("status", "match_type", "confidence", "name_std", "is_syn",
                               "via_nominate")]
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

# Species-rank taxa whose own name COL files under an infraspecific taxon that
# is not the nominate one, with that taxon's species in the cube. Their records
# are inside the species' cube key; whether they have any cannot be read from a
# species-level cube.
other_infra <- merge(
  name_map[is_syn == FALSE, .(name_std, dyntaxa_taxonID = accepted_taxonID, threatStatus,
                              taxonRank = if ("taxonRank" %in% names(name_map)) taxonRank
                                          else NA_character_)],
  cache_dt[col_infraspecific == TRUE & col_nominate == FALSE & !is.na(species_cube_key),
           .(name_std, col_name, col_species, species_cube_key)],
  by = "name_std"
)
other_infra <- other_infra[is.na(taxonRank) | tolower(taxonRank) == "species"]
other_infra <- other_infra[order(!(threatStatus %in% THREATENED_CODES), name_std)]
other_infra_file <- here(p_gaps, "col_crosswalk_infraspecific.csv")
dir.create(dirname(other_infra_file), showWarnings = FALSE, recursive = TRUE)
fwrite(other_infra, other_infra_file)

report <- c(report, "",
  "## Names that COL files below species rank",
  sprintf("- Under a nominate infraspecific taxon: %s names, %s of them linked to the cube key of the species",
          scales::comma(sum(cache_dt$col_nominate)), scales::comma(sum(cache_dt$via_nominate))),
  sprintf("- Under another infraspecific taxon: %s names. Their records are inside the species' cube key and cannot be told apart there.",
          scales::comma(sum(cache_dt$col_infraspecific & !cache_dt$col_nominate))),
  sprintf("- Species-rank taxa whose own name is filed that way, with the species in the cube: %s (threatened: %s). Listed in `col_crosswalk_infraspecific.csv`.",
          scales::comma(uniqueN(other_infra$dyntaxa_taxonID)),
          scales::comma(uniqueN(other_infra[threatStatus %in% THREATENED_CODES, dyntaxa_taxonID])))
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
    if (is.na(base)) {
      return(sprintf("- %s: **%s%s** (no baseline configured)", label, val, unit))
    }
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
    "## Threatened taxa reached by the crosswalk (must not regress)",
    paste0("_A health check on the crosswalk, NOT the official \"missing threatened\" figure. ",
           "That one (docs/metrics.md, Overview) is scoped to species rank and uses every ",
           "matching tier, so it is lower._"),
    sprintf("- Threatened accepted Dyntaxa taxa (unscoped): %s", scales::comma(n_threat)),
    sprintf("- ...reached through the crosswalk alone: %s", scales::comma(n_threat_ingbif)),
    cmp("Threatened taxa not reached by the crosswalk", missing_threat,
        BASE_MISSING_THREATENED, "", "lower")
  )

  cli_alert_info("Matched cube species: {pct_species}% (baseline {BASE_MATCH_PCT}%)")
  cli_alert_info("Occurrence coverage: {pct_occ}% (baseline {BASE_OCC_PCT}%)")
  cli_alert_info("Threatened not reached by crosswalk: {missing_threat} (baseline {BASE_MISSING_THREATENED})")
  n_regressed <- sum(grepl("REGRESSED", report, fixed = TRUE))
  if (n_regressed > 0) {
    cli_alert_warning("{n_regressed} crosswalk metric{?s} below baseline -- see {.path {report_file}}")
  }
} else {
  report <- c(report, "", "## Coverage",
              "_Cube species_summary not found — rerun after 06b to validate coverage._")
}

dir.create(dirname(report_file), showWarnings = FALSE, recursive = TRUE)
writeLines(report, report_file)
cli_alert_success("Wrote validation report: {.path {report_file}}")

elapsed <- round(difftime(Sys.time(), timer_start, units = "mins"), 1)
cli_alert_info("Elapsed: {elapsed} minutes")
cli_alert_info("Review {.path {report_file}}; the crosswalk feeds 09a as the additive Tier 5.")
