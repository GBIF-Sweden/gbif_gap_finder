# scripts/09a_reconcile_taxonomy.R
# ============================================================================
# Taxonomic Reconciliation: GBIF Species <-> National Taxonomy
# ============================================================================
# Purpose:
#   Build the best possible mapping between GBIF occurrence species and the
#   national taxonomy backbone (Dyntaxa for Sweden). Uses a 4-tier strategy
#   to maximise the match rate before the gap analysis in 09b.
#
#   This script runs BEFORE 09b_taxonomic_gaps.R and produces a lookup
#   table that 09b reads instead of doing its own name matching.
#
# Matching tiers:
#   Tier 1 -- Direct accepted name match (local)
#   Tier 2 -- Synonym name match (local, via Dyntaxa synonym rows)
#   Tier 3 -- Infraspecific collapse (strip subsp/var, re-match locally)
#   Tier 4 -- GBIF Species API (for remaining unmatched, cached)
#
# Inputs:
#   - data_proc/derived/by_order/species_summary/*_10km.csv  (GBIF species)
#   - data_proc/derived/by_family/species_summary/*_10km.csv
#   - data_proc/taxa_reference_current.rds                   (Dyntaxa)
#
# Outputs:
#   - data_proc/taxonomic_reconciliation.rds       Main lookup table
#   - data_proc/gbif_name_cache.rds                Cached GBIF API responses
#   - data_proc/gaps/taxonomic_match_table.csv      Reconciliation as CSV
#   - data_proc/gaps/taxonomic_reconciliation_summary.csv  Tier-level summary
#
# Dependencies: data.table, stringr, here, cli, httr2 (optional, for Tier 4)
# ============================================================================

library(here)
library(data.table)
library(stringr)
library(cli)

source(here("scripts", "00_setup.R"))

# Null-coalescing operator (in case purrr is not attached)
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b

timer_start <- Sys.time()


# ============================================================================
# Configuration
# ============================================================================

# Occurrence threshold: only query the GBIF API for unmatched species with
# at least this many occurrences. Species below this are likely vagrants
# or misidentifications that aren't in the national backbone anyway.
api_min_occurrences <- cfg_get("parameters.taxonomic.api_min_occurrences", 10)

# Maximum number of API requests per run (safety valve).
# If there are more candidates, the remainder will be picked up on
# the next run (cache persists across runs).
api_max_requests <- cfg_get("parameters.taxonomic.api_max_requests", 5000)

# GBIF API rate limit (requests per second, GBIF allows ~10)
api_rate_limit <- 10

# Cache file for GBIF API responses (persists across reruns)
cache_file <- here(p_data_proc, "gbif_name_cache.rds")

# Output paths
reconciliation_file <- here(p_data_proc, "taxonomic_reconciliation.rds")
gaps_dir <- here(p_data_proc, cfg_get("gaps.dir", "gaps"))
dir.create(gaps_dir, showWarnings = FALSE, recursive = TRUE)

cli_h1("09a -- Taxonomic Reconciliation")


# ============================================================================
# Step 1: Load GBIF Species
# ============================================================================

cli_h2("Loading GBIF species from occurrence cubes")

sum_files <- list.files(
  here(p_data_proc, "derived"),
  pattern = "species_summary.*10km\\.csv$",
  recursive = TRUE, full.names = TRUE
)

if (length(sum_files) == 0) {
  cli_abort("No species_summary files found in {.path {here(p_data_proc, 'derived')}}. Run 06b first.")
}
cli_alert_info("Reading {length(sum_files)} species_summary files")

gbif_raw <- rbindlist(lapply(sum_files, fread,
  select = c("specieskey", "species", "basisofrecord", "occurrences")
))

# Aggregate: one row per specieskey with total occurrences.
# A species may appear in multiple files (by_order + by_family splits).
gbif_species <- gbif_raw[
  basisofrecord == "all",
  .(total_occ = sum(as.numeric(occurrences), na.rm = TRUE)),
  by = .(specieskey, species)
]
setorder(gbif_species, -total_occ)

# Standardise names for matching
gbif_species[, name_std := tolower(trimws(species))]

n_gbif <- nrow(gbif_species)
cli_alert_success("GBIF species: {scales::comma(n_gbif)}")
cli_alert_info("Total occurrences: {scales::comma(sum(gbif_species$total_occ))}")


# ============================================================================
# Step 2: Load National Taxonomy (Dyntaxa)
# ============================================================================

cli_h2("Loading national taxonomy backbone")

taxa_file <- here(p_data_proc, "taxa_reference_current.rds")
if (!file.exists(taxa_file)) {
  cli_abort("Taxonomy reference not found: {.path {taxa_file}}. Run script 03 first.")
}

taxa <- as.data.table(readRDS(taxa_file))
taxa[, name_std := tolower(trimws(scientificName))]

# Classify rows: accepted (taxonID == acceptedNameUsageID) vs synonym
taxa[, is_accepted := (taxonID == acceptedNameUsageID)]

accepted_taxa <- taxa[is_accepted == TRUE]
synonym_taxa  <- taxa[is_accepted == FALSE]

accepted_names    <- unique(accepted_taxa$name_std)
all_dyntaxa_names <- unique(taxa$name_std)

cli_alert_info("Dyntaxa accepted: {scales::comma(nrow(accepted_taxa))}")
cli_alert_info("Dyntaxa synonyms: {scales::comma(nrow(synonym_taxa))}")
cli_alert_info("Unique accepted names: {scales::comma(length(accepted_names))}")
cli_alert_info("Combined unique names: {scales::comma(length(all_dyntaxa_names))}")


# ============================================================================
# Step 3: Build Synonym Lookup
# ============================================================================
# For each synonym name in Dyntaxa, resolve to its accepted taxon.
# This lookup is used in Tier 2, Tier 3, and Tier 4.

cli_h2("Building synonym resolution lookup")

syn_lookup <- synonym_taxa[, .(
  name_std,
  syn_taxonID      = taxonID,
  accepted_taxonID = acceptedNameUsageID
)]

# Add the accepted scientific name by joining back to accepted taxa
syn_lookup <- merge(
  syn_lookup,
  accepted_taxa[, .(taxonID, accepted_name = scientificName,
                    accepted_name_std = name_std)],
  by.x = "accepted_taxonID", by.y = "taxonID",
  all.x = TRUE
)

# Flag ambiguous synonyms (one name pointing to >1 accepted taxon)
syn_name_counts <- syn_lookup[, .N, by = name_std]
ambiguous_syns  <- syn_name_counts[N > 1, name_std]
syn_lookup[, is_ambiguous := name_std %in% ambiguous_syns]

# Deduplicate: keep first per name (for automated matching)
syn_lookup_unique <- syn_lookup[, .SD[1], by = name_std]

cli_alert_info("Unique synonym names: {scales::comma(nrow(syn_lookup_unique))}")
if (length(ambiguous_syns) > 0) {
  cli_alert_warning("Ambiguous synonyms (>1 accepted taxon): {scales::comma(length(ambiguous_syns))}")
}


# ============================================================================
# Initialise the Reconciliation Table
# ============================================================================
# Every GBIF species gets exactly one row. We fill in the Dyntaxa match
# tier by tier. At the end, unmatched species get categorised.

recon <- gbif_species[, .(specieskey, species, name_std, total_occ)]
recon[, `:=`(
  dyntaxa_taxonID        = NA_character_,
  dyntaxa_scientificName = NA_character_,
  match_tier             = NA_character_,
  match_type             = NA_character_,
  match_name_used        = NA_character_
)]


# ============================================================================
# TIER 1: Direct Accepted Name Match
# ============================================================================
# Does the GBIF species name appear as an accepted Dyntaxa name?
# This is what the current script 09 does.

cli_h2("Tier 1: Direct accepted name match")

t1_mask <- recon$name_std %in% accepted_names

# Get the taxonID for each match
t1_join <- merge(
  recon[t1_mask, .(specieskey, name_std)],
  accepted_taxa[, .(name_std, taxonID, scientificName)][, .SD[1], by = name_std],
  by = "name_std"
)

recon[t1_join, on = "specieskey",
      `:=`(dyntaxa_taxonID        = i.taxonID,
           dyntaxa_scientificName = i.scientificName,
           match_tier             = "tier1",
           match_type             = "accepted_name",
           match_name_used        = name_std)]

n_t1   <- sum(recon$match_tier == "tier1", na.rm = TRUE)
occ_t1 <- if (n_t1 > 0) recon[match_tier == "tier1", sum(as.numeric(total_occ))] else 0
cli_alert_success("Tier 1: {scales::comma(n_t1)} species ({scales::comma(occ_t1)} occ)")
cli_alert_info("  Remaining: {scales::comma(sum(is.na(recon$match_tier)))}")


# ============================================================================
# TIER 2: Dyntaxa Synonym Match
# ============================================================================
# Does the GBIF species name appear as a SYNONYM in Dyntaxa?
# If so, resolve to the accepted Dyntaxa taxon.

cli_h2("Tier 2: Dyntaxa synonym match")

t2_pool <- recon[is.na(match_tier), .(specieskey, name_std)]

t2_join <- merge(
  t2_pool,
  syn_lookup_unique[, .(name_std, accepted_taxonID, accepted_name,
                        is_ambiguous)],
  by = "name_std"
)

recon[t2_join, on = "specieskey",
      `:=`(dyntaxa_taxonID        = i.accepted_taxonID,
           dyntaxa_scientificName = i.accepted_name,
           match_tier             = fifelse(i.is_ambiguous,
                                            "tier2_ambiguous", "tier2"),
           match_type             = "synonym",
           match_name_used        = name_std)]

n_t2       <- sum(recon$match_tier %in% c("tier2", "tier2_ambiguous"), na.rm = TRUE)
n_t2_ambig <- sum(recon$match_tier == "tier2_ambiguous", na.rm = TRUE)
occ_t2     <- if (n_t2 > 0) recon[match_tier %in% c("tier2", "tier2_ambiguous"),
                    sum(as.numeric(total_occ))] else 0

cli_alert_success("Tier 2: {scales::comma(n_t2)} species ({scales::comma(occ_t2)} occ)")
if (n_t2_ambig > 0) {
  cli_alert_warning("  Of which {n_t2_ambig} ambiguous (>1 possible accepted taxon)")
}
cli_alert_info("  Remaining: {scales::comma(sum(is.na(recon$match_tier)))}")

# Show top synonym resolutions
if (n_t2 > 0) {
  top_t2 <- recon[match_tier == "tier2"][order(-total_occ)][seq_len(min(5, n_t2))]
  cli_alert_info("  Top synonym matches:")
  for (i in seq_len(nrow(top_t2))) {
    cli_alert_info("    {top_t2$species[i]} -> {top_t2$dyntaxa_scientificName[i]} ({scales::comma(top_t2$total_occ[i])} occ)")
  }
}


# ============================================================================
# TIER 3: Infraspecific Collapse
# ============================================================================
# For names with 3+ words (or containing subsp./var./f.), extract the
# binomial (first two words) and try matching that against Dyntaxa.

cli_h2("Tier 3: Infraspecific collapse to binomial")

t3_pool <- recon[is.na(match_tier), .(specieskey, name_std, total_occ)]
t3_pool[, n_words := sapply(strsplit(name_std, "\\s+"), length)]
t3_pool[, is_infra := n_words >= 3 |
          grepl("\\bsubsp\\b|\\bvar\\b|\\bf\\.", name_std)]

t3_candidates <- t3_pool[is_infra == TRUE]
n_t3 <- 0

if (nrow(t3_candidates) > 0) {
  t3_candidates[, binomial := paste(word(name_std, 1), word(name_std, 2))]
  t3_candidates[, binom_in_dyntaxa := binomial %in% all_dyntaxa_names]
  t3_hits <- t3_candidates[binom_in_dyntaxa == TRUE]

  if (nrow(t3_hits) > 0) {
    # Resolve binomial: try accepted names first, then synonyms
    t3_resolved <- merge(
      t3_hits[, .(specieskey, name_std, binomial)],
      accepted_taxa[, .(binomial = name_std, taxonID,
                        scientificName)][, .SD[1], by = binomial],
      by = "binomial", all.x = TRUE
    )

    # Fallback to synonym lookup for unresolved binomials
    t3_na <- t3_resolved[is.na(taxonID)]
    if (nrow(t3_na) > 0) {
      t3_syn <- merge(
        t3_na[, .(specieskey, binomial)],
        syn_lookup_unique[, .(binomial = name_std,
                              taxonID = accepted_taxonID,
                              scientificName = accepted_name)],
        by = "binomial", all.x = TRUE
      )
      t3_resolved[t3_syn, on = "specieskey",
                  `:=`(taxonID = i.taxonID,
                       scientificName = i.scientificName)]
    }

    t3_resolved <- t3_resolved[!is.na(taxonID)]

    if (nrow(t3_resolved) > 0) {
      recon[t3_resolved, on = "specieskey",
            `:=`(dyntaxa_taxonID        = i.taxonID,
                 dyntaxa_scientificName = i.scientificName,
                 match_tier             = "tier3",
                 match_type             = "infraspecific_collapse",
                 match_name_used        = i.binomial)]
      n_t3 <- nrow(t3_resolved)
    }
  }
}

occ_t3 <- if (n_t3 > 0) recon[match_tier == "tier3", sum(as.numeric(total_occ))] else 0
cli_alert_success("Tier 3: {scales::comma(n_t3)} species ({scales::comma(occ_t3)} occ)")
cli_alert_info("  Infraspecific candidates tested: {nrow(t3_candidates)}")
cli_alert_info("  Remaining: {scales::comma(sum(is.na(recon$match_tier)))}")


# ============================================================================
# TIER 4: GBIF Species API
# ============================================================================
# For remaining unmatched species above the occurrence threshold:
#   1. Query GBIF Species API for synonyms of each specieskey
#   2. Check if any returned synonym matches a Dyntaxa name
#   3. Cache all API responses so reruns are instant
#
# API endpoint: GET https://api.gbif.org/v1/species/{specieskey}/synonyms

cli_h2("Tier 4: GBIF Species API lookup")

t4_pool <- recon[is.na(match_tier), .(specieskey, species, name_std, total_occ)]

# Filter: above occurrence threshold, non-hybrid, at least binomial
t4_pool[, n_words := sapply(strsplit(name_std, "\\s+"), length)]
t4_pool[, is_hybrid := grepl("\u00d7| x ", name_std)]
t4_candidates <- t4_pool[total_occ >= api_min_occurrences &
                          n_words >= 2 &
                          is_hybrid == FALSE]

cli_alert_info("Tier 4 candidates: {scales::comma(nrow(t4_candidates))} species")
cli_alert_info("  (>= {api_min_occurrences} occ, non-hybrid, binomial+)")

# --- Load or initialise cache ---
if (file.exists(cache_file)) {
  api_cache <- readRDS(cache_file)
  cli_alert_info("Loaded API cache: {scales::comma(length(api_cache))} entries")
} else {
  api_cache <- list()
  cli_alert_info("No existing API cache -- starting fresh")
}

# Determine which specieskeys still need querying
t4_candidates[, sk_chr := as.character(specieskey)]
already_cached <- t4_candidates$sk_chr %in% names(api_cache)
to_query <- t4_candidates[!already_cached]

n_to_query <- min(nrow(to_query), api_max_requests)

cli_alert_info("Already cached: {scales::comma(sum(already_cached))}")
cli_alert_info("Need API query: {scales::comma(nrow(to_query))}")
if (nrow(to_query) > api_max_requests) {
  cli_alert_warning("Capped at {api_max_requests} per run. Remaining will be queried on next rerun.")
}

# --- Query the API ---
api_available <- TRUE

if (n_to_query > 0) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli_alert_warning("Package {.pkg httr2} not available -- skipping API queries")
    cli_alert_info("Install with: {.code install.packages('httr2')}")
    api_available <- FALSE
    n_to_query <- 0
  }
}

if (n_to_query > 0 && api_available) {
  library(httr2)

  to_query <- to_query[1:n_to_query]
  est_minutes <- round(n_to_query / api_rate_limit / 60, 1)
  cli_alert_info("Querying GBIF API for {scales::comma(n_to_query)} species (~{est_minutes} min)")

  pb <- cli_progress_bar("GBIF Species API", total = n_to_query)
  n_api_errors <- 0

  for (i in seq_len(n_to_query)) {
    sk <- to_query$sk_chr[i]

    tryCatch({
      resp <- request(paste0("https://api.gbif.org/v1/species/", sk, "/synonyms")) |>
        req_url_query(limit = 100) |>
        req_retry(max_tries = 3, backoff = ~ 2) |>
        req_throttle(rate = api_rate_limit) |>
        req_perform()

      body <- resp_body_json(resp)
      results <- body$results %||% list()

      # Extract synonym names from API results
      # scientificName includes authorship (e.g., "Cortinarius odorifer (Britzelm.) Bres.")
      # species is the canonical binomial (e.g., "Cortinarius odorifer")
      # We need both, plus a stripped version of scientificName as fallback
      # when the 'species' field is missing
      all_names <- character(0)

      for (r in results) {
        sn <- tolower(trimws(r$scientificName %||% ""))
        cn <- tolower(trimws(r$species %||% ""))

        if (nzchar(cn)) all_names <- c(all_names, cn)
        if (nzchar(sn)) {
          all_names <- c(all_names, sn)
          # Also extract canonical binomial from scientificName
          # (first two lowercase words, ignoring authorship)
          parts <- strsplit(sn, "\\s+")[[1]]
          if (length(parts) >= 2 &&
              grepl("^[a-z]+$", parts[1]) &&
              grepl("^[a-z-]+$", parts[2])) {
            all_names <- c(all_names, paste(parts[1], parts[2]))
          }
        }
      }

      all_names <- unique(all_names[all_names != ""])

      api_cache[[sk]] <- list(
        synonyms   = all_names,
        queried_at = Sys.time(),
        n_results  = length(results)
      )
    },
    error = function(e) {
      api_cache[[sk]] <<- list(
        synonyms   = character(0),
        queried_at = Sys.time(),
        error      = conditionMessage(e)
      )
      n_api_errors <<- n_api_errors + 1
    })

    cli_progress_update(id = pb)

    # Save cache periodically (every 500 queries) in case of crash
    if (i %% 500 == 0) {
      saveRDS(api_cache, cache_file)
    }
  }

  cli_progress_done(id = pb)

  if (n_api_errors > 0) {
    cli_alert_warning("API errors: {n_api_errors} / {n_to_query}")
  }

  # Save final cache
  saveRDS(api_cache, cache_file)
  cli_alert_success("API cache saved: {scales::comma(length(api_cache))} total entries")
}

# --- Resolve cached synonyms to Dyntaxa ---
n_t4 <- 0
occ_t4 <- 0

if (nrow(t4_candidates) > 0) {
  t4_results <- rbindlist(lapply(seq_len(nrow(t4_candidates)), function(i) {
    sk  <- t4_candidates$sk_chr[i]
    row <- t4_candidates[i]

    cached <- api_cache[[sk]]
    if (is.null(cached) || length(cached$synonyms) == 0) {
      return(NULL)
    }

    syn_names <- cached$synonyms
    matched_name <- NA_character_

    # Check against accepted names first (more reliable)
    hit_acc <- syn_names[syn_names %in% accepted_names]
    if (length(hit_acc) > 0) {
      matched_name <- hit_acc[1]
    } else {
      # Check against synonym names
      hit_syn <- syn_names[syn_names %in% syn_lookup_unique$name_std]
      if (length(hit_syn) > 0) {
        matched_name <- hit_syn[1]
      }
    }

    if (!is.na(matched_name)) {
      data.table(specieskey = row$specieskey, api_matched_name = matched_name)
    } else {
      NULL
    }
  }), fill = TRUE)

  if (!is.null(t4_results) && nrow(t4_results) > 0) {
    # Resolve the matched API name to a Dyntaxa accepted taxon
    t4_results[, name_std := api_matched_name]

    # Try accepted
    t4_resolved <- merge(
      t4_results,
      accepted_taxa[, .(name_std, taxonID, scientificName)][
        , .SD[1], by = name_std],
      by = "name_std", all.x = TRUE
    )

    # Try synonym for unresolved
    t4_na <- t4_resolved[is.na(taxonID)]
    if (nrow(t4_na) > 0) {
      t4_syn <- merge(
        t4_na[, .(specieskey, name_std)],
        syn_lookup_unique[, .(name_std,
                              taxonID = accepted_taxonID,
                              scientificName = accepted_name)],
        by = "name_std", all.x = TRUE
      )
      t4_resolved[t4_syn, on = "specieskey",
                  `:=`(taxonID = i.taxonID,
                       scientificName = i.scientificName)]
    }

    t4_resolved <- t4_resolved[!is.na(taxonID)]

    if (nrow(t4_resolved) > 0) {
      recon[t4_resolved, on = "specieskey",
            `:=`(dyntaxa_taxonID        = i.taxonID,
                 dyntaxa_scientificName = i.scientificName,
                 match_tier             = "tier4",
                 match_type             = "gbif_api_synonym",
                 match_name_used        = i.api_matched_name)]

      n_t4   <- nrow(t4_resolved)
      occ_t4 <- recon[match_tier == "tier4", sum(as.numeric(total_occ))]
    }
  }
}

cli_alert_success("Tier 4: {scales::comma(n_t4)} species ({scales::comma(occ_t4)} occ)")
cli_alert_info("  Remaining: {scales::comma(sum(is.na(recon$match_tier)))}")


# ============================================================================
# Categorise Unmatched Species
# ============================================================================

cli_h2("Categorising unmatched species")

unmatched_mask <- is.na(recon$match_tier)
n_unmatched_raw <- sum(unmatched_mask)

# Assign tier label
recon[unmatched_mask, match_tier := "unmatched"]

# Sub-categorise to help downstream users understand what's missing
recon[, n_words_tmp := fifelse(
  match_tier == "unmatched",
  as.integer(sapply(strsplit(name_std, "\\s+"), length)),
  NA_integer_
)]

recon[match_tier == "unmatched", match_type := fcase(
  n_words_tmp == 1,                              "genus_only",
  grepl("\u00d7| x ", name_std),                 "hybrid",
  grepl("\\bsubsp\\b|\\bvar\\b", name_std),      "infraspecific_unresolved",
  n_words_tmp == 2 & total_occ < api_min_occurrences, "below_api_threshold",
  n_words_tmp == 2,                              "binomial_no_match",
  default =                                      "other"
)]

recon[, n_words_tmp := NULL]

# Report unmatched breakdown
cat_summary <- recon[match_tier == "unmatched", .(
  n = .N, occ = sum(as.numeric(total_occ))
), by = match_type][order(-occ)]

for (i in seq_len(nrow(cat_summary))) {
  cli_alert_info("  {cat_summary$match_type[i]}: {scales::comma(cat_summary$n[i])} species ({scales::comma(cat_summary$occ[i])} occ)")
}


# ============================================================================
# Enrich with Higher Taxonomy
# ============================================================================
# Join kingdom/phylum/class/order/family and threat status from the
# matched Dyntaxa accepted taxon, so 09b doesn't need to re-join.

cli_h2("Enriching with higher taxonomy from matched Dyntaxa taxa")

taxa_hier_cols <- intersect(
  c("taxonID", "taxonRank", "kingdom", "phylum", "class", "order", "family",
    "threatStatus_dyntaxa", "threatStatus_redlist"),
  names(accepted_taxa)
)

if (length(taxa_hier_cols) > 1) {
  taxa_hier <- accepted_taxa[, ..taxa_hier_cols]
  taxa_hier <- taxa_hier[, .SD[1], by = taxonID]

  recon <- merge(
    recon,
    taxa_hier,
    by.x = "dyntaxa_taxonID", by.y = "taxonID",
    all.x = TRUE
  )
  cli_alert_success("Added columns: {paste(setdiff(taxa_hier_cols, 'taxonID'), collapse = ', ')}")
} else {
  cli_alert_info("No higher taxonomy columns available to add")
}


# ============================================================================
# Save Outputs
# ============================================================================

cli_h2("Saving outputs")

# Reorder columns for readability
key_cols <- c("specieskey", "species", "total_occ",
              "dyntaxa_taxonID", "dyntaxa_scientificName",
              "match_tier", "match_type", "match_name_used")
other_cols <- setdiff(names(recon), c(key_cols, "name_std"))
setcolorder(recon, c(key_cols, other_cols))

# Primary output: RDS (fast to load in 09b)
saveRDS(recon, reconciliation_file)
cli_alert_success("Saved: {.path {reconciliation_file}}")

# CSV for the gap analysis pipeline (matches config)
match_table_path <- here(gaps_dir,
  cfg_get("gaps.outputs.taxonomic_match_table", "taxonomic_match_table.csv"))
fwrite(recon[, -"name_std"], match_table_path)
cli_alert_success("Saved: {.path {match_table_path}}")

# Tier summary CSV
tier_summary <- recon[, .(
  n_species       = .N,
  total_occ       = sum(as.numeric(total_occ)),
  pct_species     = round(100 * .N / n_gbif, 1),
  pct_occurrences = round(100 * sum(as.numeric(total_occ)) /
                            sum(as.numeric(gbif_species$total_occ)), 2)
), by = .(match_tier, match_type)][order(match_tier, -n_species)]

# Tier summary CSV (09a-specific; 09b writes the backbone-level match_summary)
match_summary_path <- here(gaps_dir, "taxonomic_reconciliation_summary.csv")
fwrite(tier_summary, match_summary_path)
cli_alert_success("Saved: {.path {match_summary_path}}")


# ============================================================================
# Final Report
# ============================================================================

cli_h1("Reconciliation Complete")

n_matched   <- sum(recon$match_tier != "unmatched")
n_unmatched <- sum(recon$match_tier == "unmatched")
occ_matched <- recon[match_tier != "unmatched", sum(as.numeric(total_occ))]
occ_total   <- sum(as.numeric(recon$total_occ))

cli_alert_success("Total GBIF species:   {scales::comma(n_gbif)}")
cli_alert_success("Matched to backbone:  {scales::comma(n_matched)} ({round(100 * n_matched / n_gbif, 1)}%)")
cli_alert_success("Occurrence coverage:  {round(100 * occ_matched / occ_total, 2)}%")
cli_alert_info("")

# Per-tier breakdown
tier_order <- c("tier1", "tier2", "tier2_ambiguous", "tier3", "tier4", "unmatched")
tier_top <- recon[, .(
  n   = .N,
  occ = sum(as.numeric(total_occ))
), by = match_tier]
tier_top[, tier_rank := match(match_tier, tier_order, nomatch = 99)]
setorder(tier_top, tier_rank)
tier_top[, tier_rank := NULL]

cli_alert_info(sprintf("  %-25s %8s %14s", "Tier", "Species", "Occurrences"))
cli_alert_info(strrep("-", 52))
for (i in seq_len(nrow(tier_top))) {
  cli_alert_info(sprintf("  %-25s %8s %14s",
                         tier_top$match_tier[i],
                         scales::comma(tier_top$n[i]),
                         scales::comma(tier_top$occ[i])))
}

# Warnings for incomplete Tier 4
if (!api_available && nrow(t4_candidates) > 0) {
  cli_alert_warning("")
  cli_alert_warning("Tier 4 skipped: {.pkg httr2} not installed.")
  cli_alert_warning("Install and rerun to resolve up to ~{scales::comma(nrow(t4_candidates))} more species.")
}

uncached <- nrow(to_query) - n_to_query
if (uncached > 0) {
  cli_alert_info("")
  cli_alert_info("{scales::comma(uncached)} species still need API queries (hit per-run cap).")
  cli_alert_info("Rerun this script to continue. Cache persists across runs.")
}

elapsed <- round(difftime(Sys.time(), timer_start, units = "mins"), 1)
cli_alert_info("")
cli_alert_info("Elapsed: {elapsed} minutes")
cli_alert_info("Next: source('scripts/09b_taxonomic_gaps.R')")
