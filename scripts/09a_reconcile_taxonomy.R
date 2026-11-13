# scripts/09a_reconcile_taxonomy.R
# ============================================================================
# Taxonomic Reconciliation: GBIF Species <-> National Taxonomy
# ============================================================================
# Purpose:
#   Build the best possible mapping between GBIF occurrence species and the
#   national taxonomy backbone. Uses a 5-tier strategy to maximise the match
#   rate before the gap analysis in 09b.
#
#   This script runs BEFORE 09b_taxonomic_gaps.R and produces a lookup
#   table that 09b reads instead of doing its own name matching.
#
# Matching tiers:
#   Tier 1 -- Direct accepted name match (local)
#   Tier 2 -- Synonym name match (local, via backbone synonym rows)
#   Tier 3 -- Infraspecific collapse (strip subsp/var, re-match locally)
#   Tier 4 -- COL synonyms of the cube key at species rank (GBIF Species API,
#             cached); several backbone taxa for one key -> tier4_ambiguous
#   Tier 5 -- COL crosswalk (key match via 09a1's col_crosswalk.rds; additive)
#
# Inputs:
#   - data/{CC}/proc/derived/by_order/species_summary/*_10km.csv  (GBIF species)
#   - data/{CC}/proc/derived/by_family/species_summary/*_10km.csv
#   - data/{CC}/proc/taxa_reference_current.rds                   (backbone)
#   - data/{CC}/proc/col_crosswalk.rds                            (09a1; optional)
#
# Outputs:
#   - data/{CC}/proc/taxonomic_reconciliation.rds       Main lookup table
#   - data/{CC}/proc/taxa_reference_classified.rds      Classified backbone (read by 09b)
#   - data/{CC}/proc/col_synonym_cache.rds              Cached COL synonym lookups (per
#                                                       synonym: canonical name, rank,
#                                                       authorship, taxonomic status)
#   - data/{CC}/proc/gaps/taxonomic_match_table.csv      Reconciliation as CSV
#   - data/{CC}/proc/gaps/taxonomic_reconciliation_summary.csv  Tier-level summary
#
# Dependencies: data.table, stringr, here, cli, httr2 (optional, for Tier 4)
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# %||% is defined in R/globals.R

timer_start <- Sys.time()


# ============================================================================
# Configuration
# ============================================================================

# Occurrence threshold: only query the GBIF API for unmatched species with
# at least this many occurrences. Species below this are likely vagrants
# or misidentifications that aren't in the national backbone anyway.
api_min_occurrences <- cfg_get("parameters.taxonomic.api_min_occurrences", 1)

# Maximum number of API requests per BATCH (safety valve against
# runaway API calls). The script loops over batches of this size
# until all candidates are queried (or api_max_batches is reached).
# The cache is persisted between batches so progress is never lost.
api_max_requests <- cfg_get("parameters.taxonomic.api_max_requests", 5000)

# Maximum number of batches to run in a single script execution.
# Default Inf means "keep going until all candidates are resolved".
# Set to a finite value (e.g. 1) for a quick partial run.
api_max_batches <- cfg_get("parameters.taxonomic.api_max_batches", Inf)

# GBIF API rate limit (requests per second, GBIF allows ~10)
api_rate_limit <- 10

# Cache file for COL synonym lookups (persists across reruns).
cache_file <- here(p_data_proc, "col_synonym_cache.rds")

# GBIF interprets occurrences against the Catalogue of Life (COL) backbone,
# so the cube's `specieskey` is a COL taxonID (alphanumeric, e.g. "6VFN8"), not
# an integer nub key. Tier 4 resolves synonyms within this COL checklist dataset;
# override per-country in config if GBIF's COL checklist key ever changes.
col_checklist_key <- get_col_checklist_key()

# Output paths (p_gaps defined in R/globals.R)
# Directory created by ensure_dirs() in 00_setup.R

reconciliation_file <- here(p_timepoint, "taxonomic_reconciliation.rds")

cli_h1("09a -- Taxonomic Reconciliation")


# ============================================================================
# Step 1: Load GBIF Species
# ============================================================================

cli_h2("Loading GBIF species from occurrence cubes")

sum_files <- list.files(
  p_derived,
  pattern = "species_summary.*10km\\.csv$",
  recursive = TRUE, full.names = TRUE
)

if (length(sum_files) == 0) {
  cli_abort(
    "No species_summary files found in {.path {p_derived}}. Run 06b first."
  )
}
cli_alert_info("Reading {length(sum_files)} species_summary files")

gbif_raw <- rbindlist(lapply(sum_files, fread,
  select = c("specieskey", "species", "basisofrecord", "occurrences", "class")
), fill = TRUE)

# specieskey is the cross-table join key. The rest of the pipeline standardises
# it as CHARACTER (read_cube() in R/globals.R, and the joins in 09b/09c), so 09a
# must emit character too. fread() infers specieskey as integer for some
# species_summary files and as character for others (e.g. a key beyond 32-bit
# int range, or a blank), and rbindlist() then unifies the whole column to
# character whenever ANY file tripped character inference -- a non-deterministic
# type that breaks BOTH the reconciliation schema and the integer-vs-character
# cube joins in 09b/09c. Pin it once here so the output type is stable
# regardless of what fread/rbindlist inferred.
gbif_raw[, specieskey := as.character(specieskey)]

# Aggregate: one row per specieskey with total occurrences.
# A species may appear in multiple files (by_order + by_family splits).
# Carry a representative class (first non-blank) so we can scope-filter the
# GBIF universe to the backbone before matching (see Step 2b).
gbif_species <- gbif_raw[
  basisofrecord == "all",
  .(total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
    class     = {
      v <- class[!is.na(class) & class != ""]
      if (length(v)) v[1L] else NA_character_
    }),
  by = .(specieskey, species)
]
setorder(gbif_species, -total_occ)

# Standardise names for matching
gbif_species[, name_std := tolower(trimws(species))]

n_gbif <- nrow(gbif_species)
cli_alert_success("GBIF species: {scales::comma(n_gbif)}")
cli_alert_info("Total occurrences: {scales::comma(sum(gbif_species$total_occ))}")


# ============================================================================
# Step 2: Load National Taxonomy (backbone)
# ============================================================================

cli_h2("Loading national taxonomy backbone")

taxa_file <- here(p_data_proc, "taxa_reference_current.rds")
if (!file.exists(taxa_file)) {
  cli_abort("Taxonomy reference not found: {.path {taxa_file}}. Run script 03 first.")
}

taxa <- as.data.table(readRDS(taxa_file))
taxa[, name_std := tolower(trimws(scientificName))]

# Classify rows: accepted vs synonym (NA-safe, uses taxonomicStatus first)
taxa <- classify_accepted(taxa)

# Persist the classified backbone so 09b consumes the SAME accepted/synonym
# split instead of re-loading taxa_reference_current.rds and re-running
# classify_accepted() -- removes the duplicate-load drift risk.
saveRDS(taxa, here(p_data_proc, "taxa_reference_classified.rds"))
cli_alert_success("Saved classified backbone: taxa_reference_classified.rds")

accepted_taxa <- taxa[is_accepted == TRUE]
synonym_taxa  <- taxa[is_accepted == FALSE]

accepted_names    <- unique(accepted_taxa$name_std)
all_backbone_names <- unique(taxa$name_std)

cli_alert_info("Backbone accepted: {scales::comma(nrow(accepted_taxa))}")
cli_alert_info("Backbone synonyms: {scales::comma(nrow(synonym_taxa))}")
cli_alert_info("Unique accepted names: {scales::comma(length(accepted_names))}")
cli_alert_info("Combined unique names: {scales::comma(length(all_backbone_names))}")


# ============================================================================
# Step 2b: Restrict GBIF to the backbone's taxonomic scope (pre-match)
# ============================================================================
# The GBIF species universe includes groups the (multicellular) national
# backbone never covers -- bacteria, archaea, many protists/algae. They can
# never match a backbone taxon, so they only deflate the match rate and inflate
# the "unmatched" set. We drop GBIF species whose CLASS is absent from the
# backbone (kingdom isn't in the species summaries, but class is, and a class
# the backbone has no taxa in cannot produce a match). NA/blank class is KEPT
# so legitimate taxa with a missing class still go through name matching.
# Self-calibrating: the allowed set is read from the backbone itself, so it
# tracks whatever the backbone actually covers. This narrows the reconciliation
# universe only. Toggle off via parameters.taxonomic.restrict_to_backbone_scope,
# the same switch that governs the kingdom scoping in 06a/06b/08.
if (isTRUE(cfg_get("parameters.taxonomic.restrict_to_backbone_scope", TRUE)) &&
    "class" %in% names(gbif_species)) {

  class_col_ref <- intersect(c("class", "Class"), names(accepted_taxa))[1]
  if (!is.na(class_col_ref)) {
    cli_h2("Step 2b: Restricting GBIF to backbone classes")

    # Build the allowed-class set from the MULTICELLULAR backbone only. The
    # national backbone (Dyntaxa) does carry some non-multicellular kingdoms
    # (Bacteria/Archaea/Viruses), so deriving allowed classes from the whole
    # backbone would silently re-admit them. Exclude those kingdoms' classes so
    # "backbone scope" means the multicellular scope. Config-gated; empty = off.
    exclude_kingdoms <- unlist(cfg_get("parameters.taxonomic.exclude_kingdoms", character(0)))
    acc_scope <- if (length(exclude_kingdoms) && "kingdom" %in% names(accepted_taxa)) {
      accepted_taxa[is.na(kingdom) | !(kingdom %in% exclude_kingdoms)]
    } else accepted_taxa

    allowed_classes <- unique(acc_scope[[class_col_ref]])
    allowed_classes <- allowed_classes[!is.na(allowed_classes) & allowed_classes != ""]

    # Safety net: never drop a GBIF species whose name is already a backbone
    # name, whatever its class string says. GBIF and the backbone occasionally
    # disagree on a species' class concept; without this, such a mismatch would
    # cut a species that WOULD have matched and flip its backbone taxon to
    # "missing" -- inflating the very gap we are trying to measure honestly.
    keep <- is.na(gbif_species$class) | gbif_species$class == "" |
            gbif_species$class %in% allowed_classes |
            gbif_species$name_std %in% all_backbone_names

    n_before   <- nrow(gbif_species)
    occ_before <- sum(as.numeric(gbif_species$total_occ))
    dropped    <- gbif_species[!keep]
    gbif_species <- gbif_species[keep]

    cli_alert_info("Backbone classes: {scales::comma(length(allowed_classes))}")
    cli_alert_success(
      "Kept {scales::comma(nrow(gbif_species))} / {scales::comma(n_before)} GBIF species \\
      ({scales::comma(n_before - nrow(gbif_species))} dropped as out-of-scope)"
    )

    if (nrow(dropped) > 0) {
      occ_dropped <- sum(as.numeric(dropped$total_occ))
      cli_alert_info(
        "Dropped occurrences: \\
         {scales::comma(occ_dropped)} ({round(100 * occ_dropped / occ_before, 2)}% of GBIF occ)"
      )
      drop_summary <- dropped[, .(n_species = .N, occ = sum(as.numeric(total_occ))),
                              by = class][order(-n_species)]
      for (i in seq_len(min(nrow(drop_summary), 15L))) {
        cli_alert_info(
          "  - {drop_summary$class[i] %||% 'NA'}: \\
           {scales::comma(drop_summary$n_species[i])} species, \\
           {scales::comma(drop_summary$occ[i])} occ"
        )
      }
      if (nrow(drop_summary) > 15L)
        cli_alert_info("  ... and {nrow(drop_summary) - 15L} more classes")
    }

    # Refresh the GBIF total used in downstream match-rate reporting.
    n_gbif <- nrow(gbif_species)
  } else {
    cli_alert_warning("Backbone has no 'class' column -- scope filter skipped")
  }
} else {
  cli_alert_info("Scope filter disabled or no class column -- using full GBIF universe")
}


# ============================================================================
# Step 3: Build Synonym Lookup
# ============================================================================
# For each synonym name in the backbone, resolve to its accepted taxon.
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
  cli_alert_warning(
    "Ambiguous synonyms (>1 accepted taxon): {scales::comma(length(ambiguous_syns))}"
  )
}


# ============================================================================
# Initialise the Reconciliation Table
# ============================================================================
# Every GBIF species gets exactly one row. We fill in the backbone match
# tier by tier. At the end, unmatched species get categorised.

recon <- gbif_species[, .(specieskey, species, name_std, total_occ)]
recon[, `:=`(
  backbone_taxonID        = NA_character_,
  backbone_scientificName = NA_character_,
  match_tier             = NA_character_,
  match_type             = NA_character_,
  match_name_used        = NA_character_
)]


# ============================================================================
# TIER 1: Direct Accepted Name Match
# ============================================================================
# Does the GBIF species name appear as an accepted backbone name?

cli_h2("Tier 1: Direct accepted name match")

t1_mask <- recon$name_std %in% accepted_names

# Get the taxonID for each match
t1_join <- merge(
  recon[t1_mask, .(specieskey, name_std)],
  accepted_taxa[, .(name_std, taxonID, scientificName)][, .SD[1], by = name_std],
  by = "name_std"
)

recon[t1_join, on = "specieskey",
      `:=`(backbone_taxonID        = i.taxonID,
           backbone_scientificName = i.scientificName,
           match_tier             = "tier1",
           match_type             = "accepted_name",
           match_name_used        = name_std)]

n_t1   <- sum(recon$match_tier == "tier1", na.rm = TRUE)
occ_t1 <- if (n_t1 > 0) recon[match_tier == "tier1", sum(as.numeric(total_occ))] else 0
cli_alert_success("Tier 1: {scales::comma(n_t1)} species ({scales::comma(occ_t1)} occ)")
cli_alert_info("  Remaining: {scales::comma(sum(is.na(recon$match_tier)))}")


# ============================================================================
# TIER 2: Backbone Synonym Match
# ============================================================================
# Does the GBIF species name appear as a SYNONYM in the backbone?
# If so, resolve to the accepted backbone taxon.

cli_h2("Tier 2: Backbone synonym match")

t2_pool <- recon[is.na(match_tier), .(specieskey, name_std)]

t2_join <- merge(
  t2_pool,
  syn_lookup_unique[, .(name_std, accepted_taxonID, accepted_name,
                        is_ambiguous)],
  by = "name_std"
)

recon[t2_join, on = "specieskey",
      `:=`(backbone_taxonID        = i.accepted_taxonID,
           backbone_scientificName = i.accepted_name,
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
    cli_alert_info(
      "    {top_t2$species[i]} -> \\
       {top_t2$backbone_scientificName[i]} ({scales::comma(top_t2$total_occ[i])} occ)"
    )
  }
}


# ============================================================================
# TIER 3: Infraspecific Collapse
# ============================================================================
# For names with 3+ words (or containing subsp./var./f.), extract the
# binomial (first two words) and try matching that against the backbone.

cli_h2("Tier 3: Infraspecific collapse to binomial")

t3_pool <- recon[is.na(match_tier), .(specieskey, name_std, total_occ)]
t3_pool[, n_words := sapply(strsplit(name_std, "\\s+"), length)]
t3_pool[, is_infra := n_words >= 3 |
          grepl("\\bsubsp\\b|\\bvar\\b|\\bf\\.", name_std)]

t3_candidates <- t3_pool[is_infra == TRUE]
n_t3 <- 0

if (nrow(t3_candidates) > 0) {
  t3_candidates[, binomial := paste(word(name_std, 1), word(name_std, 2))]
  t3_candidates[, binom_in_backbone := binomial %in% all_backbone_names]
  t3_hits <- t3_candidates[binom_in_backbone == TRUE]

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
            `:=`(backbone_taxonID        = i.taxonID,
                 backbone_scientificName = i.scientificName,
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
# TIER 4: COL synonym lookup (GBIF ChecklistBank)
# ============================================================================
# For remaining unmatched species above the occurrence threshold:
#   1. Resolve the cube's COL taxonID -> GBIF integer usage key (sourceId lookup)
#   2. Query that usage's synonyms within the COL checklist dataset
#   3. Keep the synonyms at species rank and check them against backbone names
#   4. Cache successful responses so reruns are instant
#
# Only a species-rank synonym names the same species as the cube key. An
# infraspecific synonym ("Cladonia uncialis var. bolacina") says nothing about
# its parent species ("Cladonia uncialis"), so it is not used for matching.
#
# The cube key is a COL taxonID (e.g. "6VFN8"), not an integer, so the classic
# GET /v1/species/{key}/synonyms 400s on it. COL-aware path instead:
#   GET /v1/species?datasetKey={COL}&sourceId={taxonID}  -> integer usage key
#   GET /v1/species/{usageKey}/synonyms                  -> synonym names

cli_h2("Tier 4: COL synonym lookup (GBIF ChecklistBank)")

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
# max_age_days = NULL on purpose: this cache is ~22k COL synonym lookups, so
# expiring it on age would silently turn a routine rebuild into an hours-long
# re-query. Refresh it deliberately with parameters.cache.force_refresh; its age
# is reported by scripts/00_preflight.R.
api_cache <- load_cache(cache_file, "COL synonym cache", max_age_days = NULL)

# A cache entry is usable when it records the rank of each synonym
# (`syn_ranks`). An entry without ranks holds names cut down to binomials, which
# turns an infraspecific synonym into its parent species, so it is queried
# again. An entry without any synonym is complete as it is and only gains the
# (empty) rank fields.
cache_has_ranks <- vapply(api_cache, function(e) !is.null(e$syn_ranks), logical(1))
cache_is_empty  <- vapply(api_cache, function(e) length(e$synonyms) == 0L, logical(1))
for (sk in names(api_cache)[!cache_has_ranks & cache_is_empty]) {
  api_cache[[sk]]$syn_names   <- character(0)
  api_cache[[sk]]$syn_ranks   <- character(0)
  api_cache[[sk]]$syn_authors <- character(0)
  api_cache[[sk]]$syn_status  <- character(0)
}
n_cache_no_ranks <- sum(!cache_has_ranks & !cache_is_empty)
if (n_cache_no_ranks > 0) {
  cli_alert_warning(
    "Cached entries without synonym ranks: {scales::comma(n_cache_no_ranks)} (queried again)"
  )
}

# Determine which specieskeys still need querying
t4_candidates[, sk_chr := as.character(specieskey)]
already_cached <- t4_candidates$sk_chr %in% names(api_cache)[cache_has_ranks | cache_is_empty]
remaining <- t4_candidates[!already_cached]

cli_alert_info("Already cached: {scales::comma(sum(already_cached))}")
cli_alert_info("Need API query: {scales::comma(nrow(remaining))}")

# --- Query the API in batches ---
api_available <- TRUE

if (nrow(remaining) > 0) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli_alert_warning("Package {.pkg httr2} not available -- skipping API queries")
    cli_alert_info("Install with: {.code install.packages('httr2')}")
    api_available <- FALSE
  }
}

# Track totals across all batches for final reporting
total_queried <- 0L
total_api_errors <- 0L
n_batches_run <- 0L

if (nrow(remaining) > 0 && api_available) {
  library(httr2)

  # Compute batch plan
  n_total_needed <- nrow(remaining)
  n_batches_needed <- ceiling(n_total_needed / api_max_requests)
  n_batches_to_run <- min(n_batches_needed, api_max_batches)

  est_total_minutes <- round(min(n_total_needed, n_batches_to_run * api_max_requests) /
                             api_rate_limit / 60, 1)
  cli_alert_info(
    "Batch plan: {n_batches_to_run} batch(es) \\
     of up to {scales::comma(api_max_requests)} queries (~{est_total_minutes} min total)"
  )
  if (n_batches_needed > api_max_batches) {
    cli_alert_warning(
      "Capped at {api_max_batches} batch(es) per run \\
       ({scales::comma(n_total_needed - n_batches_to_run * api_max_requests)} candidates \\
       will be picked up on next rerun)"
    )
  }

  # === Outer loop: batches ===
  while (nrow(remaining) > 0 && n_batches_run < api_max_batches) {
    n_batches_run <- n_batches_run + 1L
    batch_size <- min(nrow(remaining), api_max_requests)
    batch <- remaining[1:batch_size]

    cli_h3("Batch {n_batches_run}/{n_batches_to_run}: querying {scales::comma(batch_size)} species")

    pb <- cli_progress_bar(
      paste0("GBIF API batch ", n_batches_run),
      total = batch_size
    )
    n_batch_errors <- 0L

    # === Inner loop: queries within batch ===
    for (i in seq_len(batch_size)) {
      sk <- batch$sk_chr[i]

      tryCatch({
        # Step 1: resolve the COL taxonID (the cube's specieskey, e.g. "6VFN8")
        # to GBIF's integer usage key within the COL checklist dataset. The
        # /synonyms endpoint is keyed by that integer usage key, not the taxonID.
        src <- request("https://api.gbif.org/v1/species") |>
          req_url_query(datasetKey = col_checklist_key, sourceId = sk) |>
          req_retry(max_tries = 3, backoff = ~ 2) |>
          req_throttle(rate = api_rate_limit) |>
          req_perform()
        src_results <- resp_body_json(src)$results %||% list()

        # Canonical name (no authorship) and rank of every synonym: what the
        # matching uses. Authorship and taxonomic status are stored with them
        # for inspection (homonyms, misapplied names).
        syn_names   <- character(0)
        syn_ranks   <- character(0)
        syn_authors <- character(0)
        syn_status  <- character(0)

        if (length(src_results) > 0) {
          usage_key <- src_results[[1]]$key

          # Step 2: fetch this usage's synonyms from the COL checklist.
          resp <- request(paste0("https://api.gbif.org/v1/species/", usage_key, "/synonyms")) |>
            req_url_query(limit = 200) |>
            req_retry(max_tries = 3, backoff = ~ 2) |>
            req_throttle(rate = api_rate_limit) |>
            req_perform()
          results <- resp_body_json(resp)$results %||% list()

          for (r in results) {
            cn <- tolower(trimws(r$canonicalName %||% ""))
            if (nzchar(cn)) {
              syn_names   <- c(syn_names, cn)
              syn_ranks   <- c(syn_ranks, toupper(r$rank %||% ""))
              syn_authors <- c(syn_authors, r$authorship %||% "")
              syn_status  <- c(syn_status, toupper(r$taxonomicStatus %||% ""))
            }
          }
        }

        # Cache the (possibly empty) result. Empty is a VALID answer here (no COL
        # usage for this key, or a taxon with no synonyms) and is cached so reruns
        # skip it. Transport errors are handled below and deliberately NOT cached.
        # `synonyms` holds the names used for matching: species rank only.
        api_cache[[sk]] <- list(
          synonyms    = unique(syn_names[syn_ranks == "SPECIES"]),
          syn_names   = syn_names,
          syn_ranks   = syn_ranks,
          syn_authors = syn_authors,
          syn_status  = syn_status,
          queried_at  = Sys.time()
        )
      },
      error = function(e) {
        # Do NOT persist transport errors as negative cache entries: a cached
        # failure is never retried, so one bad run would permanently suppress
        # those lookups. Left uncached, `already_cached` stays FALSE for them
        # and they retry on the next run.
        n_batch_errors <<- n_batch_errors + 1L
      })

      cli_progress_update(id = pb)

      # Save cache periodically (every 500 queries) in case of crash
      if (i %% 500 == 0) {
        saveRDS(api_cache, cache_file)
      }
    }

    cli_progress_done(id = pb)

    # Persist cache at end of each batch
    saveRDS(api_cache, cache_file)

    # Update totals and remaining
    total_queried <- total_queried + batch_size
    total_api_errors <- total_api_errors + n_batch_errors

    cli_alert_success(
      "Batch {n_batches_run} complete: {scales::comma(batch_size)} queried",
      " ({n_batch_errors} errors, cache: {scales::comma(length(api_cache))} total)"
    )

    # Drop the rows we just attempted (cached OR errored) so the batch loop always
    # makes progress even when some keys error. Uncached transport errors are
    # picked up again on the NEXT run via the cache-membership check above --
    # retried, never permanently lost, and never spinning forever within a run.
    remaining <- remaining[-seq_len(batch_size)]
  }

  if (total_api_errors > 0) {
    cli_alert_warning("Total API errors across all batches: {total_api_errors} / {total_queried}")
  }

  cli_alert_success(
    "Tier 4 API phase complete: {n_batches_run} batch(es), \\
     {scales::comma(total_queried)} queries, \\
     cache: {scales::comma(length(api_cache))} entries"
  )
}

# --- Resolve cached synonyms to backbone ---
# Every species-rank synonym that is a backbone name is a hit, and each hit is
# resolved to its accepted backbone taxon. A hit on an accepted name ranks
# before a hit on a backbone synonym; within each, the API order decides. A key
# whose hits lead to more than one backbone taxon keeps the first and is marked
# tier4_ambiguous.
n_t4       <- 0
n_t4_ambig <- 0
occ_t4     <- 0

t4_usable <- t4_candidates$sk_chr[
  vapply(api_cache[t4_candidates$sk_chr], function(e) !is.null(e$syn_ranks), logical(1))
]
n_t4_waiting <- nrow(t4_candidates) - length(t4_usable)

if (length(t4_usable) > 0) {
  t4_syn <- lapply(api_cache[t4_usable], function(e) e$synonyms)
  syn_long <- data.table(
    specieskey = rep(t4_usable, lengths(t4_syn)),
    name_std   = as.character(unlist(t4_syn, use.names = FALSE))
  )
  syn_long[, syn_order := seq_len(.N), by = specieskey]

  rank_tab <- sort(table(unlist(lapply(api_cache[t4_usable], function(e) e$syn_ranks),
                                use.names = FALSE)), decreasing = TRUE)
  if (length(rank_tab) > 0) {
    cli_alert_info(
      "  Cached synonyms by rank: \\
       {paste0(names(rank_tab), ' ', scales::comma(as.integer(rank_tab)), collapse = ', ')}"
    )
  }

  status_tab <- sort(table(unlist(lapply(api_cache[t4_usable], function(e) e$syn_status),
                                  use.names = FALSE)), decreasing = TRUE)
  if (length(status_tab) > 0) {
    cli_alert_info(
      "  Cached synonyms by status: \\
       {paste0(names(status_tab), ' ', scales::comma(as.integer(status_tab)), collapse = ', ')}"
    )
  }

  acc_lookup <- accepted_taxa[, .(name_std, taxonID, scientificName)][, .SD[1], by = name_std]
  hits_acc <- merge(syn_long, acc_lookup, by = "name_std")
  hits_acc[, hit_rank := 1L]
  hits_syn <- merge(
    syn_long[!name_std %in% acc_lookup$name_std],
    syn_lookup_unique[, .(name_std,
                          taxonID = accepted_taxonID,
                          scientificName = accepted_name)],
    by = "name_std"
  )
  hits_syn[, hit_rank := 2L]

  t4_hits <- rbind(hits_acc, hits_syn, use.names = TRUE)[!is.na(taxonID)]

  if (nrow(t4_hits) > 0) {
    setorder(t4_hits, specieskey, hit_rank, syn_order)
    t4_hits[, n_taxa := uniqueN(taxonID), by = specieskey]
    t4_resolved <- t4_hits[, .SD[1L], by = specieskey]

    recon[t4_resolved, on = "specieskey",
          `:=`(backbone_taxonID        = i.taxonID,
               backbone_scientificName = i.scientificName,
               match_tier             = fifelse(i.n_taxa > 1L, "tier4_ambiguous", "tier4"),
               match_type             = "gbif_api_synonym",
               match_name_used        = i.name_std)]

    n_t4       <- nrow(t4_resolved)
    n_t4_ambig <- sum(t4_resolved$n_taxa > 1L)
    occ_t4     <- recon[match_tier %in% c("tier4", "tier4_ambiguous"),
                        sum(as.numeric(total_occ))]
  }
}

cli_alert_success("Tier 4: {scales::comma(n_t4)} species ({scales::comma(occ_t4)} occ)")
if (n_t4_ambig > 0) {
  cli_alert_warning(
    "  Of which {scales::comma(n_t4_ambig)} ambiguous (synonyms lead to >1 backbone taxon)"
  )
}
if (n_t4_waiting > 0) {
  cli_alert_warning(
    "  Candidates without a usable cache entry: {scales::comma(n_t4_waiting)} \\
     (not matched in Tier 4 until queried)"
  )
}
cli_alert_info("  Remaining: {scales::comma(sum(is.na(recon$match_tier)))}")


# ============================================================================
# TIER 5: COL crosswalk (Dyntaxa <-> COL taxonID authority)
# ============================================================================
# For species still unmatched after Tiers 1-4, use the authoritative
# Dyntaxa->COL crosswalk (built by 09a1 via GBIF's v2 match API). The cube's
# specieskey IS a COL taxonID, so a cube key that equals a Dyntaxa taxon's COL
# key IS that taxon -- a key-based match that resolves the COL name-divergence
# (e.g. Sylvia -> Curruca) that name matching (Tiers 1-3) and the COL synonym
# lookup (Tier 4) can miss. ADDITIVE: only fills rows Tiers 1-4 left NA, so the
# Tier 1-4 assignments (and the Tier-4 count) are preserved exactly. Skipped
# gracefully if the crosswalk has not been built, so 09a still runs standalone.

cli_h2("Tier 5: COL crosswalk (Dyntaxa <-> COL key)")

crosswalk_file <- here(p_data_proc, "col_crosswalk.rds")
n_t5 <- 0L
if (file.exists(crosswalk_file)) {
  crosswalk <- as.data.table(readRDS(crosswalk_file))
  crosswalk <- crosswalk[!is.na(col_key) & nzchar(col_key)]

  # One accepted taxon per COL key: prefer an accepted-name link over a synonym
  # link, then higher confidence. (A COL "lump" -- two Dyntaxa taxa sharing one
  # COL key -- is rare here, ~1%; picking the best-supported link keeps the
  # GBIF-species-centric reconciliation at one taxon per cube key.)
  if (!"via_synonym" %in% names(crosswalk)) crosswalk[, via_synonym := FALSE]
  if (!"confidence"  %in% names(crosswalk)) crosswalk[, confidence  := NA_integer_]
  setorder(crosswalk, col_key, via_synonym, -confidence)
  cw1 <- crosswalk[, .SD[1], by = col_key,
                   .SDcols = c("dyntaxa_taxonID", "name_used")]

  t5_join <- merge(
    recon[is.na(match_tier), .(specieskey)],
    cw1, by.x = "specieskey", by.y = "col_key"
  )
  # Resolve the accepted backbone scientific name for display.
  t5_join <- merge(
    t5_join,
    accepted_taxa[, .(taxonID, scientificName)][, .SD[1], by = taxonID],
    by.x = "dyntaxa_taxonID", by.y = "taxonID", all.x = TRUE
  )

  if (nrow(t5_join) > 0) {
    recon[t5_join, on = "specieskey",
          `:=`(backbone_taxonID        = i.dyntaxa_taxonID,
               backbone_scientificName = i.scientificName,
               match_tier             = "tier5_col",
               match_type             = "col_crosswalk",
               match_name_used        = i.name_used)]
    n_t5 <- nrow(t5_join)
  }
} else {
  cli_alert_warning(
    "col_crosswalk.rds not found -- Tier 5 skipped (run 09a1 to build it). \\
     Additive tier; Tiers 1-4 are unaffected."
  )
}

occ_t5 <- if (n_t5 > 0) recon[match_tier == "tier5_col", sum(as.numeric(total_occ))] else 0
cli_alert_success("Tier 5: {scales::comma(n_t5)} species ({scales::comma(occ_t5)} occ)")
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
  cli_alert_info(
    "  {cat_summary$match_type[i]}: {scales::comma(cat_summary$n[i])} species \\
     ({scales::comma(cat_summary$occ[i])} occ)"
  )
}


# ============================================================================
# Enrich with Higher Taxonomy
# ============================================================================
# Join kingdom/phylum/class/order/family and threat status from the
# matched backbone accepted taxon, so 09b doesn't need to re-join.

cli_h2("Enriching with higher taxonomy from matched backbone taxa")

taxa_hier_cols <- intersect(
  c("taxonID", "taxonRank", "kingdom", "phylum", "class", "order", "family",
    "threatStatus_backbone", "threatStatus_redlist",
    "is_invasive", "is_sensitive", "sensitivity_category", "in_dyntaxa",
    "establishmentMeans", "occurrenceStatus"),
  names(accepted_taxa)
)

if (length(taxa_hier_cols) > 1) {
  taxa_hier <- accepted_taxa[, ..taxa_hier_cols]
  taxa_hier <- taxa_hier[, .SD[1], by = taxonID]

  recon <- merge(
    recon,
    taxa_hier,
    by.x = "backbone_taxonID", by.y = "taxonID",
    all.x = TRUE
  )
  cli_alert_success("Added columns: {paste(setdiff(taxa_hier_cols, 'taxonID'), collapse = ', ')}")
} else {
  cli_alert_info("No higher taxonomy columns available to add")
}

# Set in_dyntaxa: TRUE for matched species, FALSE for unmatched
# (The flag from the backbone is TRUE for all backbone taxa; here we also need
#  to set it for GBIF species based on whether they matched the backbone.)
if ("in_dyntaxa" %in% names(recon)) {
  recon[match_tier == "unmatched", in_dyntaxa := FALSE]
  recon[is.na(in_dyntaxa), in_dyntaxa := FALSE]
} else {
  recon[, in_dyntaxa := match_tier != "unmatched"]
}

# Ensure is_invasive column exists (FALSE for unmatched/non-invasive)
if (!"is_invasive" %in% names(recon)) {
  recon[, is_invasive := FALSE]
} else {
  recon[is.na(is_invasive), is_invasive := FALSE]
}

# Ensure is_sensitive column exists (FALSE for unmatched/non-sensitive)
if (!"is_sensitive" %in% names(recon)) {
  recon[, is_sensitive := FALSE]
} else {
  recon[is.na(is_sensitive), is_sensitive := FALSE]
}

n_in_dyntaxa <- sum(recon$in_dyntaxa)
n_invasive   <- sum(recon$is_invasive)
n_sensitive  <- sum(recon$is_sensitive)
cli_alert_info("in_dyntaxa: {scales::comma(n_in_dyntaxa)} / {scales::comma(nrow(recon))}")
cli_alert_info("is_invasive: {scales::comma(n_invasive)} species flagged")
cli_alert_info("is_sensitive: {scales::comma(n_sensitive)} species flagged")


# ============================================================================
# Save Outputs
# ============================================================================

cli_h2("Saving outputs")

# Validate schema before saving
if (exists("validate_reconciliation")) {
  validate_reconciliation(recon)
}

# Reorder columns for readability
key_cols <- c("specieskey", "species", "total_occ",
              "backbone_taxonID", "backbone_scientificName",
              "match_tier", "match_type", "match_name_used")
other_cols <- setdiff(names(recon), c(key_cols, "name_std"))
setcolorder(recon, c(key_cols, other_cols))

# Primary output: RDS (fast to load in 09b)
saveRDS(recon, reconciliation_file)
cli_alert_success("Saved: {.path {reconciliation_file}}")

# CSV for the gap analysis pipeline (matches config)
match_table_path <- here(p_gaps,
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
match_summary_path <- here(p_gaps, "taxonomic_reconciliation_summary.csv")
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
cli_alert_success(
  "Matched to backbone:  {scales::comma(n_matched)} \\
   ({round(100 * n_matched / n_gbif, 1)}%)"
)
cli_alert_success("Occurrence coverage:  {round(100 * occ_matched / occ_total, 2)}%")
cli_alert_info("")

# Per-tier breakdown
tier_order <- c("tier1", "tier2", "tier2_ambiguous", "tier3", "tier4", "tier4_ambiguous",
                "tier5_col", "unmatched")
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
  cli_alert_warning(
    "Install and rerun to resolve up to ~{scales::comma(nrow(t4_candidates))} more species."
  )
}

# Did we hit the batch cap before resolving everything?
if (api_available && nrow(remaining) > 0) {
  cli_alert_info("")
  cli_alert_info(
    "{scales::comma(nrow(remaining))} species still need API queries \\
     (hit api_max_batches = {api_max_batches})."
  )
  cli_alert_info("Rerun this script to continue. Cache persists across runs.")
}

elapsed <- round(difftime(Sys.time(), timer_start, units = "mins"), 1)
cli_alert_info("")
cli_alert_info("Elapsed: {elapsed} minutes")
cli_alert_info("Next: source('scripts/09b_taxonomic_gaps.R')")
