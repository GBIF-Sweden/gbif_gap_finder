# R/closure.R
# ============================================================================
# Gap-closure computation between two time points
# ============================================================================
# Purpose:
#   The arithmetic behind scripts/14_gap_closure.R, kept separate from it so it
#   can be tested without arrow, without the real cubes and without a pipeline
#   run. Every function here takes data.tables and returns data.tables; nothing
#   reads a file or knows a path. tools/test_closure.R exercises all of it
#   against hand-built fixtures whose right answers are obvious by inspection.
#
# THE GRAIN, and why everything starts from it:
#   A "grain" table is one row per (key, eeacellcode, datasetkey) for one time
#   point, carrying min_year, occurrences and n_records. It is the coarsest
#   summary that still answers all five panels:
#     - species x cell pairs      -> unique(key, eeacellcode)
#     - cell occupancy            -> aggregate over key + datasetkey
#     - mechanism                 -> min_year of a gained pair
#     - source split              -> datasetkey -> source_group
#   Reducing the 42 M-row cube to a ~6.5 M-row grain once, and differencing
#   grains, is what keeps this cheap.
#
# TWO KEY SPACES, deliberately (see brief-historical-comparable-current-cube.md):
#   dyntaxa - key = backbone_taxonID. THE HEADLINE. A national checklist does
#             not move when GBIF changes its own backbone, so this is the only
#             space in which a snapshot and a COL-interpreted cube can be
#             differenced at all.
#   gbif    - key = specieskey. Not comparable across the snapshot -> cube
#             boundary, but it is what the Step 0 probe measured, so it is the
#             reconciliation that proves this code correct. It is also the
#             honest "all of GBIF" figure, since ~18% of species carry no
#             Dyntaxa taxonID.
#   Every output carries `key_space`. Never mix them in one number.
#
# PAIRS THAT SPAN SOURCE GROUPS are counted in BOTH per-group figures and ONCE
#   in the total, so per-group columns do not sum to the total. That is
#   deliberate. The alternative - crediting each pair to a single "owner" group
#   - needs a tie-break, and in Step 0 the tie-break alone moved Artportalen's
#   share by 1.2 points (82.6% against a true 81.4%). Order-independent measures
#   only.
#
# Dependencies: data.table, cli
# ============================================================================

CLOSURE_CELL_STATUS <- c("filled", "deepened", "unchanged", "thinned", "regressed")
CLOSURE_MECHANISMS  <- c("fieldwork", "digitisation", "unattributable")

# ============================================================================
# Source groups
# ============================================================================

#' Classify dataset keys into source groups
#'
#' The membership list is configuration, not code: it is a national fact that
#' changes when a country's observation platforms change, and hard-coding one
#' dataset key would not survive a second country. Anything not listed is
#' "collections" - the residual is the museum/institutional stream.
#'
#' @param datasetkey Character vector of GBIF dataset UUIDs.
#' @param platforms  Character vector of dataset keys counted as observation
#'   platforms (config: parameters.closure.source_groups.observation_platforms).
#' @return Character vector, "observation_platforms" or "collections".
closure_source_group <- function(datasetkey, platforms) {
  ifelse(as.character(datasetkey) %chin% as.character(platforms),
         "observation_platforms", "collections")
}

# ============================================================================
# Building the grain
# ============================================================================

#' Reduce raw cube rows to the closure grain
#'
#' One row per (key, eeacellcode, datasetkey) with min_year, occurrences and a
#' record count.
#'
#' TYPE CONSISTENCY IS THE WHOLE POINT OF THIS FUNCTION EXISTING.
#' `min(year, na.rm = TRUE)` returns an INTEGER for a group that has any year,
#' and `Inf` - a DOUBLE - for a group where every record lacks one. data.table
#' requires one type across all groups, so the moment the first year-less pair
#' appears the aggregation dies with "Column 1 of result for group N is type
#' 'double' but expecting type 'integer'". On the real 2021 cube that was group
#' 289 of ~5.3 million.
#'
#' Coercing `year` and `occurrences` to double BEFORE the aggregation fixes it
#' and is also faster than coercing inside `j`: data.table's GForce optimisation
#' applies to `min(x, na.rm = TRUE)` on a plain column but not to
#' `min(as.numeric(x), ...)`.
#'
#' Inf is then mapped to NA: "no year" is the unattributable bucket, not a year,
#' and it must not sort as one.
#'
#' @param dt data.table with specieskey, eeacellcode, datasetkey, year,
#'   occurrences. Modified by reference - pass a copy if that matters.
#' @param platforms Dataset keys counted as observation platforms.
#' @return data.table(key, eeacellcode, datasetkey, source_group, min_year,
#'   occ, n_rec)
closure_build_grain <- function(dt, platforms) {
  need <- c("specieskey", "eeacellcode", "datasetkey", "year", "occurrences")
  missing <- setdiff(need, names(dt))
  if (length(missing)) {
    cli_abort("closure_build_grain(): missing column{?s} {paste(missing, collapse = ', ')}")
  }
  dt <- data.table::as.data.table(dt)
  dt[, year        := as.numeric(year)]
  dt[, occurrences := as.numeric(occurrences)]
  g <- dt[, .(min_year = suppressWarnings(min(year, na.rm = TRUE)),
              occ      = sum(occurrences, na.rm = TRUE),
              n_rec    = .N),
          by = .(key = as.character(specieskey),
                 eeacellcode = as.character(eeacellcode),
                 datasetkey  = as.character(datasetkey))]
  g[!is.finite(min_year), min_year := NA_real_]
  g[, source_group := closure_source_group(datasetkey, platforms)]
  g[]
}

#' Re-key a grain from GBIF specieskey into another key space
#'
#' `closure_build_grain()` emits `key` holding the GBIF specieskey. In "gbif"
#' space that is already the key and this is a no-op. In "dyntaxa" space each
#' specieskey is mapped to its national-checklist taxonID, and species with no
#' taxonID are DROPPED and counted, not silently carried.
#'
#' Several specieskeys collapsing onto one taxonID is expected and wanted - that
#' is synonyms resolving. The reverse would be a defect, so the lookup is forced
#' unique on specieskey first: a duplicated specieskey would fan the grain out
#' and inflate every occurrence count downstream with no error anywhere.
#'
#' @param grain Output of closure_build_grain().
#' @param lookup data.table(specieskey, backbone_taxonID); ignored for "gbif".
#' @param key_space "gbif" or "dyntaxa".
#' @return list(grain, n_dropped)
closure_rekey <- function(grain, lookup, key_space = c("gbif", "dyntaxa")) {
  key_space <- match.arg(key_space)
  if (identical(key_space, "gbif")) return(list(grain = grain, n_dropped = 0L))

  need <- c("specieskey", "backbone_taxonID")
  if (length(setdiff(need, names(lookup)))) {
    cli_abort("closure_rekey(): lookup needs {paste(need, collapse = ' and ')}")
  }
  lk <- lookup[!is.na(backbone_taxonID) & nzchar(as.character(backbone_taxonID)),
               .(key = as.character(specieskey),
                 .taxon_key = as.character(backbone_taxonID))]
  lk <- unique(lk, by = "key")          # fan-out guard, see above

  before <- data.table::uniqueN(grain$key)
  g <- merge(grain, lk, by = "key")
  after  <- data.table::uniqueN(g$key)
  g[, key := .taxon_key][, .taxon_key := NULL]
  list(grain = g[, .(key, eeacellcode, datasetkey, source_group,
                     min_year, occ, n_rec)],
       n_dropped = before - after)
}

# ============================================================================
# Grain -> the derived views
# ============================================================================

#' Reduce a grain to distinct (key, cell) pairs
closure_pairs <- function(grain) {
  unique(grain[!is.na(key) & !is.na(eeacellcode), .(key, eeacellcode)])
}

#' Cell-level occupancy from a grain
#'
#' @param grain data.table(key, eeacellcode, datasetkey, min_year, occ, n_rec)
#' @return data.table(eeacellcode, occ, n_keys)
closure_cell_view <- function(grain) {
  grain[!is.na(eeacellcode),
        .(occ = sum(as.numeric(occ), na.rm = TRUE),
          n_keys = data.table::uniqueN(key)),
        by = eeacellcode]
}

#' Species-level occupancy from a grain
closure_key_view <- function(grain) {
  grain[!is.na(key),
        .(occ = sum(as.numeric(occ), na.rm = TRUE),
          n_cells = data.table::uniqueN(eeacellcode)),
        by = key]
}

# ============================================================================
# Cell closure
# ============================================================================

#' Cell-level closure between two time points, over the FULL grid universe
#'
#' `all_cellcodes` is required and must come from the grid file, never from the
#' data: taking the universe from the cells that happen to appear collapses the
#' denominator onto the numerator and reports 100% coverage with zero empty
#' cells. That exact bug is why `complete_to_grid()` in R/globals.R is written
#' the way it is; the same rule applies here.
#'
#' @param cells_from,cells_to Output of closure_cell_view().
#' @param all_cellcodes Character vector of EVERY cell in the grid.
#' @return data.table(eeacellcode, occ_from, occ_to, n_species_from,
#'   n_species_to, delta_occ, delta_species, status)
closure_cells <- function(cells_from, cells_to, all_cellcodes) {
  if (!length(all_cellcodes)) {
    cli_abort("closure_cells(): all_cellcodes is required and must be non-empty")
  }
  out <- data.table::data.table(eeacellcode = unique(as.character(all_cellcodes)))
  out <- merge(out, cells_from[, .(eeacellcode, occ_from = occ,
                                   n_species_from = n_keys)],
               by = "eeacellcode", all.x = TRUE)
  out <- merge(out, cells_to[, .(eeacellcode, occ_to = occ,
                                 n_species_to = n_keys)],
               by = "eeacellcode", all.x = TRUE)
  for (v in c("occ_from", "occ_to", "n_species_from", "n_species_to")) {
    data.table::set(out, which(is.na(out[[v]])), v, 0)
  }
  out[, delta_occ     := occ_to - occ_from]
  out[, delta_species := n_species_to - n_species_from]
  out[, status := data.table::fcase(
    occ_from == 0 & occ_to >  0, "filled",
    occ_from >  0 & occ_to == 0, "regressed",
    occ_to   >  occ_from,        "deepened",
    occ_to   <  occ_from,        "thinned",
    default = "unchanged"
  )]
  data.table::setorder(out, eeacellcode)
  out[]
}

# ============================================================================
# Species closure
# ============================================================================

#' Species-level closure: how many cells each key occupies, then and now
closure_species <- function(pairs_from, pairs_to) {
  a <- pairs_from[, .(cells_from = .N), by = key]
  b <- pairs_to[,   .(cells_to   = .N), by = key]
  out <- merge(a, b, by = "key", all = TRUE)
  out[is.na(cells_from), cells_from := 0L]
  out[is.na(cells_to),   cells_to   := 0L]

  # Gained/lost cells per key must be computed on the SETS, not the counts: a
  # species that lost one cell and gained another has delta 0 but is not
  # unchanged, and a "where to go next" list that hides that is wrong.
  g <- data.table::fsetdiff(pairs_to, pairs_from)[, .(cells_gained = .N), by = key]
  l <- data.table::fsetdiff(pairs_from, pairs_to)[, .(cells_lost   = .N), by = key]
  out <- merge(out, g, by = "key", all.x = TRUE)
  out <- merge(out, l, by = "key", all.x = TRUE)
  out[is.na(cells_gained), cells_gained := 0L]
  out[is.na(cells_lost),   cells_lost   := 0L]

  out[, status := data.table::fcase(
    cells_from == 0 & cells_to >  0, "new",
    cells_from >  0 & cells_to == 0, "lost",
    cells_gained >  0 & cells_lost == 0, "expanded",
    cells_gained == 0 & cells_lost >  0, "contracted",
    cells_gained >  0 & cells_lost >  0, "shifted",
    default = "unchanged"
  )]
  data.table::setorder(out, -cells_gained, key)
  out[]
}

# ============================================================================
# Mechanism
# ============================================================================

#' Split gained pairs into fieldwork / digitisation / unattributable
#'
#' The rule, and the thing that has to be said in the tab rather than a methods
#' note: a record dated >= the baseline year CANNOT have been in the baseline
#' snapshot, while an earlier record had to both exist AND be unpublished at the
#' cut. The boundary is therefore definitional, not empirical, and it creates a
#' step in the year histogram that is not a surge in fieldwork. Step 0 measured
#' it: 35,601 gained pairs at min-year 2020 against 197,806 at 2021.
#'
#' Mechanism is computed WITHIN each source group, not once per pair, because
#' "was this fieldwork?" is a question about a stream. The `total` group uses the
#' pair's overall minimum year.
#'
#' @param gained data.table(key, eeacellcode) - the gained pairs.
#' @param grain_to The later time point's grain, carrying source_group.
#' @param baseline_year Integer; the earlier time point's year.
#' @return data.table(source_group, mechanism, n_pairs, n_occurrences,
#'   n_records, n_cells, n_species)
closure_mechanism <- function(gained, grain_to, baseline_year) {
  if (!nrow(gained)) {
    return(data.table::data.table(
      source_group = character(), mechanism = character(), n_pairs = integer(),
      n_occurrences = numeric(), n_records = numeric(),
      n_cells = integer(), n_species = integer()))
  }
  g <- merge(grain_to, gained, by = c("key", "eeacellcode"))
  # Same integer/Inf trap as in closure_build_grain(): if a caller hands us an
  # integer min_year, the first all-NA group returns Inf and the aggregation
  # dies on a type mismatch. Coerce once, here, rather than trust the caller.
  g[, min_year := as.numeric(min_year)]

  per_group <- function(dt, label) {
    if (!nrow(dt)) return(NULL)
    p <- dt[, .(min_year = suppressWarnings(min(min_year, na.rm = TRUE)),
                occ = sum(as.numeric(occ), na.rm = TRUE),
                n_rec = sum(as.numeric(n_rec), na.rm = TRUE)),
            by = .(key, eeacellcode)]
    # An all-NA group yields Inf from min(na.rm = TRUE); that is the
    # unattributable bucket, not a number.
    p[!is.finite(min_year), min_year := NA_real_]
    p[, mechanism := data.table::fcase(
      is.na(min_year),             "unattributable",
      min_year >= baseline_year,   "fieldwork",
      default = "digitisation"
    )]
    p[, .(source_group = label, n_pairs = .N,
          n_occurrences = sum(occ), n_records = sum(n_rec),
          n_cells = data.table::uniqueN(eeacellcode),
          n_species = data.table::uniqueN(key)),
      by = mechanism]
  }

  out <- list(per_group(g, "total"))
  for (grp in sort(unique(g$source_group))) {
    out[[length(out) + 1L]] <- per_group(g[source_group == grp], grp)
  }
  out <- data.table::rbindlist(out, fill = TRUE)
  data.table::setcolorder(out, c("source_group", "mechanism"))
  data.table::setorder(out, source_group, mechanism)
  out[]
}

# ============================================================================
# Source concentration
# ============================================================================

#' Per-dataset concentration of the gain, without a tie-break
#'
#' `touched` counts gained pairs a dataset contributes any record to.
#' `vanish_if_dropped` counts gained pairs whose records ALL come from that one
#' dataset - i.e. what disappears if the dataset is removed. Both are
#' order-independent, which the single-owner alternative is not.
closure_dataset_concentration <- function(gained, grain_to) {
  if (!nrow(gained)) {
    return(data.table::data.table(datasetkey = character(), source_group = character(),
                                  touched = integer(), vanish_if_dropped = integer(),
                                  touched_pct = numeric(), vanish_pct = numeric()))
  }
  g <- merge(grain_to, gained, by = c("key", "eeacellcode"))
  n_gained <- nrow(gained)

  touched <- unique(g[, .(key, eeacellcode, datasetkey, source_group)])[
    , .(touched = .N), by = .(datasetkey, source_group)]

  n_ds <- unique(g[, .(key, eeacellcode, datasetkey)])[
    , .(n_ds = .N), by = .(key, eeacellcode)]
  sole <- merge(unique(g[, .(key, eeacellcode, datasetkey)]),
                n_ds[n_ds == 1L, .(key, eeacellcode)],
                by = c("key", "eeacellcode"))[, .(vanish_if_dropped = .N), by = datasetkey]

  out <- merge(touched, sole, by = "datasetkey", all.x = TRUE)
  out[is.na(vanish_if_dropped), vanish_if_dropped := 0L]
  out[, touched_pct := round(100 * touched / n_gained, 3)]
  out[, vanish_pct  := round(100 * vanish_if_dropped / n_gained, 3)]
  data.table::setorder(out, -touched)
  out[]
}

# ============================================================================
# Frozen thresholds
# ============================================================================

#' Under-sampled cells, judged against the BASELINE's thresholds
#'
#' `gap_zero` is absolute and means the same thing at two dates. `gap_low_q10`
#' does not: script 07 recomputes quantiles from whatever data it is given, so
#' comparing its flag across dates confounds real change with distribution
#' shift - a cell can be flagged newly under-sampled while its record count
#' rose. Freezing the baseline's thresholds and applying them forward is the
#' only version of this metric that means anything in a difference.
#'
#' @param cells Output of closure_cells().
#' @param q Quantile (default 0.10).
#' @return The input with `q_frozen`, `low_from`, `low_to` and `low_status`.
closure_frozen_low <- function(cells, q = 0.10) {
  nz <- cells[occ_from > 0, occ_from]
  thr <- if (length(nz)) as.numeric(stats::quantile(nz, probs = q, names = FALSE)) else NA_real_
  out <- data.table::copy(cells)
  out[, q_frozen := thr]
  out[, low_from := occ_from > 0 & occ_from <= thr]
  out[, low_to   := occ_to   > 0 & occ_to   <= thr]
  out[, low_status := data.table::fcase(
    low_from & !low_to,  "improved",
    !low_from & low_to,  "worsened",
    low_from & low_to,   "still_low",
    default = "not_low"
  )]
  out[]
}

# ============================================================================
# Summary
# ============================================================================

#' The tile numbers, long format
#'
#' @return data.table(metric, source_group, key_space, value)
closure_summary <- function(pairs_from, pairs_to, cells, species, mechanism,
                            key_space, baseline_year, regime_boundary = FALSE) {
  gained <- data.table::fsetdiff(pairs_to, pairs_from)
  lost   <- data.table::fsetdiff(pairs_from, pairs_to)

  base <- data.table::data.table(
    metric = c("pairs_from", "pairs_to", "pairs_gained", "pairs_lost", "pairs_net",
               "species_from", "species_to", "species_newly_recorded",
               "species_no_longer_recorded",
               "cells_universe", "cells_occupied_from", "cells_occupied_to",
               "cells_filled", "cells_regressed", "cells_never_filled",
               "cells_empty_now",
               "occurrences_from", "occurrences_to",
               "mechanism_boundary_year", "regime_boundary"),
    value = c(nrow(pairs_from), nrow(pairs_to), nrow(gained), nrow(lost),
              nrow(pairs_to) - nrow(pairs_from),
              data.table::uniqueN(pairs_from$key), data.table::uniqueN(pairs_to$key),
              nrow(species[status == "new"]), nrow(species[status == "lost"]),
              nrow(cells), nrow(cells[occ_from > 0]), nrow(cells[occ_to > 0]),
              nrow(cells[status == "filled"]), nrow(cells[status == "regressed"]),
              # never_filled is 0 -> 0: still open, and never was open. Distinct
              # from empty_now, which also catches cells that REGRESSED to zero.
              # Conflating them hides losses inside a "still to do" number.
              nrow(cells[occ_from == 0 & occ_to == 0]),
              nrow(cells[occ_to == 0]),
              sum(cells$occ_from), sum(cells$occ_to),
              baseline_year, as.integer(isTRUE(regime_boundary)))
  )
  base[, source_group := "total"]

  mech <- mechanism[, .(metric = paste0("pairs_", mechanism), value = as.numeric(n_pairs),
                        source_group)]
  out <- data.table::rbindlist(list(base, mech), use.names = TRUE, fill = TRUE)
  out[, key_space := key_space]
  data.table::setcolorder(out, c("metric", "source_group", "key_space", "value"))
  out[]
}

# ============================================================================
# Taxon annotation
# ============================================================================

#' Build the key -> taxonomy lookup that names the closure tables
#'
#' `to` first, `from` only as a fallback for keys `to` does not have.
#'
#' A taxon present at BOTH ends is described by the LATER taxonomy. That is what
#' makes the two key spaces comparable and it is unchanged here.
#'
#' But a taxon that is GONE by `to` has no row in `to` at all. Annotating from
#' `to` alone therefore leaves every lost species with NA name, class and order,
#' and then rolls all of them into a single (NA, NA) group whose loss_rate is
#' 100% by construction, because every pair in it is a lost pair.
#'
#' Measured on the delivered tables: 155 species / 229 baseline pairs for
#' 2021 -> 2024, and 717 species / 26,962 pairs - 8.5% of all loss - for
#' 2024 -> live. All 717 are recoverable from the `from` side: Rosa mollis,
#' Huperzia europaea, Galium palustre subsp. elongatum and 714 others. Unfixed,
#' the taxonomic panel renders an unnamed bar at 100% loss, and the "still open"
#' panel cannot name a single species we no longer have - which is the one thing
#' that panel is for.
#'
#' The lookup is forced unique on key at every step. A duplicated key would fan
#' out the species join and inflate every group total with no error anywhere -
#' the same failure mode closure_rekey() guards against upstream, and the reason
#' this belongs here under test rather than inline in script 14.
#'
#' @param mt_to Match table (script 09a output) for the later time point.
#' @param mt_from Match table for the earlier time point; may be NULL.
#' @param tax_cols Character vector of taxonomy columns to carry across.
#' @param lk_key Key column: "specieskey" in gbif space, "backbone_taxonID" in
#'   dyntaxa space.
#' @return list(lookup = data.table(key, <tax_cols>) unique on key,
#'   n_fallback = number of keys annotated from `from`)
closure_taxon_lookup <- function(mt_to, mt_from, tax_cols, lk_key) {
  if (!lk_key %in% names(mt_to)) {
    cli_abort("closure_taxon_lookup(): {lk_key} is not a column of the later match table")
  }
  lk <- unique(mt_to[, c(lk_key, tax_cols), with = FALSE])
  data.table::setnames(lk, lk_key, "key")
  lk <- unique(lk, by = "key")

  if (is.null(mt_from) || !nrow(mt_from) || !lk_key %in% names(mt_from)) {
    return(list(lookup = lk[], n_fallback = 0L))
  }

  fb_cols <- intersect(tax_cols, names(mt_from))
  fb <- unique(mt_from[, c(lk_key, fb_cols), with = FALSE])
  data.table::setnames(fb, lk_key, "key")
  fb <- unique(fb, by = "key")
  fb <- fb[!(key %in% lk$key)]
  if (!nrow(fb)) return(list(lookup = lk[], n_fallback = 0L))

  # A column the earlier match table does not carry stays NA rather than
  # silently shifting into the wrong slot: a shape change between time points is
  # worth seeing, not papering over.
  for (cc in setdiff(tax_cols, fb_cols)) {
    data.table::set(fb, j = cc, value = NA_character_)
  }
  out <- data.table::rbindlist(
    list(lk, fb[, c("key", tax_cols), with = FALSE]), use.names = TRUE)
  list(lookup = out[], n_fallback = nrow(fb))
}

# ============================================================================
# Bundling for the app
# ============================================================================

#' Assemble script 14's closure CSVs into the tables the "Gaps filled" tab reads
#'
#' Binds each family of tables into ONE long table tagged with pair_id,
#' resolution and key_space, so the app filters rather than juggling list names.
#' Nothing is differenced here or at runtime - script 14 already did it.
#'
#' The file list is the source of truth for what exists, not a
#' RESOLUTIONS x KEY_SPACES guess: a pair that straddles the regime boundary has
#' no gbif-space tables at all, by design, and a guess would produce silent NULLs
#' where the absence is meaningful.
#'
#' THE SPECIES TABLE IS THINNED, AND THAT IS THE ONE THING THAT CAN GO WRONG
#' QUIETLY. Full, it is ~57,000 rows per pair x resolution x key space; across
#' three pairs that is roughly half a million rows of mostly-untouched species,
#' against a bundle that has sat at 78-80 MB since July and should not start
#' growing now (finding-bundle-size-2026-07-23.md). Rows are cut to the three
#' populations the tab enumerates BY NAME and nothing else:
#'
#'   - every red-listed species              (the threatened-species panel)
#'   - every species that lost or contracted (the "where to go next" list, and
#'     the taxa closure_taxon_lookup() above just made nameable)
#'   - the top `keep_top` by cells gained and by cells lost
#'
#' `species_totals` is computed BEFORE the cut, from the full table. That is the
#' invariant the tab depends on: a displayed COUNT comes from species_totals, a
#' displayed NAME comes from species. A tile that counted the kept rows would
#' under-report and still look plausible, which is the worst kind of wrong.
#'
#' @param closure_dir Directory script 14 wrote to (data/{CC}/proc/closure).
#' @param keep_top Rows kept per resolution x key space beyond the red-listed
#'   and lost/contracted species.
#' @param threat_cats Red-list categories treated as "threatened" for the cut.
#' @return list(tables = named list of data.tables, pairs = character vector,
#'   report = data.table(pair_id, n_species_full, n_species_kept))
closure_bundle <- function(closure_dir, keep_top = 1000L,
                           threat_cats = c("CR", "EN", "VU", "NT", "RE", "DD")) {
  date_re <- "\\d{4}-\\d{2}-\\d{2}"
  empty <- list(tables = list(), pairs = character(),
                report = data.table::data.table(
                  pair_id = character(), n_species_full = integer(),
                  n_species_kept = integer()))
  if (!dir.exists(closure_dir)) return(empty)

  sum_re    <- sprintf("^closure_summary_(%s)_(%s)\\.csv$", date_re, date_re)
  sum_files <- list.files(closure_dir, pattern = sum_re)
  if (!length(sum_files)) return(empty)

  read_family <- function(prefix, from, to) {
    pat <- sprintf("^closure_%s_(\\d+)km_([a-z]+)_%s_%s\\.csv$", prefix, from, to)
    fs  <- list.files(closure_dir, pattern = pat)
    if (!length(fs)) return(NULL)
    data.table::rbindlist(lapply(fs, function(f) {
      m <- regmatches(f, regexec(pat, f))[[1]]
      d <- data.table::fread(file.path(closure_dir, f), showProgress = FALSE)
      d[, `:=`(pair_id    = paste(from, to, sep = "__"),
               resolution = paste0(m[2], "km"),
               key_space  = m[3])]
      d[]
    }), use.names = TRUE, fill = TRUE)
  }

  acc <- list(summary = list(), cells = list(), group = list(), mechanism = list(),
              datasets = list(), species = list(), species_totals = list())
  pairs <- character()
  report <- list()

  for (sf in sum_files) {
    m <- regmatches(sf, regexec(sum_re, sf))[[1]]
    from <- m[2]; to <- m[3]; pid <- paste(from, to, sep = "__")

    s <- data.table::fread(file.path(closure_dir, sf), showProgress = FALSE)
    s[, `:=`(pair_id = pid, from_date = from, to_date = to)]
    acc$summary[[pid]] <- s

    for (nm in c("cells", "group", "mechanism", "datasets")) {
      acc[[nm]][[pid]] <- read_family(nm, from, to)
    }

    sp <- read_family("species", from, to)
    if (!is.null(sp)) {
      # Totals FIRST, off the full table. See the note above.
      acc$species_totals[[pid]] <- sp[, .(
        n_species    = .N,
        cells_gained = sum(cells_gained, na.rm = TRUE),
        cells_lost   = sum(cells_lost,   na.rm = TRUE)
      ), by = .(pair_id, resolution, key_space, status,
                threat = data.table::fifelse(
                  threatStatus_redlist %in% threat_cats, threatStatus_redlist, "none"))]

      # The rank is taken WITHIN each resolution x key space. Ranking across the
      # pile would spend the whole budget on 10 km and leave 50 km with nothing.
      sp[, keep :=
           threatStatus_redlist %in% threat_cats |
           status %in% c("lost", "contracted") |
           data.table::frank(-cells_gained, ties.method = "first") <= keep_top |
           data.table::frank(-cells_lost,   ties.method = "first") <= keep_top,
         by = .(resolution, key_space)]
      acc$species[[pid]] <- sp[keep == TRUE][, keep := NULL]
      report[[pid]] <- data.table::data.table(
        pair_id = pid, n_species_full = nrow(sp),
        n_species_kept = nrow(acc$species[[pid]]))
    }
    pairs <- c(pairs, pid)
  }

  tables <- list()
  for (nm in names(acc)) {
    b <- data.table::rbindlist(acc[[nm]], use.names = TRUE, fill = TRUE)
    if (nrow(b)) tables[[nm]] <- b
  }

  # The pair index the tab's selectors are built from. regime_boundary travels
  # with it so the app never infers "is this cross-pipeline?" from the dates -
  # script 14 already decided, and the warnings hang off that decision.
  if (!is.null(tables$summary)) {
    flags <- c("regime_boundary", "mechanism_boundary_year", "filters_differ")
    idx <- tables$summary[metric %in% flags,
                          .(value = max(value)), by = .(pair_id, from_date, to_date, metric)]
    idx <- data.table::dcast(idx, pair_id + from_date + to_date ~ metric,
                             value.var = "value", fill = 0)
    for (fl in setdiff(flags, names(idx))) data.table::set(idx, j = fl, value = 0)
    data.table::setorder(idx, from_date, to_date)
    tables$pair_index <- idx
  }

  list(tables = tables, pairs = pairs,
       report = data.table::rbindlist(report, use.names = TRUE))
}
