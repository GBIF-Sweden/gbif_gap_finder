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
