# R/bridge.R
# ============================================================================
# The bridge: one moment measured two ways
# ============================================================================
# Purpose:
#   The arithmetic behind scripts/15_bridge.R. A snapshot and a cube taken a day
#   apart describe the same GBIF content, so every difference between them is
#   method: occurrence filters (presence only, no geospatial issues), 2 dp
#   versus full-precision gridding, the NFI exclusion in the snapshot SQL, and
#   records whose interpretation differs between the two reads. Measuring that
#   gap here is what lets a snapshot number and a cube number be put side by
#   side.
#
#   Like R/closure.R this touches no files. Inputs are the grains and pair
#   tables R/closure.R already builds; tools/test_bridge.R checks every
#   function against a hand-built fixture.
#
# SIDES: "snapshot" and "cube". A pair present only in the snapshot is
#   `snapshot_only`, one present only in the cube is `cube_only`.
#
# LOCAL vs REGIONAL:
#   A 10 km pair missing on one side whose taxon is still present in the same
#   50 km cell on that side is LOCAL: the record exists, it fell into the
#   neighbouring 10 km cell (2 dp rounding) or one nearby record was filtered.
#   Otherwise it is REGIONAL: the taxon is gone from the whole 50 km cell, which
#   is what record filters (absences, geospatial issues) and name differences
#   do. At 50 km every missing pair is regional by definition.
#
# Dependencies: data.table, cli, R/closure.R
# ============================================================================

#' 50 km EEA cell containing a 10 km EEA cell
#'
#' "10kmE474N381" -> "50kmE470N380". The 50 km code is in 10 km units, so the
#' easting/northing are floored to a multiple of 5 (as 04b's nesting check).
bridge_parent_cell <- function(cell10) {
  ok <- grepl("^10kmE[0-9]+N[0-9]+$", cell10)
  e <- suppressWarnings(as.integer(sub("^10kmE([0-9]+)N[0-9]+$", "\\1", cell10)))
  n <- suppressWarnings(as.integer(sub("^10kmE[0-9]+N([0-9]+)$", "\\1", cell10)))
  out <- rep(NA_character_, length(cell10))
  out[ok] <- sprintf("50kmE%dN%d", (e[ok] %/% 5L) * 5L, (n[ok] %/% 5L) * 5L)
  out
}

#' Split two pair tables into both / snapshot-only / cube-only
#'
#' @param p_snap,p_cube data.table(key, eeacellcode), as closure_pairs().
#' @return list(both, snapshot_only, cube_only), each data.table(key, eeacellcode).
bridge_compare_pairs <- function(p_snap, p_cube) {
  list(
    both          = data.table::fintersect(p_snap, p_cube),
    snapshot_only = data.table::fsetdiff(p_snap, p_cube),
    cube_only     = data.table::fsetdiff(p_cube, p_snap)
  )
}

#' Is a missing 10 km pair local (taxon still in the parent 50 km cell)?
#'
#' @param only 10 km pairs present on one side only, data.table(key, eeacellcode).
#' @param other_pairs50 50 km pairs of the OTHER side, data.table(key, eeacellcode).
#' @return logical, one per row of `only`.
bridge_is_local <- function(only, other_pairs50) {
  if (!nrow(only)) return(logical())
  x <- data.table::copy(only)[, .row := .I]
  x[, parent := bridge_parent_cell(eeacellcode)]
  hit <- unique(other_pairs50[, .(key, parent = eeacellcode, .hit = TRUE)])
  x <- merge(x, hit, by = c("key", "parent"), all.x = TRUE, sort = FALSE)
  data.table::setorder(x, .row)
  !is.na(x$.hit)
}

#' Which datasets carry the pairs that one side lacks
#'
#' A pair is SOLELY supported by a dataset when that dataset is its only source
#' on the side where it exists. Sole support is what makes the attribution
#' clean: removing that dataset's records removes the pair.
#'
#' @param only Pairs present on one side only, data.table(key, eeacellcode).
#' @param grain That side's grain (closure_build_grain(), re-keyed).
#' @return data.table(datasetkey, pairs_any, pairs_sole), by pairs_sole desc.
bridge_dataset_support <- function(only, grain) {
  if (!nrow(only)) {
    return(data.table::data.table(datasetkey = character(), pairs_any = integer(),
                                  pairs_sole = integer()))
  }
  sup <- unique(merge(grain[, .(key, eeacellcode, datasetkey)], only,
                      by = c("key", "eeacellcode")))
  sup[, n_ds := .N, by = .(key, eeacellcode)]
  out <- sup[, .(pairs_any = .N, pairs_sole = sum(n_ds == 1L)), by = datasetkey]
  data.table::setorder(out, -pairs_sole, -pairs_any, datasetkey)
  out[]
}

#' Bridge factors per class x order
#'
#' bridge_factor = pairs_cube / pairs_snapshot. Multiplying a snapshot pair
#' count by it gives the cube-equivalent count for the same moment; a factor
#' near 1 with small snapshot_only and cube_only shares means the group is
#' comparable across the regimes.
#'
#' @param p_snap,p_cube Pair tables of the two sides.
#' @param cmp Output of bridge_compare_pairs(p_snap, p_cube).
#' @param lookup data.table(key, class, order).
#' @return data.table(class, order, n_taxa, pairs_snapshot, pairs_cube,
#'   snapshot_only, cube_only, bridge_factor, snapshot_only_pct, cube_only_pct)
bridge_group_factors <- function(p_snap, p_cube, cmp, lookup) {
  lk <- unique(lookup[, .(key = as.character(key), class, order)], by = "key")
  count <- function(p, nm) {
    out <- merge(p[, .(key)], lk, by = "key", all.x = TRUE)[
      , .(n = .N), by = .(class, order)]
    data.table::setnames(out, "n", nm)
    out
  }
  taxa <- merge(unique(rbind(p_snap[, .(key)], p_cube[, .(key)])), lk,
                by = "key", all.x = TRUE)[, .(n_taxa = .N), by = .(class, order)]
  parts <- list(taxa,
                count(p_snap, "pairs_snapshot"), count(p_cube, "pairs_cube"),
                count(cmp$snapshot_only, "snapshot_only"),
                count(cmp$cube_only, "cube_only"))
  g <- Reduce(function(a, b) merge(a, b, by = c("class", "order"), all = TRUE), parts)
  for (col in c("pairs_snapshot", "pairs_cube", "snapshot_only", "cube_only")) {
    data.table::set(g, which(is.na(g[[col]])), col, 0L)
  }
  g[, bridge_factor := round(data.table::fifelse(pairs_snapshot > 0,
                                                 pairs_cube / pairs_snapshot, NA_real_), 4)]
  g[, snapshot_only_pct := round(data.table::fifelse(pairs_snapshot > 0,
                                                     100 * snapshot_only / pairs_snapshot, NA_real_), 2)]
  g[, cube_only_pct := round(data.table::fifelse(pairs_snapshot > 0,
                                                 100 * cube_only / pairs_snapshot, NA_real_), 2)]
  data.table::setorder(g, -pairs_snapshot)
  g[]
}

#' Occurrences per dataset on both sides
#'
#' The dataset-level view of the record filters: a dataset whose cube count is
#' far below its snapshot count is losing records to the cube's filters
#' (absences, geospatial issues); one missing from the snapshot entirely is
#' excluded by the snapshot SQL (the NFI datasets) or was being republished.
#'
#' @param g_snap,g_cube Grains of the two sides (any key space).
#' @return data.table(datasetkey, occ_snapshot, occ_cube, occ_diff, occ_ratio),
#'   by |occ_diff| desc.
bridge_dataset_occ <- function(g_snap, g_cube) {
  s <- g_snap[, .(occ_snapshot = sum(as.numeric(occ))), by = datasetkey]
  cb <- g_cube[, .(occ_cube = sum(as.numeric(occ))), by = datasetkey]
  d <- merge(s, cb, by = "datasetkey", all = TRUE)
  d[is.na(occ_snapshot), occ_snapshot := 0]
  d[is.na(occ_cube), occ_cube := 0]
  d[, occ_diff := occ_cube - occ_snapshot]
  d[, occ_ratio := round(data.table::fifelse(occ_snapshot > 0,
                                             occ_cube / occ_snapshot, NA_real_), 4)]
  d <- d[order(-abs(occ_diff), datasetkey)]
  d[]
}

#' The bridge's headline numbers, long format
#'
#' @param cmp bridge_compare_pairs() output.
#' @param p_snap,p_cube Pair tables.
#' @param local_snap,local_cube bridge_is_local() for cmp$snapshot_only and
#'   cmp$cube_only (NULL at 50 km, where locality is undefined).
#' @param g_snap,g_cube Grains (for occurrence totals and cells).
#' @return data.table(metric, value)
bridge_summary <- function(cmp, p_snap, p_cube, local_snap = NULL, local_cube = NULL,
                           g_snap, g_cube) {
  n_s <- nrow(p_snap)
  pct <- function(x) if (n_s > 0) round(100 * x / n_s, 3) else NA_real_
  cells_s <- unique(p_snap$eeacellcode); cells_c <- unique(p_cube$eeacellcode)
  taxa_s  <- unique(p_snap$key);         taxa_c  <- unique(p_cube$key)
  out <- data.table::data.table(
    metric = c("pairs_snapshot", "pairs_cube", "pairs_both",
               "pairs_snapshot_only", "pairs_cube_only",
               "snapshot_only_pct", "cube_only_pct", "pairs_ratio",
               "taxa_snapshot", "taxa_cube", "taxa_both",
               "cells_snapshot", "cells_cube", "cells_both",
               "occurrences_snapshot", "occurrences_cube"),
    value = c(n_s, nrow(p_cube), nrow(cmp$both),
              nrow(cmp$snapshot_only), nrow(cmp$cube_only),
              pct(nrow(cmp$snapshot_only)), pct(nrow(cmp$cube_only)),
              if (n_s > 0) round(nrow(p_cube) / n_s, 4) else NA_real_,
              length(taxa_s), length(taxa_c), length(intersect(taxa_s, taxa_c)),
              length(cells_s), length(cells_c), length(intersect(cells_s, cells_c)),
              sum(as.numeric(g_snap$occ)), sum(as.numeric(g_cube$occ)))
  )
  if (!is.null(local_snap) && !is.null(local_cube)) {
    out <- rbind(out, data.table::data.table(
      metric = c("snapshot_only_local", "snapshot_only_regional",
                 "cube_only_local", "cube_only_regional"),
      value = c(sum(local_snap), sum(!local_snap), sum(local_cube), sum(!local_cube))))
  }
  out[]
}
