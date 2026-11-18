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
#     - periods                   -> min_year of a gained pair
#     - source split              -> datasetkey -> source type
#   Reducing the 42 M-row cube to a ~6.5 M-row grain once, and differencing
#   grains, is what keeps this cheap.
#
# TWO KEY SPACES, deliberately:
#   dyntaxa - key = backbone_taxonID. THE HEADLINE. A national checklist does
#             not move when GBIF changes its own backbone, so this is the only
#             space in which a snapshot and a COL-interpreted cube can be
#             differenced at all.
#   gbif    - key = specieskey. Only comparable between time points whose keys
#             share a vocabulary (closure_pair_kind()), but script 14's
#             regression fixtures use it, so it is the
#             reconciliation that proves this code correct. It is also the
#             honest "all of GBIF" figure, since ~18% of species carry no
#             Dyntaxa taxonID.
#   Every output carries `key_space`. Never mix them in one number.
#
# TWO GBIF TAXONOMIES. The 2021 and 2024 snapshots name species after GBIF's
#   former backbone, later time points after the Catalogue of Life (COL). Each
#   reaches Dyntaxa through its own match table, and the two matchers differ:
#   one name can match at one end only. In a pair across the switch the earlier
#   end is therefore translated into COL first, with GBIF's own mapping from the
#   old keys to COL (closure_col_route()), and hybrids are left out at both ends
#   (COL lists them as taxa; the former backbone had no keys for them).
#
# PAIRS THAT SPAN SOURCE TYPES are counted in EVERY type's figures and ONCE in
#   the total, so per-type columns do not sum to the total. That is deliberate.
#   The alternative - crediting each pair to a single "owner" type - needs a
#   tie-break, and the tie-break alone shifts the shares. Order-independent
#   measures only.
#
# Dependencies: data.table, cli, yaml
# ============================================================================

CLOSURE_CELL_STATUS  <- c("filled", "deepened", "unchanged", "thinned", "regressed")
CLOSURE_SOURCE_TYPES <- c("platform", "monitoring", "collection", "other")
CLOSURE_PERIODS      <- c("baseline_on", "decade_before", "1950_on", "pre_1950", "no_year")
CLOSURE_BAR_STATUS   <- c("rose_above", "fell_below", "stayed_under", "above", "still_empty")

# ============================================================================
# Source types
# ============================================================================

#' Read the typed dataset list
#'
#' Every dataset is typed by how its records come about: `platform` (open
#' reporting platforms and apps, where anyone can report), `monitoring`
#' (monitoring programmes, inventories, surveys and sampling series,
#' metabarcoding included), `collection` (specimens kept in a collection) and
#' `other`. Which dataset is which is a national fact, not code, so the list
#' lives next to the country config (parameters.closure.source_types_file).
#'
#' @param path CSV with at least the columns `datasetkey` and `type`.
#' @return Named character vector: names are dataset keys, values types.
closure_read_types <- function(path) {
  if (!file.exists(path)) {
    cli_abort("Source-type list not found: {.path {path}}")
  }
  d <- data.table::fread(path, colClasses = "character", showProgress = FALSE,
                         encoding = "UTF-8")
  absent <- setdiff(c("datasetkey", "type"), names(d))
  if (length(absent)) {
    cli_abort("{.path {path}} lacks column{?s} {.field {absent}}")
  }
  dup <- unique(d$datasetkey[duplicated(d$datasetkey)])
  if (length(dup)) {
    cli_abort("{.path {path}} lists {length(dup)} dataset{?s} twice: {.val {utils::head(dup, 5)}}")
  }
  bad <- setdiff(unique(d$type), CLOSURE_SOURCE_TYPES)
  if (length(bad)) {
    cli_abort(c("{.path {path}} uses unknown type{?s} {.val {bad}}",
                "i" = "Allowed: {.val {CLOSURE_SOURCE_TYPES}}"))
  }
  stats::setNames(d$type, d$datasetkey)
}

#' Source type of each record's dataset
#'
#' A dataset that is not on the list counts as `other`; script 14 names such
#' datasets, so the list can be extended.
#'
#' @param datasetkey Character vector of GBIF dataset UUIDs.
#' @param types Output of closure_read_types().
#' @return Character vector of types, one per element of `datasetkey`.
closure_source_group <- function(datasetkey, types) {
  out <- unname(types)[match(as.character(datasetkey), names(types))]
  out[is.na(out)] <- "other"
  out
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
#' 'double' but expecting type 'integer'".
#'
#' Coercing `year` and `occurrences` to double BEFORE the aggregation fixes it
#' and is also faster than coercing inside `j`: data.table's GForce optimisation
#' applies to `min(x, na.rm = TRUE)` on a plain column but not to
#' `min(as.numeric(x), ...)`.
#'
#' Inf is then mapped to NA: "no year" is a period of its own, not a year, and
#' it must not sort as one.
#'
#' @param dt data.table with specieskey, eeacellcode, datasetkey, year,
#'   occurrences. Modified by reference - pass a copy if that matters.
#' @param types Output of closure_read_types().
#' @return data.table(key, eeacellcode, datasetkey, source_group, min_year,
#'   occ, n_rec); `source_group` holds the dataset's source type.
closure_build_grain <- function(dt, types) {
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
  g[, source_group := closure_source_group(datasetkey, types)]
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
#' cells. `complete_to_grid()` in R/globals.R follows the same rule.
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
# Periods
# ============================================================================

#' The period a gained pair's earliest record falls in
#'
#' Five periods, relative to the baseline year `b`: from `b` on (records that
#' cannot have been in the baseline), the ten years before `b` (mostly recent
#' sampling published late, such as monitoring and metabarcoding), from 1950 to
#' then, before 1950 (mostly specimens), and no year. The boundaries are
#' definitional; the tab states them as years.
#'
#' @param min_year Numeric vector of years, NA where a pair has no dated record.
#' @param baseline_year Integer; the earlier time point's year.
#' @return Character vector of CLOSURE_PERIODS.
closure_period <- function(min_year, baseline_year) {
  data.table::fcase(
    is.na(min_year),                "no_year",
    min_year >= baseline_year,      "baseline_on",
    min_year >= baseline_year - 10, "decade_before",
    min_year >= 1950,               "1950_on",
    default = "pre_1950"
  )
}

#' First and last year of each period, for labels
closure_period_bounds <- function(baseline_year) {
  data.table::data.table(
    period    = CLOSURE_PERIODS,
    year_from = c(baseline_year, baseline_year - 10, 1950, NA, NA),
    year_to   = c(NA, baseline_year - 1, baseline_year - 11, 1949, NA)
  )
}

#' Gained pairs by the period of their earliest record, per source type
#'
#' Computed WITHIN each source type, not once per pair: "when were these
#' records collected?" is a question about a stream. A type's figure uses the
#' earliest of that type's own records; the `total` uses the pair's overall
#' earliest record.
#'
#' With `taxa` the same counts come per class x order as well, for the tab's
#' taxonomic filter.
#'
#' @param gained data.table(key, eeacellcode) - the gained pairs.
#' @param grain_to The later time point's grain, carrying source_group.
#' @param baseline_year Integer; the earlier time point's year.
#' @param taxa Optional data.table(key, class, order).
#' @return data.table(source_group, [class, order,] period, year_from, year_to,
#'   n_pairs, n_occurrences, n_records, n_cells, n_species)
closure_mechanism <- function(gained, grain_to, baseline_year, taxa = NULL) {
  by_taxa <- !is.null(taxa)
  grp_cols <- c("source_group", if (by_taxa) c("class", "order"), "period")
  if (!nrow(gained)) {
    out <- data.table::data.table(
      source_group = character(), period = character(), year_from = numeric(),
      year_to = numeric(), n_pairs = integer(), n_occurrences = numeric(),
      n_records = numeric(), n_cells = integer(), n_species = integer())
    if (by_taxa) out[, `:=`(class = character(), order = character())]
    data.table::setcolorder(out, c(grp_cols, "year_from", "year_to"))
    return(out[])
  }
  g <- merge(grain_to, gained, by = c("key", "eeacellcode"))
  # Same integer/Inf trap as in closure_build_grain(): if a caller hands us an
  # integer min_year, the first all-NA group returns Inf and the aggregation
  # dies on a type mismatch. Coerce once, here, rather than trust the caller.
  g[, min_year := as.numeric(min_year)]
  g[, occ := as.numeric(occ)]
  g[, n_rec := as.numeric(n_rec)]

  # One row per pair and source type (that type's earliest record), then one
  # per pair over all types. An all-NA group yields Inf from min(na.rm = TRUE),
  # which the second min() passes over and the last step turns into NA.
  p <- g[, .(min_year = suppressWarnings(min(min_year, na.rm = TRUE)),
             occ = sum(occ, na.rm = TRUE), n_rec = sum(n_rec, na.rm = TRUE)),
         by = .(source_group, key, eeacellcode)]
  rm(g)
  p <- data.table::rbindlist(list(
    p[, .(source_group = "total", min_year = suppressWarnings(min(min_year)),
          occ = sum(occ), n_rec = sum(n_rec)), by = .(key, eeacellcode)],
    p), use.names = TRUE)
  p[!is.finite(min_year), min_year := NA_real_]
  p[, period := closure_period(min_year, baseline_year)]
  if (by_taxa) {
    tx <- unique(data.table::as.data.table(taxa)[, .(key, class, order)], by = "key")
    p[, `:=`(class = NA_character_, order = NA_character_)]
    p[tx, on = "key", `:=`(class = i.class, order = i.order)]
  }
  out <- p[, .(n_pairs = .N, n_occurrences = sum(occ), n_records = sum(n_rec),
               n_cells = data.table::uniqueN(eeacellcode),
               n_species = data.table::uniqueN(key)),
           by = grp_cols]
  rm(p)
  out <- merge(out, closure_period_bounds(baseline_year), by = "period", sort = FALSE)
  out[, period_rank := match(period, CLOSURE_PERIODS)]
  out[, type_rank := match(source_group, c("total", CLOSURE_SOURCE_TYPES))]
  data.table::setorderv(out, c("type_rank", setdiff(grp_cols, c("source_group", "period")),
                               "period_rank"))
  out[, c("period_rank", "type_rank") := NULL]
  data.table::setcolorder(out, c(grp_cols, "year_from", "year_to"))
  out[]
}

#' Species x cell pairs per cell and class x order, for the tab's filter
#'
#' Four counts per (cell, class, order): the species recorded there at each end
#' and the pairs gained and lost. The map of the tab filters these rows instead
#' of differencing anything at runtime.
#'
#' @param pairs_from,pairs_to,gained,lost data.table(key, eeacellcode).
#' @param taxa data.table(key, class, order); keys without a row get NA class
#'   and order.
#' @return data.table(eeacellcode, class, order, n_species_from, n_species_to,
#'   pairs_gained, pairs_lost)
closure_cells_by_group <- function(pairs_from, pairs_to, gained, lost, taxa) {
  tx <- unique(data.table::as.data.table(taxa)[, .(key, class, order)], by = "key")
  keys <- c("eeacellcode", "class", "order")
  count <- function(p, col) {
    x <- p[, .(key, eeacellcode, class = NA_character_, order = NA_character_)]
    x[tx, on = "key", `:=`(class = i.class, order = i.order)]
    x <- x[, .N, by = keys]
    data.table::setnames(x, "N", col)
    x
  }
  parts <- list(count(pairs_from, "n_species_from"), count(pairs_to, "n_species_to"),
                count(gained, "pairs_gained"), count(lost, "pairs_lost"))
  out <- Reduce(function(a, b) merge(a, b, by = keys, all = TRUE), parts)
  for (v in c("n_species_from", "n_species_to", "pairs_gained", "pairs_lost")) {
    data.table::set(out, which(is.na(out[[v]])), v, 0L)
  }
  data.table::setorderv(out, keys)
  out[]
}

# ============================================================================
# Source concentration
# ============================================================================

#' Which datasets a set of pairs rests on, without a tie-break
#'
#' Script 14 calls it both ways round: the gained pairs with the later time
#' point's grain (where the gain came from), and the lost pairs with the
#' baseline's grain (what the vanished pairs had rested on).
#'
#' `touched` counts the pairs a dataset contributes any record to.
#' `vanish_if_dropped` counts the pairs whose records ALL come from that one
#' dataset - i.e. what disappears if the dataset is removed. Both are
#' order-independent, which the single-owner alternative is not. Both
#' percentages are of all the pairs passed in.
#'
#' Every pair must have a record in the grain. A pair without one means the
#' wrong time point's grain was passed (the later grain for lost pairs), which
#' would otherwise leave every count at zero without an error.
#'
#' @param pairs data.table(key, eeacellcode), one row per pair: the gained or
#'   the lost pairs.
#' @param grain Grain of the time point the pairs exist in (output of
#'   closure_build_grain(), re-keyed by closure_rekey()).
#' @return data.table(datasetkey, source_group, touched, vanish_if_dropped,
#'   touched_pct, vanish_pct), the dataset touching most pairs first.
closure_dataset_concentration <- function(pairs, grain) {
  if (!nrow(pairs)) {
    return(data.table::data.table(datasetkey = character(), source_group = character(),
                                  touched = integer(), vanish_if_dropped = integer(),
                                  touched_pct = numeric(), vanish_pct = numeric()))
  }
  g <- merge(grain, pairs, by = c("key", "eeacellcode"))
  n_pairs <- nrow(pairs)
  n_missing <- n_pairs - nrow(unique(g[, .(key, eeacellcode)]))
  if (n_missing > 0L) {
    cli_abort(c(
      "closure_dataset_concentration(): {n_missing} pair{?s} {?has/have} no record \\
       in the grain (of {n_pairs}).",
      "i" = "Pass the grain of the time point the pairs exist in: the later one \\
             for gained pairs, the baseline for lost pairs."))
  }

  touched <- unique(g[, .(key, eeacellcode, datasetkey, source_group)])[
    , .(touched = .N), by = .(datasetkey, source_group)]

  n_ds <- unique(g[, .(key, eeacellcode, datasetkey)])[
    , .(n_ds = .N), by = .(key, eeacellcode)]
  sole <- merge(unique(g[, .(key, eeacellcode, datasetkey)]),
                n_ds[n_ds == 1L, .(key, eeacellcode)],
                by = c("key", "eeacellcode"))[, .(vanish_if_dropped = .N), by = datasetkey]

  out <- merge(touched, sole, by = "datasetkey", all.x = TRUE)
  out[is.na(vanish_if_dropped), vanish_if_dropped := 0L]
  out[, touched_pct := round(100 * touched / n_pairs, 3)]
  out[, vanish_pct  := round(100 * vanish_if_dropped / n_pairs, 3)]
  data.table::setorder(out, -touched)
  out[]
}

# ============================================================================
# The low-data bar, frozen at the baseline
# ============================================================================

#' Where a cell stands against the low-data bar, at both ends
#'
#' A cell is under the bar when it holds at most `thr` records, none included:
#' a cell that empties falls below the bar, and one that fills past it rises
#' above it. Cells with no record at either end are counted apart, as still
#' empty, so they do not swell "stayed under".
#'
#' @param occ_from,occ_to Records per cell at the baseline and at the later
#'   time point.
#' @param thr The bar (records per cell). NA gives NA.
#' @return Character vector: "rose_above", "fell_below", "stayed_under",
#'   "still_empty" or "above" (above the bar at both ends).
closure_bar_status <- function(occ_from, occ_to, thr) {
  thr <- rep_len(as.numeric(thr), length(occ_from))
  under_from <- occ_from <= thr
  under_to   <- occ_to   <= thr
  data.table::fcase(
    is.na(thr),                      NA_character_,
    occ_from == 0 & occ_to == 0,     "still_empty",
    under_from & !under_to,          "rose_above",
    !under_from & under_to,          "fell_below",
    under_from & under_to,           "stayed_under",
    default = "above"
  )
}

#' Cells against the BASELINE's low-data bar
#'
#' `gap_zero` is absolute and means the same thing at two dates. `gap_low_q10`
#' does not: script 07 recomputes quantiles from whatever data it is given, so
#' comparing its flag across dates confounds real change with distribution
#' shift - a cell can be flagged newly under-sampled while its record count
#' rose. Freezing the baseline's bar (the `q` quantile of records per occupied
#' cell) and applying it to both ends is the only version of this metric that
#' means anything in a difference.
#'
#' @param cells Output of closure_cells().
#' @param q Quantile (default 0.10).
#' @return The input with `q_frozen` (the bar) and `bar_status`
#'   (closure_bar_status()).
closure_frozen_low <- function(cells, q = 0.10) {
  nz <- cells[occ_from > 0, occ_from]
  thr <- if (length(nz)) as.numeric(stats::quantile(nz, probs = q, names = FALSE)) else NA_real_
  out <- data.table::copy(cells)
  out[, q_frozen := thr]
  out[, bar_status := closure_bar_status(occ_from, occ_to, q_frozen)]
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
              # never_filled is 0 -> 0: open then, still open now. Distinct
              # from empty_now, which also catches cells that REGRESSED to zero.
              # Conflating them hides losses inside a "still to do" number.
              nrow(cells[occ_from == 0 & occ_to == 0]),
              nrow(cells[occ_to == 0]),
              sum(cells$occ_from), sum(cells$occ_to),
              baseline_year, as.integer(isTRUE(regime_boundary)))
  )
  base[, source_group := "total"]

  mech <- mechanism[, .(metric = paste0("pairs_period_", period),
                        value = as.numeric(n_pairs), source_group)]
  out <- data.table::rbindlist(list(base, mech), use.names = TRUE, fill = TRUE)
  out[, key_space := key_space]
  data.table::setcolorder(out, c("metric", "source_group", "key_space", "value"))
  out[]
}

# ============================================================================
# Time-point metadata and pair selection
# ============================================================================
# A time point carries two facts that decide what can be differenced against it:
#   regime        "snapshot" (GBIF Sweden's analytics.snapshots export, 2 dp,
#                 no status/geospatial filters) or "cube" (GBIF SQL cube, full
#                 precision, presence only + no geospatial issues).
#   key_backbone  "legacy" (GBIF nub integer keys) or "col" (COL taxonIDs).
# They are independent: the 2026-10-01 snapshot is regime "snapshot" with
# key_backbone "col". Regime decides the method gap; key_backbone decides
# whether GBIF key space can be compared at all.

CLOSURE_REGIMES       <- c("snapshot", "cube")
CLOSURE_KEY_BACKBONES <- c("legacy", "col")

#' Classify a pair of time points
#'
#' A pair across regimes whose dates are closer than `bridge_days` is the same
#' moment measured two ways. Its difference is method, not change, so it is the
#' bridge (script 15) and never a closure pair.
#'
#' @param meta_from,meta_to list(tp, regime, key_backbone).
#' @param bridge_days Cross-regime pairs closer than this are the bridge.
#' @return list(regime_boundary, key_backbone_differs, is_bridge, key_spaces)
closure_pair_kind <- function(meta_from, meta_to, bridge_days = 31L) {
  for (m in list(meta_from, meta_to)) {
    if (!isTRUE(m$regime %in% CLOSURE_REGIMES)) {
      cli_abort("closure_pair_kind(): regime of {m$tp} must be one of \\
                 {paste(CLOSURE_REGIMES, collapse = ', ')} (got {.val {m$regime}})")
    }
    if (!isTRUE(m$key_backbone %in% CLOSURE_KEY_BACKBONES)) {
      cli_abort("closure_pair_kind(): key_backbone of {m$tp} must be one of \\
                 {paste(CLOSURE_KEY_BACKBONES, collapse = ', ')} (got {.val {m$key_backbone}})")
    }
  }
  regime_boundary <- !identical(meta_from$regime, meta_to$regime)
  kb_differs      <- !identical(meta_from$key_backbone, meta_to$key_backbone)
  days <- abs(as.numeric(as.Date(meta_to$tp) - as.Date(meta_from$tp)))
  list(
    regime_boundary      = regime_boundary,
    key_backbone_differs = kb_differs,
    is_bridge            = regime_boundary && days < bridge_days,
    # Keys of different vocabularies share nothing: a set difference would
    # report every pair as lost and every pair as gained, without erroring.
    key_spaces           = if (kb_differs) "dyntaxa" else c("dyntaxa", "gbif")
  )
}

#' The default set of closure pairs
#'
#' Within each regime: every consecutive pair, plus first -> last when the
#' regime has more than two points. Cross-regime pairs are left out of the
#' default because their loss side is mostly method (see script 15); they can
#' still be run one at a time with GAP_CLOSURE_PAIR.
#'
#' @param meta data.table(tp, regime), one row per time point.
#' @return list of character(2) pairs, ordered by regime then date.
closure_default_pairs <- function(meta) {
  meta <- data.table::as.data.table(meta)
  out <- list()
  for (rg in intersect(CLOSURE_REGIMES, unique(meta$regime))) {
    tps <- sort(meta[regime == rg, tp])
    if (length(tps) < 2L) next
    for (i in seq_len(length(tps) - 1L)) out[[length(out) + 1L]] <- tps[c(i, i + 1L)]
    if (length(tps) > 2L) out[[length(out) + 1L]] <- tps[c(1L, length(tps))]
  }
  out
}

# ============================================================================
# Between GBIF's former backbone and the Catalogue of Life
# ============================================================================

# The match-table columns script 14 reads besides the key: the Dyntaxa taxon a
# GBIF key is counted as, and how the taxon is named and ranked.
CLOSURE_TAXON_COLS <- c("backbone_taxonID", "backbone_scientificName", "taxonRank",
                        "class", "order", "threatStatus_redlist", "threatStatus_backbone")

#' TRUE for a hybrid name: a "×" anywhere, or an "x" standing alone between words
#'
#' Dyntaxa writes plant hybrids with "×" (Galium mollugo × verum) and some
#' animal hybrids with a plain "x" (Anser anser x Branta canadensis).
closure_is_hybrid <- function(name) {
  name <- as.character(name)
  !is.na(name) & (grepl("\u00d7", name) | grepl("(^|\\s)x(\\s|$)", name))
}

#' The species name in a COL name: subgenus dropped, genus and epithet kept
#'
#' "Stictoleptura (Maculileptura) maculicornis" gives "Stictoleptura
#' maculicornis"; "Hypochaeris maculata subsp. maculata" gives "Hypochaeris
#' maculata". A hybrid has no species name of its own and gives NA.
closure_species_name <- function(name) {
  name <- trimws(gsub("\\s*\\([^)]*\\)", "", as.character(name)))
  words <- strsplit(name, "\\s+")
  out <- vapply(words, function(w) if (length(w) >= 2L) paste(w[1:2], collapse = " ") else NA_character_,
                character(1))
  out[is.na(name) | closure_is_hybrid(name)] <- NA_character_
  out
}

#' Count a time point keyed in GBIF's former backbone the way a COL-keyed one is
#'
#' A pair whose ends use different GBIF taxonomies compares two match tables
#' built by different matchers. A spelling only the COL matcher resolves
#' (Phylloscopus sibillatrix), a species renamed between the two lists
#' (Trommsdorffia → Hypochaeris maculata) and a species COL merges into another
#' (Rosa mollis → R. villosa) then turn up as lost or new without any change in
#' the records. This gives every old key the Dyntaxa taxon the COL-keyed end
#' gives to the same species:
#'
#'   key     the old key's COL taxon (GBIF's mapping; a synonym is replaced by
#'           its accepted taxon) is a key of `mt_col`: take that row's taxon;
#'   name    otherwise, the species name of that COL taxon is the GBIF name of a
#'           matched row of `mt_col`: take that row's taxon (the most recorded
#'           one when the name occurs twice);
#'   legacy  otherwise, keep the old key's own match.
#'
#' @param mt_legacy Match table of the time point keyed in the former backbone.
#' @param mt_col Match table of the COL-keyed time point of the pair.
#' @param mapping data.table(legacy_key, col_key, col_name): the COL taxon of
#'   each old key, accepted taxon already resolved (read_col_mapping()).
#' @return `mt_legacy`'s specieskey, species and total_occ, the taxon columns
#'   (CLOSURE_TAXON_COLS, as text) and `col_route` ("key", "name", "legacy").
closure_col_route <- function(mt_legacy, mt_col, mapping) {
  need <- c("specieskey", "species")
  for (nm in c("mt_legacy", "mt_col")) {
    absent <- setdiff(c(need, "backbone_taxonID"), names(get(nm)))
    if (length(absent)) cli_abort("closure_col_route(): {nm} lacks {.field {absent}}")
  }
  cols <- intersect(CLOSURE_TAXON_COLS, names(mt_col))
  keep <- intersect(c("specieskey", "species", "total_occ"), names(mt_legacy))
  out <- data.table::as.data.table(mt_legacy)[, keep, with = FALSE]
  out[, specieskey := as.character(specieskey)]
  own <- data.table::as.data.table(mt_legacy)
  for (cc in cols) {
    v <- if (cc %in% names(own)) as.character(own[[cc]]) else rep(NA_character_, nrow(own))
    data.table::set(out, j = cc, value = v)
  }

  mp <- unique(data.table::as.data.table(mapping), by = "legacy_key")
  i_mp <- match(out$specieskey, as.character(mp$legacy_key))
  col_key <- as.character(mp$col_key)[i_mp]
  canon <- closure_species_name(mp$col_name[i_mp])

  mc <- data.table::as.data.table(mt_col)
  mc[, specieskey := as.character(specieskey)]
  by_key <- unique(mc, by = "specieskey")
  matched <- mc[!is.na(backbone_taxonID) & nzchar(as.character(backbone_taxonID))]
  if ("total_occ" %in% names(matched)) {
    data.table::setorderv(matched, "total_occ", order = -1L, na.last = TRUE)
  }
  by_name <- unique(matched, by = "species")

  i_key  <- match(col_key, by_key$specieskey)
  i_name <- match(canon, as.character(by_name$species))
  route <- data.table::fifelse(!is.na(i_key), "key",
                               data.table::fifelse(!is.na(i_name), "name", "legacy"))
  for (cc in cols) {
    v <- out[[cc]]
    k <- route == "key"; n <- route == "name"
    v[k] <- as.character(by_key[[cc]])[i_key[k]]
    v[n] <- as.character(by_name[[cc]])[i_name[n]]
    data.table::set(out, j = cc, value = v)
  }
  out[, col_route := route]
  out[]
}

#' Dyntaxa taxa that are hybrids, from the match tables of a pair
#'
#' @param ... Match tables (backbone_taxonID, backbone_scientificName).
#' @return Character vector of Dyntaxa taxon IDs.
closure_hybrid_keys <- function(...) {
  ids <- unlist(lapply(list(...), function(m) {
    m <- data.table::as.data.table(m)
    if (!all(c("backbone_taxonID", "backbone_scientificName") %in% names(m))) return(character())
    as.character(m$backbone_taxonID[closure_is_hybrid(m$backbone_scientificName)])
  }))
  sort(unique(ids[!is.na(ids) & nzchar(ids)]))
}

# ============================================================================
# Taxon annotation
# ============================================================================

#' Build the key -> taxonomy lookup that names the closure tables
#'
#' `to` first, `from` only as a fallback for keys `to` does not have.
#'
#' A taxon present at BOTH ends is described by the LATER taxonomy. That is what
#' makes the two key spaces comparable.
#'
#' But a taxon that is GONE by `to` has no row in `to` at all. Annotating from
#' `to` alone therefore leaves every lost species with NA name, class and order,
#' and then rolls all of them into a single (NA, NA) group whose loss_rate is
#' 100% by construction, because every pair in it is a lost pair.
#'
#' Without the `from` fallback, the taxonomic panel renders an unnamed bar at
#' 100% loss, and the "still open" panel cannot name a single species we no
#' longer have - which is the one thing that panel is for.
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
#' against an ~80 MB app bundle that should not grow. Rows are cut to the three
#' populations the tab enumerates BY NAME and nothing else:
#'
#'   - every red-listed species              (the threatened-species panel)
#'   - every species that lost or contracted (the "where to go next" list, and
#'     the taxa closure_taxon_lookup() above makes nameable)
#'   - the top `keep_top` by cells gained and by cells lost
#'
#' `species_totals` is computed BEFORE the cut, from the full table. That is the
#' invariant the tab depends on: a displayed COUNT comes from species_totals, a
#' displayed NAME comes from species. A tile that counted the kept rows would
#' under-report and still look plausible, which is the worst kind of wrong.
#'
#' `species_totals` comes per class x order, so the tab's taxonomic filter
#' counts species without the thinned table either.
#'
#' The cells keep the counts, the bar and `bar_status`, which is computed here
#' from occ_from, occ_to and q_frozen (closure_bar_status()): the tab reads no
#' other classification of the low-data bar.
#'
#' Blank class and order are "" in every table, never NA, so one test finds
#' them in the tab.
#'
#' @param closure_dir Directory script 14 wrote to (data/{CC}/proc/closure).
#' @param keep_top Rows kept per resolution x key space beyond the red-listed
#'   and lost/contracted species.
#' @param threat_cats Red-list categories treated as "threatened" for the cut.
#' @param datasets_meta Optional data.table(datasetkey, title, publisher)
#'   (closure_read_dataset_meta()); joined onto the dataset tables.
#' @return list(tables = named list of data.tables, pairs = character vector,
#'   report = data.table(pair_id, n_species_full, n_species_kept))
closure_bundle <- function(closure_dir, keep_top = 1000L,
                           threat_cats = c("CR", "EN", "VU", "NT", "RE", "DD"),
                           datasets_meta = NULL) {
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
      for (cc in intersect(c("class", "order"), names(d))) {
        data.table::set(d, which(is.na(d[[cc]])), cc, "")
      }
      d[, `:=`(pair_id    = paste(from, to, sep = "__"),
               resolution = paste0(m[2], "km"),
               key_space  = m[3])]
      d[]
    }), use.names = TRUE, fill = TRUE)
  }

  acc <- list(summary = list(), cells = list(), cells_group = list(), group = list(),
              mechanism = list(), mechanism_group = list(), datasets = list(),
              datasets_lost = list(), species = list(), species_totals = list())
  pairs <- character()
  report <- list()

  for (sf in sum_files) {
    m <- regmatches(sf, regexec(sum_re, sf))[[1]]
    from <- m[2]; to <- m[3]; pid <- paste(from, to, sep = "__")

    s <- data.table::fread(file.path(closure_dir, sf), showProgress = FALSE)
    s[, `:=`(pair_id = pid, from_date = from, to_date = to)]
    acc$summary[[pid]] <- s

    for (nm in c("cells", "cells_group", "group", "mechanism", "mechanism_group",
                 "datasets", "datasets_lost")) {
      acc[[nm]][[pid]] <- read_family(nm, from, to)
    }
    cl <- acc$cells[[pid]]
    if (!is.null(cl)) {
      cl[, bar_status := closure_bar_status(occ_from, occ_to, q_frozen)]
      keep <- intersect(c("eeacellcode", "occ_from", "n_species_from", "occ_to",
                          "n_species_to", "delta_occ", "delta_species", "status",
                          "q_frozen", "bar_status", "pair_id", "resolution",
                          "key_space"), names(cl))
      acc$cells[[pid]] <- cl[, keep, with = FALSE]
    }
    for (nm in c("datasets", "datasets_lost")) {
      d <- acc[[nm]][[pid]]
      if (is.null(d) || is.null(datasets_meta)) next
      meta <- unique(data.table::as.data.table(datasets_meta)[
        , .(datasetkey, title, publisher)], by = "datasetkey")
      d[, `:=`(title = NA_character_, publisher = NA_character_)]
      d[meta, on = "datasetkey", `:=`(title = i.title, publisher = i.publisher)]
      acc[[nm]][[pid]] <- d
    }

    sp <- read_family("species", from, to)
    if (!is.null(sp)) {
      for (cc in setdiff(c("class", "order"), names(sp))) data.table::set(sp, j = cc, value = "")
      # Totals FIRST, off the full table. See the note above.
      acc$species_totals[[pid]] <- sp[, .(
        n_species    = .N,
        cells_gained = sum(cells_gained, na.rm = TRUE),
        cells_lost   = sum(cells_lost,   na.rm = TRUE)
      ), by = .(pair_id, resolution, key_space, class, order, status,
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

  # The per-group cell table is by far the largest (some two million rows for
  # three pairs). With its codes and names as factors it takes a third less
  # memory in the app.
  if (!is.null(tables$cells_group)) {
    for (cc in c("eeacellcode", "class", "order", "pair_id", "resolution", "key_space")) {
      data.table::set(tables$cells_group, j = cc, value = factor(tables$cells_group[[cc]]))
    }
  }

  # The pair index the tab's selectors are built from. regime_boundary travels
  # with it so the app never infers "is this cross-pipeline?" from the dates -
  # script 14 already decided, and the warnings hang off that decision.
  # key_backbone_differs says whether the pair has a GBIF key space at all.
  if (!is.null(tables$summary)) {
    flags <- c("regime_boundary", "mechanism_boundary_year", "filters_differ",
               "key_backbone_differs")
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

#' Titles and publishers of the datasets, for the tab's dataset tables
#'
#' Read from the typed dataset list (parameters.closure.source_types_file),
#' which names every dataset of every time point.
#'
#' @param path The typed dataset list.
#' @return data.table(datasetkey, title, publisher), or NULL when the file is
#'   missing.
closure_read_dataset_meta <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- data.table::fread(path, colClasses = "character", showProgress = FALSE,
                         encoding = "UTF-8")
  if (!"datasetkey" %in% names(d)) return(NULL)
  for (cc in setdiff(c("title", "publisher"), names(d))) {
    data.table::set(d, j = cc, value = NA_character_)
  }
  unique(d[, .(datasetkey, title, publisher)], by = "datasetkey")[]
}

#' The national checklist release each compared date was matched with
#'
#' run_timepoint.R records the release in each time point's
#' reference_version.yml. The live time point has no such file: it was matched
#' with the live pipeline's checklist, `live_release`. Script 14 refuses a pair
#' whose dates differ in it, so the tab can name one release per pair, and say
#' when the other tabs use another one.
#'
#' @param tps Time point dates ("YYYY-MM-DD").
#' @param timepoints_dir Directory with one folder per time point.
#' @param live_release Release of the live pipeline's checklist, for a time
#'   point without reference_version.yml; NA when unknown.
#' @return Named character vector, time point -> release ("YYYY-MM-DD"), NA
#'   where unknown.
closure_reference_releases <- function(tps, timepoints_dir, live_release = NA_character_) {
  day <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(as.character(x[1]))) return(NA_character_)
    d <- suppressWarnings(as.Date(substr(as.character(x[1]), 1, 10)))
    if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
  }
  tps <- unique(as.character(tps))
  rel <- vapply(tps, function(tp) {
    f <- file.path(timepoints_dir, tp, "reference_version.yml")
    if (!file.exists(f)) return(day(live_release))
    r <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    day(r$taxonomy$published)
  }, character(1))
  stats::setNames(unname(rel), tps)
}

#' The bundle's metadata entries for the Gaps filled tab
#'
#' @param cb Output of closure_bundle().
#' @param keep_top The `keep_top` closure_bundle() ran with: the tab says how
#'   many species beyond the complete groups it lists.
#' @param notes Time point notes (parameters.closure.timepoint_notes), or NULL.
#' @param reference_releases closure_reference_releases() for the dates of the
#'   pairs, or NULL.
#' @return list(has_closure, closure_pairs, closure_species_top_n,
#'   closure_timepoint_notes, closure_reference_releases)
closure_bundle_meta <- function(cb, keep_top, notes = NULL, reference_releases = NULL) {
  list(
    has_closure = !is.null(cb$tables$summary) && !is.null(cb$tables$pair_index) &&
      nrow(cb$tables$pair_index) > 0,
    closure_pairs = cb$pairs,
    closure_species_top_n = keep_top,
    closure_timepoint_notes = notes,
    closure_reference_releases = reference_releases
  )
}
