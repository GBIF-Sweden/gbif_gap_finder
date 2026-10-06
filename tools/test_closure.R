#!/usr/bin/env Rscript
# tools/test_closure.R
# ============================================================================
# Unit tests for R/closure.R
# ============================================================================
# The closure arithmetic decides every number in the "Gaps filled" tab, and most
# of its failure modes are silent: an off-by-one in a set difference, a
# tie-break creeping into a concentration figure, a mechanism computed over the
# wrong subset. None of those error - they just publish a different number.
#
# So the fixture below is tiny and its right answers are worked out by hand in
# the comments. Six species, five cells, three datasets. If a change to
# R/closure.R breaks one of these, the message says which invariant died.
#
#   Rscript tools/test_closure.R
#
# Exits non-zero on any failure, so it can go in CI. Needs data.table, cli and
# here only - no arrow, no cubes, no pipeline run.
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(cli); library(here) })
source(here::here("R", "closure.R"))

fail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) cli_alert_success(msg)
  else { cli_alert_danger(msg); fail <<- fail + 1L }
}
eq <- function(got, want, msg) ok(isTRUE(all.equal(got, want)),
                                 sprintf("%s  (got %s, want %s)", msg,
                                         paste(got, collapse = ","),
                                         paste(want, collapse = ",")))

ALL_CELLS <- c("C1", "C2", "C3", "C4", "C5")
PLATFORMS <- "D1"          # D2, D3 are collections
BASELINE_YEAR <- 2021L

# NOTE: `key` is a reserved ARGUMENT of data.table(), so data.table(key = "A")
# sets the table's key rather than creating a column called `key`. Build these
# through as.data.table(list(...)), which has no such collision. R/closure.R only
# ever reads `key` as an existing column, so it is unaffected - but anyone
# extending this will meet the same trap.
grain <- function(rows) {
  g <- data.table::rbindlist(lapply(rows, function(r) data.table::as.data.table(list(
    key = r[[1]], eeacellcode = r[[2]], datasetkey = r[[3]],
    min_year = r[[4]], occ = r[[5]], n_rec = r[[6]]))))
  g[, source_group := closure_source_group(datasetkey, PLATFORMS)]
  g[]
}

# --- FROM: 4 pairs, cells C1(13) C2(5) C3(7) occupied, C4 C5 empty ----------
gf <- grain(list(
  list("A", "C1", "D1", 2010, 10, 1),
  list("A", "C2", "D1", 2011,  5, 1),
  list("B", "C1", "D2", 2012,  3, 1),
  list("C", "C3", "D1", 2015,  7, 1)
))

# --- TO --------------------------------------------------------------------
# A-C1 deepened 13->23 · A-C2 unchanged · C-C3 dropped (regressed, C lost)
# GAINED: A-C4 (D1, 2022)      -> fieldwork, platform
#         D-C2 (D3, 1995)      -> digitisation, collections, D is new
#         E-C5 (D2, no year)   -> unattributable, collections, E is new
#         F-C4 (D1 2022 + D2 2005) -> spans BOTH groups: overall min 2005 so the
#                                 total says digitisation, but the platform
#                                 stream's own contribution is 2022 = fieldwork
gt <- grain(list(
  list("A", "C1", "D1", 2010, 20, 2),
  list("A", "C2", "D1", 2011,  5, 1),
  list("A", "C4", "D1", 2022,  4, 1),
  list("B", "C1", "D2", 2012,  3, 1),
  list("D", "C2", "D3", 1995,  2, 1),
  list("E", "C5", "D2",   NA,  1, 1),
  list("F", "C4", "D1", 2022,  2, 1),
  list("F", "C4", "D2", 2005,  3, 1)
))

pf <- closure_pairs(gf); pt <- closure_pairs(gt)
gained <- data.table::fsetdiff(pt, pf)
lost   <- data.table::fsetdiff(pf, pt)

cli_h2("grain construction (the integer/Inf trap)")
# The case that would otherwise stop script 14: a (species, cell, dataset)
# group where EVERY record lacks a year. min() returns
# an integer for groups that have one and Inf - a double - for this one, and
# data.table refuses the type mismatch. `year` arrives from parquet as an
# INTEGER, which is exactly when it bites.
cube <- data.table::as.data.table(list(
  specieskey  = c("A", "A", "B",  "B",  "C"),
  eeacellcode = c("C1", "C1", "C2", "C2", "C3"),
  datasetkey  = c("D1", "D1", "D2", "D2", "D1"),
  year        = c(2010L, 2014L, NA_integer_, NA_integer_, 2019L),   # integer!
  occurrences = c(1L, 2L, 3L, 4L, 5L)
))
bg <- try(closure_build_grain(cube, PLATFORMS), silent = TRUE)
ok(!inherits(bg, "try-error"),
   "a group whose records ALL lack a year does not blow up the aggregation")
if (!inherits(bg, "try-error")) {
  eq(nrow(bg), 3L, "one grain row per (species, cell, dataset)")
  ok(is.double(bg$min_year), "min_year is double for every group, never integer")
  ok(is.na(bg[key == "B", min_year]), "the year-less group gets NA, not Inf")
  eq(bg[key == "A", min_year], 2010, "the earliest year wins where there is one")
  eq(bg[key == "A", occ], 3, "occurrences are summed within the group")
  eq(bg[key == "A", n_rec], 2L, "records are counted")
  eq(bg[key == "A", source_group], "observation_platforms", "D1 is a platform")
  eq(bg[key == "B", source_group], "collections", "D2 is not")
  ok(!any(is.infinite(bg$min_year)), "no Inf survives into the grain")
}
ok(inherits(try(closure_build_grain(cube[, .(specieskey)], PLATFORMS), silent = TRUE),
            "try-error"),
   "a cube missing columns is refused by name, not by a downstream crash")

cli_h2("re-keying, and the full chain script 14 walks")
# Script 14 chains these steps in code no unit test reaches, and a mismatch
# between them (a column one step renames and the next still expects) passes
# every per-function test. Walking the same sequence here - build grain,
# re-key, difference - is what closes that hole.
cube_from <- data.table::as.data.table(list(
  specieskey  = c("S1", "S2", "S3", "S9"),
  eeacellcode = c("C1", "C1", "C2", "C3"),
  datasetkey  = c("D1", "D1", "D2", "D1"),
  year        = c(2010L, 2011L, 2012L, 2015L),
  occurrences = c(10L, 5L, 3L, 7L)
))
cube_to <- data.table::as.data.table(list(
  specieskey  = c("S1", "S2", "S3", "S1", "S3"),
  eeacellcode = c("C1", "C1", "C2", "C4", "C3"),
  datasetkey  = c("D1", "D1", "D2", "D1", "D2"),
  year        = c(2010L, 2011L, 2012L, 2022L, 1995L),
  occurrences = c(20L, 5L, 3L, 4L, 2L)
))
# S1 and S2 are synonyms of one national taxon; S9 is not on the checklist.
lookup <- data.table::as.data.table(list(
  specieskey       = c("S1", "S2", "S3"),
  backbone_taxonID = c("T1", "T1", "T2")))

gf2 <- closure_build_grain(cube_from, "D1")
gt2 <- closure_build_grain(cube_to,   "D1")

rk_g_from <- closure_rekey(gf2, lookup, "gbif")
eq(rk_g_from$n_dropped, 0L, "gbif space drops nothing")
eq(sort(unique(rk_g_from$grain$key)), c("S1", "S2", "S3", "S9"),
   "gbif space keeps the specieskey as the key")

rk_d_from <- closure_rekey(gf2, lookup, "dyntaxa")
rk_d_to   <- closure_rekey(gt2, lookup, "dyntaxa")
eq(rk_d_from$n_dropped, 1L, "S9 has no taxonID and is dropped, and counted")
eq(sort(unique(rk_d_from$grain$key)), c("T1", "T2"), "S1 and S2 collapse onto T1")
eq(nrow(closure_pairs(rk_d_from$grain)), 2L,
   "two specieskeys in one cell become ONE (taxon, cell) pair")
eq(nrow(closure_pairs(rk_g_from$grain)), 4L, "gbif space still sees four pairs")

# A duplicated specieskey in the lookup would fan the grain out and inflate
# every occurrence count, with no error anywhere.
bad_lookup <- rbind(lookup, data.table::as.data.table(
  list(specieskey = "S1", backbone_taxonID = "T1")))
eq(sum(closure_rekey(gf2, bad_lookup, "dyntaxa")$grain$occ),
   sum(closure_rekey(gf2, lookup, "dyntaxa")$grain$occ),
   "a duplicated lookup row does not inflate occurrences")

# ...and now the whole chain, exactly as 14 runs it.
for (ks in c("gbif", "dyntaxa")) {
  a <- closure_rekey(gf2, lookup, ks)$grain
  b <- closure_rekey(gt2, lookup, ks)$grain
  pa <- closure_pairs(a); pb <- closure_pairs(b)
  cl <- closure_cells(closure_cell_view(a), closure_cell_view(b),
                      c("C1", "C2", "C3", "C4"))
  sp2 <- closure_species(pa, pb)
  me <- closure_mechanism(data.table::fsetdiff(pb, pa), b, 2021L)
  sm2 <- closure_summary(pa, pb, cl, sp2, me, key_space = ks, baseline_year = 2021L)
  ok(nrow(sm2) > 0L, sprintf("%s: the full build -> re-key -> difference chain runs", ks))
  eq(sm2[metric == "pairs_gained" & source_group == "total", value], 2,
     sprintf("%s: two pairs gained", ks))
}
# The loss is visible in GBIF space and invisible in Dyntaxa space, because S9
# is off the checklist entirely. That is why n_dropped has to be reported.
eq(nrow(data.table::fsetdiff(closure_pairs(rk_g_from$grain),
                             closure_pairs(closure_rekey(gt2, lookup, "gbif")$grain))), 1L,
   "gbif space sees the off-checklist species disappear")
eq(nrow(data.table::fsetdiff(closure_pairs(rk_d_from$grain),
                             closure_pairs(rk_d_to$grain))), 0L,
   "dyntaxa space cannot see it, because it was never in that space")

cli_h2("mechanism survives an integer min_year")
gt_int <- data.table::copy(gt)
gt_int[, min_year := as.integer(min_year)]
ok(!inherits(try(closure_mechanism(gained, gt_int, BASELINE_YEAR), silent = TRUE),
             "try-error"),
   "closure_mechanism coerces min_year itself rather than trusting the caller")

cli_h2("pairs")
eq(nrow(pf), 4L, "4 pairs at the baseline")
eq(nrow(pt), 7L, "7 pairs at the comparison point")
eq(nrow(gained), 4L, "4 pairs gained")
eq(nrow(lost), 1L, "1 pair lost")

cli_h2("cells")
cells <- closure_cells(closure_cell_view(gf), closure_cell_view(gt), ALL_CELLS)
eq(nrow(cells), 5L, "every grid cell is present, including the ones with no data")
eq(cells[eeacellcode == "C1", status], "deepened", "C1 13 -> 23 is deepened")
eq(cells[eeacellcode == "C2", status], "deepened", "C2 5 -> 7 is deepened")
eq(cells[eeacellcode == "C3", status], "regressed", "C3 7 -> 0 is regressed")
eq(cells[eeacellcode == "C4", status], "filled", "C4 0 -> 9 is filled")
eq(cells[eeacellcode == "C5", status], "filled", "C5 0 -> 1 is filled")
eq(cells[eeacellcode == "C4", occ_to], 9, "C4 sums both datasets of F plus A")
eq(cells[eeacellcode == "C4", n_species_to], 2L, "C4 holds two species at the end")
eq(sum(cells$status == "filled"), 2L, "2 cells filled")
eq(sum(cells$occ_from), 25, "baseline occurrences preserved")
eq(sum(cells$occ_to), 40, "comparison occurrences preserved (23+7+9+1)")

cli_h2("species")
sp <- closure_species(pf, pt)
eq(sp[key == "A", status], "expanded", "A gained a cell and lost none")
eq(sp[key == "B", status], "unchanged", "B held still")
eq(sp[key == "C", status], "lost", "C disappeared")
eq(sort(sp[status == "new", key]), c("D", "E", "F"), "three species newly recorded")
eq(sp[key == "A", cells_gained], 1L, "A gained exactly one cell")

# A species that swaps one cell for another has delta 0 but is NOT unchanged.
sp_shift <- closure_species(
  data.table::as.data.table(list(key = "Z", eeacellcode = "C1")),
  data.table::as.data.table(list(key = "Z", eeacellcode = "C2")))
eq(sp_shift[key == "Z", status], "shifted", "a cell-for-cell swap reads as shifted, not unchanged")

cli_h2("mechanism")
mech <- closure_mechanism(gained, gt, BASELINE_YEAR)
tot <- mech[source_group == "total"]
eq(tot[mechanism == "fieldwork", n_pairs], 1L, "total: 1 fieldwork pair (A-C4)")
eq(tot[mechanism == "digitisation", n_pairs], 2L, "total: 2 digitisation pairs (D-C2, F-C4)")
eq(tot[mechanism == "unattributable", n_pairs], 1L, "total: 1 unattributable pair (E-C5)")
eq(sum(tot$n_pairs), 4L, "total mechanism counts sum to the gained pairs")

plat <- mech[source_group == "observation_platforms"]
coll <- mech[source_group == "collections"]
eq(plat[mechanism == "fieldwork", n_pairs], 2L,
   "platforms: 2 fieldwork - F-C4 counts here at ITS OWN min year, not the pair's")
eq(nrow(plat[mechanism == "digitisation"]), 0L, "platforms contributed no digitisation")
eq(coll[mechanism == "digitisation", n_pairs], 2L, "collections: 2 digitisation")
eq(coll[mechanism == "unattributable", n_pairs], 1L, "collections: 1 unattributable")
ok(sum(plat$n_pairs) + sum(coll$n_pairs) > sum(tot$n_pairs),
   "per-group counts exceed the total, because F-C4 spans both groups")

cli_h2("dataset concentration (no tie-break)")
dsc <- closure_dataset_concentration(gained, gt)
eq(dsc[datasetkey == "D1", touched], 2L, "D1 touches A-C4 and F-C4")
eq(dsc[datasetkey == "D1", vanish_if_dropped], 1L,
   "dropping D1 removes only A-C4: F-C4 survives on D2")
eq(dsc[datasetkey == "D2", touched], 2L, "D2 touches E-C5 and F-C4")
eq(dsc[datasetkey == "D2", vanish_if_dropped], 1L, "dropping D2 removes only E-C5")
eq(dsc[datasetkey == "D3", vanish_if_dropped], 1L, "D3 solely supports D-C2")
ok(sum(dsc$vanish_if_dropped) <= nrow(gained),
   "pairs that vanish never exceed pairs gained")

cli_h2("frozen thresholds")
fz <- closure_frozen_low(cells, q = 0.10)
eq(unique(fz$q_frozen), as.numeric(stats::quantile(c(13, 5, 7), 0.10, names = FALSE)),
   "threshold comes from the BASELINE's non-zero cells only")
eq(fz[eeacellcode == "C2", low_status], "improved", "C2 was below the frozen threshold and rose above it")
eq(fz[eeacellcode == "C5", low_status], "worsened", "C5 is newly occupied but below the frozen threshold")
eq(fz[eeacellcode == "C1", low_status], "not_low", "C1 was never low")

cli_h2("summary")
sm <- closure_summary(pf, pt, cells, sp, mech, key_space = "gbif",
                      baseline_year = BASELINE_YEAR)
val <- function(m) sm[metric == m & source_group == "total", value]
eq(val("pairs_gained"), 4, "summary: pairs gained")
eq(val("pairs_lost"), 1, "summary: pairs lost")
eq(val("pairs_net"), 3, "summary: net change")
eq(val("cells_filled"), 2, "summary: cells filled")
eq(val("cells_regressed"), 1, "summary: cells regressed")
eq(val("cells_universe"), 5, "summary: the universe is the grid, not the data")
eq(val("cells_never_filled"), 0, "summary: no cell was empty at BOTH ends")
eq(val("cells_empty_now"), 1, "summary: one cell is empty now - C3, which regressed")
eq(val("species_newly_recorded"), 3, "summary: species newly recorded")
eq(val("species_no_longer_recorded"), 1, "summary: species no longer recorded")
eq(val("mechanism_boundary_year"), 2021, "summary: the boundary year is recorded, not implied")
ok(all(sm$key_space == "gbif"), "every summary row is tagged with its key space")

cli_h2("empty-input guards")
empty_pairs <- data.table::as.data.table(list(key = character(), eeacellcode = character()))
ok(nrow(closure_mechanism(empty_pairs, gt, BASELINE_YEAR)) == 0L,
   "no gained pairs -> an empty mechanism table, not an error")
ok(nrow(closure_dataset_concentration(empty_pairs, gt)) == 0L,
   "no gained pairs -> an empty concentration table, not an error")
ok(inherits(try(closure_cells(closure_cell_view(gf), closure_cell_view(gt),
                              character()), silent = TRUE), "try-error"),
   "an empty cell universe is refused, not silently taken from the data")

cli_h2("closure_taxon_lookup(): the lost taxa must still have names")
# The failure this guards against: annotating from the LATER match table alone
# leaves every lost taxon with NA name/class/order, and the group roll-up then
# collects all of them into one (NA, NA) row at 100% loss.
TAX_COLS <- c("class", "order", "backbone_scientificName", "taxonRank")
mt <- function(ids, cls, ord, nm, cols = TRUE) {
  d <- data.table::as.data.table(list(
    specieskey = paste0("gk", ids), backbone_taxonID = as.character(ids),
    class = cls, order = ord, backbone_scientificName = nm))
  if (cols) d[, taxonRank := "species"]
  d[]
}
# `to` has lost taxon 3 and renamed taxon 1; `from` has all three.
mt_to   <- mt(c(1, 2), c("Aves", "Insecta"), c("Passeriformes", "Diptera"),
              c("Corvus cornix", "Eristalis nemorum"))
mt_from <- mt(c(1, 2, 3), c("Aves", "Insecta", "Eudicots"),
              c("Passeriformes", "Diptera", "Rosales"),
              c("Corvus corone cornix", "Eristalis nemorum", "Rosa mollis"))

tl <- closure_taxon_lookup(mt_to, mt_from, TAX_COLS, "backbone_taxonID")
eq(tl$n_fallback, 1L, "lookup: one key annotated from the earlier taxonomy")
eq(nrow(tl$lookup), 3L, "lookup: covers taxa present at either end")
ok(uniqueN(tl$lookup$key) == nrow(tl$lookup), "lookup: unique on key (no fan-out)")
eq(tl$lookup[key == "1", backbone_scientificName], "Corvus cornix",
   "lookup: a taxon at BOTH ends keeps the LATER name")
eq(tl$lookup[key == "3", backbone_scientificName], "Rosa mollis",
   "lookup: a LOST taxon is named from the earlier taxonomy")
eq(tl$lookup[key == "3", class], "Eudicots", "lookup: the lost taxon keeps its class")
ok(!any(is.na(tl$lookup$class)), "lookup: no NA class survives")

# The no-op case: `to` is a superset, so nothing falls back.
eq(closure_taxon_lookup(mt_from, mt_to, TAX_COLS, "backbone_taxonID")$n_fallback, 0L,
   "lookup: no fallback when the later taxonomy is a superset")
eq(closure_taxon_lookup(mt_to, NULL, TAX_COLS, "backbone_taxonID")$n_fallback, 0L,
   "lookup: a missing earlier match table is tolerated, not an error")

# A column absent from the earlier table must become NA, not shift the others.
short <- closure_taxon_lookup(mt_to, mt(3, "Eudicots", "Rosales", "Rosa mollis", FALSE),
                              TAX_COLS, "backbone_taxonID")$lookup
ok(is.na(short[key == "3", taxonRank]), "lookup: a column missing upstream becomes NA")
eq(short[key == "3", class], "Eudicots", "lookup: columns do not shift when one is missing")
eq(names(short), c("key", TAX_COLS), "lookup: column names and order are stable")

# A duplicated key in the earlier table must not fan the join out.
dup <- closure_taxon_lookup(
  mt_to, rbind(mt_from, mt(3, "Eudicots", "Rosales", "Rosa villosa")),
  TAX_COLS, "backbone_taxonID")$lookup
ok(uniqueN(dup$key) == nrow(dup), "lookup: a duplicated key upstream cannot fan out")

# gbif key space uses specieskey and behaves identically.
eq(closure_taxon_lookup(mt_to, mt_from, TAX_COLS, "specieskey")$lookup[
     key == "gk3", backbone_scientificName], "Rosa mollis",
   "lookup: gbif key space annotates lost taxa too")
ok(inherits(try(closure_taxon_lookup(mt_to, mt_from, TAX_COLS, "nope"), silent = TRUE),
            "try-error"),
   "lookup: an unknown key column is refused by name")

cli_h2("Result")
if (fail == 0L) { cli_alert_success("All closure checks passed"); quit(status = 0L) }
cli_abort("{fail} closure check{?s} failed")
