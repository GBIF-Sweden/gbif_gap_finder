# scripts/06b_make_species_summaries.R
# ============================================================================
# Species-Level Summaries
# ============================================================================
# Purpose:
#   Create species-level summary tables from GBIF parquet cubes,
#   including relative occurrence bias correction.
#
#   Taxonomy flags (in_dyntaxa, is_invasive, is_sensitive) are NOT
#   applied here. They are added by script 09c after reconciliation,
#   which uses the superior 4-tier matching from 09a.
#
# Strategy:
#   1. Read parquet cube for each grid resolution
#   2. Classify orders as small/large by row count
#   3. Aggregate by order (small) or family (large)
#   4. Add "all" basis rows and relative occurrence metrics
#   5. Write per-taxon CSV files
#
# Outputs (in data/{CC}/proc/derived/):
#   by_order/<type>/<type>_<order>_<grid>.csv
#   by_family/<type>/<type>_<order>_<family>_<grid>.csv
#
#   where <type> is one of:
#     - species_summary      Overall species totals
#     - species_cell         Species x cell
#     - species_time         Species x yearmonth
#     - species_cell_time    Species x cell x yearmonth (optional)
#
# Inputs:  data/{CC}/proc/cubes/*.parquet (from 04)
# Config:  parameters.processing.large_order_threshold (default 500000)
#          parameters.processing.make_species_cell_time (default false)
#
# Dependencies: scripts/00_setup.R, data.table, arrow, stringr
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# Script-specific package
library(arrow)

# ============================================================================
# Configuration
# ============================================================================

LARGE_ORDER_THRESHOLD <- cfg_get("parameters.processing.large_order_threshold", 500000)

MAKE_SPECIES_SUMMARY   <- TRUE
MAKE_SPECIES_CELL      <- TRUE
MAKE_SPECIES_TIME      <- TRUE
MAKE_SPECIES_CELL_TIME <- cfg_get("parameters.processing.make_species_cell_time", FALSE)

# p_cubes, p_derived are defined in R/globals.R
# Directories created by ensure_dirs() in 00_setup.R

# ============================================================================
# Helper Functions
# ============================================================================

# read_cube(), safe_sum(), safe_max(), clean_for_filename() are in R/globals.R

# ---------------------------------------------------------------------------
# Scope filter: restrict the cube to the backbone's taxonomic scope
# ---------------------------------------------------------------------------
# Drops occurrence rows whose KINGDOM is absent from the national backbone
# (Bacteria, Archaea, Viruses, and any protist/chromist kingdom the backbone
# doesn't cover) -- groups that can never match in reconciliation and only
# inflate totals. KINGDOM is used (not class) because it is a small, stable
# vocabulary: reptiles stay (Animalia is in the backbone) even though their
# class is blank, so they survive to be bucketed as "Unclassified" downstream.
# NA/blank kingdom is KEPT. Allowed kingdoms are read once from the backbone and
# cached. Config-gated. Promote this block to R/globals.R for a single shared
# copy (the guard keeps a globals copy authoritative if present).
if (!exists("read_cube_scoped")) {
  .scope_allowed_kingdoms <- function() {
    a <- getOption("gapfinder.scope_kingdoms", NULL)
    if (is.null(a)) {
      a <- character(0)
      bb_path <- here(p_data_proc, "taxa_reference_current.rds")
      if (file.exists(bb_path)) {
        bb <- data.table::as.data.table(readRDS(bb_path))
        kcol <- intersect(c("kingdom", "Kingdom"), names(bb))[1]
        if (!is.na(kcol)) {
          a <- unique(as.character(bb[[kcol]]))
          a <- a[!is.na(a) & a != ""]
        }
      }
      options(gapfinder.scope_kingdoms = a)
    }
    a
  }
  scope_filter_rows <- function(dt, label = "cube") {
    if (!isTRUE(cfg_get("parameters.taxonomic.restrict_to_backbone_scope", TRUE)) ||
        !"kingdom" %in% names(dt)) return(dt)
    allowed <- .scope_allowed_kingdoms()
    if (length(allowed) == 0) return(dt)
    n0 <- nrow(dt)
    dt <- dt[is.na(kingdom) | kingdom == "" | kingdom %in% allowed]
    cli_alert_info(
      "Scope filter [{label}]: kept \\
       {scales::comma(nrow(dt))}/{scales::comma(n0)} rows (in-backbone kingdoms)"
    )
    dt
  }
  read_cube_scoped <- function(pf, cols, grid_label = NULL, ...) {
    on <- isTRUE(cfg_get("parameters.taxonomic.restrict_to_backbone_scope", TRUE))
    want_kingdom <- "kingdom" %in% cols
    cols2 <- if (on && !want_kingdom) unique(c(cols, "kingdom")) else cols
    dt <- read_cube(pf, cols = cols2, grid_label = grid_label, ...)
    if (on && "kingdom" %in% names(dt)) {
      dt <- scope_filter_rows(dt)
      if (!want_kingdom && "kingdom" %in% names(dt)) dt[, kingdom := NULL]
    }
    dt
  }
}

#' Add relative occurrence columns (bias correction)
#' Computed from order/family/class totals in the data
add_relative_occurrences <- function(dt) {
  if ("order" %in% names(dt)) {
    order_totals <- dt[, .(ordercount = safe_sum(occurrences)),
                         by = .(grid, basisofrecord, order)]
    dt <- merge(dt, order_totals, by = c("grid", "basisofrecord", "order"), all.x = TRUE)
    dt[, relative_order := fifelse(ordercount > 0, occurrences / ordercount, NA_real_)]
  }
  if ("family" %in% names(dt)) {
    family_totals <- dt[, .(familycount = safe_sum(occurrences)),
                          by = .(grid, basisofrecord, family)]
    dt <- merge(dt, family_totals, by = c("grid", "basisofrecord", "family"), all.x = TRUE)
    dt[, relative_family := fifelse(familycount > 0, occurrences / familycount, NA_real_)]
  }
  if ("class" %in% names(dt)) {
    class_totals <- dt[, .(classcount = safe_sum(occurrences)), by = .(grid, basisofrecord, class)]
    dt <- merge(dt, class_totals, by = c("grid", "basisofrecord", "class"), all.x = TRUE)
    dt[, relative_class := fifelse(classcount > 0, occurrences / classcount, NA_real_)]
  }
  dt
}

#' Write summary type: combine, add "all" basis, add bias, write per grid
write_summary_type <- function(dt, group_cols, type_name, name_clean, out_subdir, grid_suffix) {
  if (nrow(dt) == 0) return()

  # Add "all" basis
  group_no_basis <- setdiff(group_cols, "basisofrecord")
  all_basis <- dt[, .(occurrences = safe_sum(occurrences), basisofrecord = "all"),
                    by = group_no_basis]
  combined <- rbindlist(list(dt, all_basis), use.names = TRUE, fill = TRUE)

  # Bias correction
  combined <- add_relative_occurrences(combined)

  # Write
  out_dir <- here(p_derived, out_subdir, type_name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, glue("{type_name}_{name_clean}_{grid_suffix}.csv"))
  fwrite(combined, out_path)

  rm(combined, all_basis)
}

# ============================================================================
# Locate Parquet Files
# ============================================================================

cli_h1("Species-Level Summaries (Script 06b)")

# Full rebuild: clear the per-taxon output dirs first. Filenames are keyed by
# order/family, so when the cube changes — COL renames a group, or a smaller
# re-download drops one — the old file lingers as an ORPHAN. 09a/09b/09c/11 glob
# derived/ for species_summary*.csv, so orphans are read as PHANTOM species and
# silently inflate the reconciliation universe (fresh cube 76,589 -> 80,940 seen
# on 2026-07-30). These two dirs are written only by this script, so wiping them
# guarantees the species universe on disk equals the current cube.
for (stale_dir in c(here(p_derived, "by_order"), here(p_derived, "by_family"))) {
  if (dir.exists(stale_dir)) {
    unlink(stale_dir, recursive = TRUE, force = TRUE)
    cli_alert_info("Cleared stale per-taxon outputs: {.path {stale_dir}}")
  }
}

parquet_files <- list.files(p_cubes, pattern = "\\.parquet$", full.names = TRUE)
if (length(parquet_files) == 0) cli_abort("No parquet files in: {.path {p_cubes}}")

grid_map <- list()
for (pf in parquet_files) {
  bn <- basename(pf)
  if (grepl("10km", bn)) grid_map[["grid10km"]] <- pf
  else if (grepl("50km", bn)) grid_map[["grid50km"]] <- pf
}

# ============================================================================
# Process each grid resolution
# ============================================================================

for (grid_name in names(grid_map)) {
  grid_suffix <- str_extract(grid_name, "\\d+km")
  pf <- grid_map[[grid_name]]

  cli_h2("{grid_name}: Loading cube")

  # Read all needed columns at once
  cols_needed <- c("specieskey", "species", "basisofrecord",
    "eeacellcode", "year", "month",
    "order", "family", "class", "kingdom", "phylum",
    "occurrences")

  ds <- open_dataset(pf)
  available <- intersect(cols_needed, names(ds$schema))
  dt <- as.data.table(ds |> select(all_of(available)) |> collect())

  # Create yearmonth
  if (all(c("year", "month") %in% names(dt))) {
    dt[, yearmonth := year * 100L + as.integer(month)]
  }
  dt[, grid := grid_name]

  # Restrict to the backbone's taxonomic scope (drops occurrence rows whose
  # kingdom is absent from the backbone; NA/blank kept, so reptiles survive).
  # Keeps the species summaries -> 09a -> the whole pipeline on the same
  # in-scope universe as 06a/08.
  dt <- scope_filter_rows(dt)

  # Recode missing taxonomy
  dt[is.na(order)  | order  == "", order  := "Unplaced"]
  dt[is.na(family) | family == "", family := "Unplaced"]

  cli_alert_info("{scales::comma(nrow(dt))} rows, {uniqueN(dt$specieskey)} species")

  # ========================================================================
  # Step 1: Classify orders by size
  # ========================================================================

  cli_h3("Classifying orders")
  order_sizes <- dt[, .N, by = order]
  large_orders <- order_sizes[N > LARGE_ORDER_THRESHOLD, order]
  small_orders <- order_sizes[N <= LARGE_ORDER_THRESHOLD, order]

  cli_alert_info(
    "Small orders: \\
     {length(small_orders)}, Large orders (by family): {length(large_orders)}"
  )
  if (length(large_orders) > 0) cli_alert_info(
    "Large: {paste(head(large_orders, 5), collapse = ', ')}"
  )

  # ========================================================================
  # Step 2: Process small orders (aggregate by order)
  # ========================================================================

  cli_h3("Processing small orders")

  dt_small <- dt[order %in% small_orders]
  n_small_orders <- length(small_orders)
  n_done <- 0

  for (ord in small_orders) {
    n_done <- n_done + 1
    chunk <- dt_small[order == ord]
    if (nrow(chunk) == 0) next
    name_clean <- clean_for_filename(ord)

    group_base <- c("grid", "basisofrecord", "specieskey", "species", "family", "class")

    if (MAKE_SPECIES_SUMMARY) {
      agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                     by = intersect(group_base, names(chunk))]
      write_summary_type(
        agg, group_base, "species_summary", name_clean, "by_order", grid_suffix
      )
    }
    if (MAKE_SPECIES_CELL && "eeacellcode" %in% names(chunk)) {
      agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                     by = intersect(c(group_base, "eeacellcode"), names(chunk))]
      write_summary_type(
        agg, c(group_base, "eeacellcode"), "species_cell", name_clean, "by_order", grid_suffix
      )
    }
    if (MAKE_SPECIES_TIME && "yearmonth" %in% names(chunk)) {
      agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                     by = intersect(c(group_base, "yearmonth"), names(chunk))]
      write_summary_type(
        agg, c(group_base, "yearmonth"), "species_time", name_clean, "by_order", grid_suffix
      )
    }
    if (MAKE_SPECIES_CELL_TIME && "eeacellcode" %in% names(chunk) &&
      "yearmonth" %in% names(chunk)) {
      agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                     by = intersect(c(group_base, "eeacellcode", "yearmonth"), names(chunk))]
      write_summary_type(
        agg, c(group_base, "eeacellcode", "yearmonth"), "species_cell_time", name_clean, "by_order",
        grid_suffix
      )
    }

    if (n_done %% 20 == 0) cli_alert_info("  {n_done}/{n_small_orders} orders done")
  }

  cli_alert_success("Small orders: {n_done} processed")
  rm(dt_small); gc()

  # ========================================================================
  # Step 3: Process large orders (aggregate by family)
  # ========================================================================

  if (length(large_orders) > 0) {
    cli_h3("Processing large orders (by family)")

    dt_large <- dt[order %in% large_orders]
    family_combos <- unique(dt_large[, .(order, family)])
    n_families <- nrow(family_combos)
    n_done <- 0

    for (i in seq_len(n_families)) {
      ord <- family_combos$order[i]
      fam <- family_combos$family[i]
      n_done <- n_done + 1

      chunk <- dt_large[order == ord & family == fam]
      if (nrow(chunk) == 0) next

      name_clean <- paste0(clean_for_filename(ord), "_", clean_for_filename(fam))
      group_base <- c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class")

      if (MAKE_SPECIES_SUMMARY) {
        agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                       by = intersect(group_base, names(chunk))]
        write_summary_type(
          agg, group_base, "species_summary", name_clean, "by_family", grid_suffix
        )
      }
      if (MAKE_SPECIES_CELL && "eeacellcode" %in% names(chunk)) {
        agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                       by = intersect(c(group_base, "eeacellcode"), names(chunk))]
        write_summary_type(
          agg, c(group_base, "eeacellcode"), "species_cell", name_clean, "by_family", grid_suffix
        )
      }
      if (MAKE_SPECIES_TIME && "yearmonth" %in% names(chunk)) {
        agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                       by = intersect(c(group_base, "yearmonth"), names(chunk))]
        write_summary_type(
          agg, c(group_base, "yearmonth"), "species_time", name_clean, "by_family", grid_suffix
        )
      }
      if (MAKE_SPECIES_CELL_TIME && "eeacellcode" %in% names(chunk) &&
        "yearmonth" %in% names(chunk)) {
        agg <- chunk[, .(occurrences = safe_sum(occurrences)),
                       by = intersect(c(group_base, "eeacellcode", "yearmonth"), names(chunk))]
        write_summary_type(
          agg, c(group_base, "eeacellcode", "yearmonth"), "species_cell_time", name_clean,
          "by_family", grid_suffix
        )
      }

      if (n_done %% 20 == 0) cli_alert_info("  {n_done}/{n_families} families done")
    }

    cli_alert_success("Large orders: {n_done} families processed")
    rm(dt_large); gc()
  }

  rm(dt); gc()
}

# ============================================================================
# Summary
# ============================================================================

cli_h1("Summary")

by_order_files <- length(
  list.files(here(p_derived, "by_order"), pattern = "\\.csv$", recursive = TRUE)
)
by_family_files <- length(
  list.files(here(p_derived, "by_family"), pattern = "\\.csv$", recursive = TRUE)
)

cli_alert_success("Species summaries complete!")
cli_alert_info("Files in by_order/:  {by_order_files}")
cli_alert_info("Files in by_family/: {by_family_files}")
cli_alert_info("Total: {by_order_files + by_family_files}")
cli_alert_info("Next: source('scripts/07_spatial_gaps.R')")
