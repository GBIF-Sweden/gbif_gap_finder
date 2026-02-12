# scripts/06b_make_species_summaries_highmem.R
# ============================================================================
# Species-Level Summaries (Optimised for ≥16 GB RAM)
# ============================================================================
# Purpose:
#   Create species-level summary tables from GBIF cubes, including
#   relative occurrence bias correction. Uses a single-pass strategy
#   with an accumulator environment for memory efficiency.
#
# Strategy:
#   1. Scan all cube files to classify orders as small/large
#   2. Single pass: aggregate by order (small) or family (large)
#   3. Write per-taxon CSV files with "all" basis rows and
#      relative occurrence metrics
#
# Outputs (in data_proc/derived/):
#   by_order/<type>/<type>_<order>_<grid>.csv
#   by_family/<type>/<type>_<order>_<family>_<grid>.csv
#
#   where <type> is one of:
#     - species_summary      Overall species totals
#     - species_cell         Species × cell
#     - species_time         Species × yearmonth
#     - species_cell_time    Species × cell × yearmonth
#
# Inputs:  data_proc/cubes/*.fst (from 04)
# Config:  parameters.processing.large_order_threshold (default 500000)
#          parameters.processing.make_species_cell_time (default true)
#
# Memory: Requires ≥16 GB RAM. For <16 GB, use 06b_*_lowmem.R instead.
#
# Dependencies: scripts/00_setup.R, data.table, fst, stringr
# ============================================================================

library(here)
library(dplyr)
library(stringr)
library(data.table)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# ============================================================================
# Configuration
# ============================================================================

LARGE_ORDER_THRESHOLD <- cfg_get(
  "parameters.processing.large_order_threshold", 500000
)

MAKE_SPECIES_SUMMARY   <- TRUE
MAKE_SPECIES_CELL      <- TRUE
MAKE_SPECIES_TIME      <- TRUE
MAKE_SPECIES_CELL_TIME <- cfg_get(
  "parameters.processing.make_species_cell_time", TRUE
)

# Paths
p_cubes   <- here(p_data_proc, "cubes")
p_derived <- here(p_data_proc, "derived")

dir.create(here(p_derived, "by_order"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_family"), showWarnings = FALSE, recursive = TRUE)

# Validate inputs
if (!dir.exists(p_cubes)) {
  cli_abort("Cubes directory not found: {.path {p_cubes}}")
}
if (!requireNamespace("fst", quietly = TRUE)) {
  cli_abort("Package {.pkg fst} is required")
}

# ============================================================================
# Helper Functions
# ============================================================================

safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(as.numeric(x), na.rm = TRUE)
}

read_fst_cols <- function(path, cols) {
  meta         <- fst::metadata_fst(path)
  cols_to_read <- intersect(cols, meta$columnNames)
  df <- fst::read_fst(path, columns = cols_to_read)
  setDT(df)
  df
}

clean_for_filename <- function(x) {
  x <- str_replace_all(x, "[^A-Za-z0-9]", "_")
  x <- str_replace_all(x, "_+", "_")
  str_remove(x, "^_|_$")
}

#' Add relative occurrence columns (bias correction)
#'
#' Divides species occurrences by family/order/class totals
#' to produce a normalised measure.
add_relative_occurrences <- function(dt) {
  if ("familycount" %in% names(dt)) {
    dt[, relative_family := fifelse(
      familycount > 0, occurrences / familycount, NA_real_
    )]
  }
  if ("ordercount" %in% names(dt)) {
    dt[, relative_order := fifelse(
      ordercount > 0, occurrences / ordercount, NA_real_
    )]
  }
  if ("classcount" %in% names(dt)) {
    dt[, relative_class := fifelse(
      classcount > 0, occurrences / classcount, NA_real_
    )]
  }
  dt
}

# ============================================================================
# Step 1: Scan Order Sizes
# ============================================================================

cli_h1("Species-Level Summaries (Optimised)")
cli_h2("Step 1: Scanning Order Sizes")

cube_files <- list.files(
  p_cubes, pattern = "\\.fst$", full.names = TRUE
)
cli_alert_info("Found {length(cube_files)} cube files")

order_sizes <- list()
family_info <- list()

cli_progress_bar(
  "Scanning", total = length(cube_files), clear = FALSE
)

for (f in cube_files) {
  cli_progress_update()
  dt <- read_fst_cols(f, c("grid", "order", "family"))

  if ("order" %in% names(dt)) {
    # Recode missing order/family as "Unplaced" so these species
    # are not silently dropped from downstream analyses
    dt[is.na(order) | order == "", order := "Unplaced"]
    if ("family" %in% names(dt)) {
      dt[is.na(family) | family == "", family := "Unplaced"]
    }

    order_sizes[[f]] <- dt[, .N, by = .(grid, order)]
    if ("family" %in% names(dt)) {
      family_info[[f]] <- dt[, .(grid, order, family)] |> unique()
    }
  }
  rm(dt); gc()
}

cli_progress_done()

order_totals  <- rbindlist(order_sizes)[
  , .(total_rows = sum(N)), by = .(grid, order)
]
family_lookup <- rbindlist(family_info) |> unique()

# Classify orders by size
large_order_names <- order_totals[
  total_rows > LARGE_ORDER_THRESHOLD, unique(order)
]
small_order_names <- order_totals[
  total_rows <= LARGE_ORDER_THRESHOLD, unique(order)
]
grid_levels <- unique(order_totals$grid)

cli_alert_info("Small orders: {length(small_order_names)}")
cli_alert_info(
  "Large orders (split by family): {length(large_order_names)}"
)

if (length(large_order_names) > 0) {
  cli_alert_info(
    "Large: {paste(head(large_order_names, 5), collapse = ', ')}"
  )
}

# ============================================================================
# Step 2: Single-Pass Aggregation
# ============================================================================

cli_h2("Step 2: Aggregating (single pass)")

# Accumulator environment (fast reference semantics)
accum <- new.env(hash = TRUE)

init_accum <- function(key) {
  if (!exists(key, envir = accum)) {
    accum[[key]] <- list(
      summary   = list(),
      cell      = list(),
      time      = list(),
      cell_time = list()
    )
  }
}

cols_species <- c(
  "grid", "basisofrecord", "eeacellcode", "yearmonth",
  "specieskey", "species", "family", "order", "class",
  "occurrences", "familycount", "ordercount", "classcount"
)

cli_progress_bar(
  "Processing cubes",
  total = length(cube_files), clear = FALSE
)

for (f in cube_files) {
  cli_progress_update()

  dt <- read_fst_cols(f, cols_species)

  if (!("order" %in% names(dt)) ||
      !("specieskey" %in% names(dt))) {
    rm(dt); next
  }

  # Ensure required columns exist
  if (!("grid" %in% names(dt)))          dt[, grid := "grid10km"]
  if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := "unknown"]
  if (!("family" %in% names(dt)))        dt[, family := NA_character_]
  if (!("class" %in% names(dt)))         dt[, class := NA_character_]
  if (!("species" %in% names(dt)))       dt[, species := NA_character_]
  if (!("familycount" %in% names(dt)))   dt[, familycount := NA_real_]
  if (!("ordercount" %in% names(dt)))    dt[, ordercount := NA_real_]
  if (!("classcount" %in% names(dt)))    dt[, classcount := NA_real_]

  have_cell <- "eeacellcode" %in% names(dt)
  have_time <- "yearmonth"   %in% names(dt)

  # Recode missing order/family as "Unplaced" (consistent with scanning step)
  dt[is.na(order) | order == "", order := "Unplaced"]
  dt[is.na(family) | family == "", family := "Unplaced"]
  if (nrow(dt) == 0) { rm(dt); next }

  dt[, is_large_order := order %in% large_order_names]

  # --- Small orders (by order) ---
  dt_small <- dt[is_large_order == FALSE]

  if (nrow(dt_small) > 0) {
    orders_in_chunk <- unique(dt_small$order)

    for (ord in orders_in_chunk) {
      key <- paste0("order|", ord)
      init_accum(key)
      chunk <- dt_small[order == ord]

      if (MAKE_SPECIES_SUMMARY) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class)]
        accum[[key]]$summary[[length(accum[[key]]$summary) + 1]] <- agg
      }

      if (MAKE_SPECIES_CELL && have_cell) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class, eeacellcode)]
        accum[[key]]$cell[[length(accum[[key]]$cell) + 1]] <- agg
      }

      if (MAKE_SPECIES_TIME && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class, yearmonth)]
        accum[[key]]$time[[length(accum[[key]]$time) + 1]] <- agg
      }

      if (MAKE_SPECIES_CELL_TIME && have_cell && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class, eeacellcode, yearmonth)]
        accum[[key]]$cell_time[[length(accum[[key]]$cell_time) + 1]] <- agg
      }
    }
  }

  # --- Large orders (by family) ---
  dt_large <- dt[is_large_order == TRUE]

  if (nrow(dt_large) > 0) {
    combos <- unique(
      dt_large[, .(order, family)]
    )

    for (i in seq_len(nrow(combos))) {
      ord <- combos$order[i]
      fam <- combos$family[i]
      key <- paste0("family|", ord, "|", fam)
      init_accum(key)
      chunk <- dt_large[order == ord & family == fam]

      if (MAKE_SPECIES_SUMMARY) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class)]
        accum[[key]]$summary[[length(accum[[key]]$summary) + 1]] <- agg
      }

      if (MAKE_SPECIES_CELL && have_cell) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class, eeacellcode)]
        accum[[key]]$cell[[length(accum[[key]]$cell) + 1]] <- agg
      }

      if (MAKE_SPECIES_TIME && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class, yearmonth)]
        accum[[key]]$time[[length(accum[[key]]$time) + 1]] <- agg
      }

      if (MAKE_SPECIES_CELL_TIME && have_cell && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount  = safe_max(ordercount),
          classcount  = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class, eeacellcode, yearmonth)]
        accum[[key]]$cell_time[[length(accum[[key]]$cell_time) + 1]] <- agg
      }
    }
  }

  rm(dt, dt_small, dt_large)
  gc()
}

cli_progress_done()

# ============================================================================
# Step 3: Combine and Write Output Files
# ============================================================================

cli_h2("Step 3: Writing Output Files")

all_keys <- ls(accum)
cli_alert_info("Processing {length(all_keys)} taxon groups")

cli_progress_bar(
  "Writing files", total = length(all_keys), clear = FALSE
)

for (key in all_keys) {
  cli_progress_update()

  data  <- accum[[key]]
  parts <- str_split(key, "\\|")[[1]]
  key_type <- parts[1]

  if (key_type == "order") {
    order_name <- parts[2]
    name_clean <- clean_for_filename(order_name)
    out_subdir <- "by_order"
    group_cols_summary   <- c("grid", "basisofrecord", "specieskey", "species", "family", "class")
    group_cols_cell      <- c(group_cols_summary, "eeacellcode")
    group_cols_time      <- c(group_cols_summary, "yearmonth")
    group_cols_cell_time <- c(group_cols_summary, "eeacellcode", "yearmonth")
  } else {
    order_name  <- parts[2]
    family_name <- parts[3]
    name_clean  <- paste0(
      clean_for_filename(order_name), "_",
      clean_for_filename(family_name)
    )
    out_subdir <- "by_family"
    group_cols_summary   <- c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class")
    group_cols_cell      <- c(group_cols_summary, "eeacellcode")
    group_cols_time      <- c(group_cols_summary, "yearmonth")
    group_cols_cell_time <- c(group_cols_summary, "eeacellcode", "yearmonth")
  }

  # Helper: combine accumulator list, add "all" basis, write per grid
  write_summary_type <- function(agg_list,
                                 group_cols,
                                 type_name) {
    if (length(agg_list) == 0) return()

    combined <- rbindlist(
      agg_list, use.names = TRUE, fill = TRUE
    )

    group_cols <- intersect(group_cols, names(combined))

    # Re-aggregate across files
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount  = safe_max(ordercount),
      classcount  = safe_max(classcount)
    ), by = group_cols]

    # Add "all" basisofrecord
    group_cols_no_basis <- setdiff(group_cols, "basisofrecord")
    all_basis <- combined[, .(
      occurrences   = safe_sum(occurrences),
      familycount   = safe_sum(familycount),
      ordercount    = safe_sum(ordercount),
      classcount    = safe_sum(classcount),
      basisofrecord = "all"
    ), by = group_cols_no_basis]

    combined <- rbindlist(
      list(combined, all_basis), use.names = TRUE, fill = TRUE
    )

    # Bias correction
    combined <- add_relative_occurrences(combined)

    # Write per grid
    for (g in unique(combined$grid)) {
      subset <- combined[grid == g]
      if (nrow(subset) == 0) next

      grid_suffix <- str_extract(g, "\\d+km")
      if (is.na(grid_suffix)) grid_suffix <- g

      out_dir <- here(p_derived, out_subdir, type_name)
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

      out_path <- file.path(
        out_dir,
        glue("{type_name}_{name_clean}_{grid_suffix}.csv")
      )
      fwrite(subset, out_path)
    }

    rm(combined, all_basis)
  }

  # Write each summary type
  if (MAKE_SPECIES_SUMMARY) {
    write_summary_type(
      data$summary, group_cols_summary, "species_summary"
    )
  }
  if (MAKE_SPECIES_CELL) {
    write_summary_type(
      data$cell, group_cols_cell, "species_cell"
    )
  }
  if (MAKE_SPECIES_TIME) {
    write_summary_type(
      data$time, group_cols_time, "species_time"
    )
  }
  if (MAKE_SPECIES_CELL_TIME) {
    write_summary_type(
      data$cell_time, group_cols_cell_time, "species_cell_time"
    )
  }

  # Free memory
  rm(list = key, envir = accum)
  gc()
}

cli_progress_done()

# ============================================================================
# Summary
# ============================================================================

cli_h1("Summary")

by_order_files <- length(list.files(
  here(p_derived, "by_order"),
  pattern = "\\.csv$", recursive = TRUE
))
by_family_files <- length(list.files(
  here(p_derived, "by_family"),
  pattern = "\\.csv$", recursive = TRUE
))

cli_alert_success("Species summaries complete!")
cli_alert_info("Files in by_order/:  {by_order_files}")
cli_alert_info("Files in by_family/: {by_family_files}")
cli_alert_info("Total files: {by_order_files + by_family_files}")
