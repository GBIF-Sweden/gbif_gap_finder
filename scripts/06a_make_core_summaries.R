# scripts/06a_make_core_summaries.R
# ==============================================================================
# Create Core and Order-Level Derived Summaries
# ==============================================================================
# This script creates:
#
# CORE SUMMARIES (single dimension):
#   - cell_summary: occurrences per cell
#   - time_summary: occurrences per yearmonth
#
# TWO-DIMENSIONAL SUMMARIES:
#   - cell_time_summary: occurrences per cell × yearmonth
#   - order_cell_summary: occurrences per order × cell
#   - order_time_summary: occurrences per order × yearmonth
#   - family_time_summary: occurrences per family × yearmonth
#
# Run this script first, then run 06b_make_species_summaries.R for species-level tables.
#
# Memory strategy: Reads only required columns, aggregates per-file,
# then combines aggregates (not raw data).

library(here)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(data.table)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------

# Toggle sections on/off for debugging or partial runs
MAKE_CORE_SUMMARIES <- TRUE
MAKE_CELL_TIME <- TRUE
MAKE_ORDER_SUMMARIES <- TRUE

# Paths -------------------------------------------------------------------
p_cubes <- here(p_data_proc, "cubes")
p_derived <- here(p_data_proc, "derived")

# Create output directories
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)

# Validate inputs ---------------------------------------------------------
if (!dir.exists(p_cubes)) {

  cli_abort("Cubes directory not found: {.path {p_cubes}}")
}

# Check for fst package
if (!requireNamespace("fst", quietly = TRUE)) {
  cli_abort(c(
    "Package {.pkg fst} is required for reading cube files",
    "i" = "Install with: {.code install.packages('fst')}",
    "i" = "Then run: {.code renv::snapshot()}"
  ))
}

# ===========================================================================
# HELPER FUNCTIONS
# ===========================================================================

#' Safely sum numeric values
safe_sum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

#' Safely take max of numeric values
safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(as.numeric(x), na.rm = TRUE)
}

#' Read selected columns from fst file
read_fst_cols <- function(path, cols) {
  meta <- fst::metadata_fst(path)
  available_cols <- meta$columnNames
  cols_to_read <- intersect(cols, available_cols)
  
  if (!("occurrences" %in% available_cols)) {
    cli_abort("Column 'occurrences' missing in: {.path {basename(path)}}")
  }
  
  df <- fst::read_fst(path, columns = cols_to_read)
  setDT(df)
  df
}

#' Write grid-specific subset to CSV
write_grid_subset <- function(dt, grid_value, out_dir, filename) {
  subset <- dt[grid == grid_value]
  
  if (nrow(subset) == 0) {
    cli_alert_info("No data for {grid_value} in {filename} - skipping")
    return(invisible(NULL))
  }
  
  out_path <- here(out_dir, filename)
  fwrite(subset, out_path)
  cli_alert_success("{filename}: {scales::comma(nrow(subset))} rows")
}

# ===========================================================================
# LOCATE CUBE FILES
# ===========================================================================
cli_h1("Core & Order-Level Summaries (Script 06a)")
cli_h2("Locating Cube Files")

cube_files <- list.files(p_cubes, pattern = "\\.fst$", full.names = TRUE)

if (length(cube_files) == 0) {
  cli_abort("No .fst cube files found in: {.path {p_cubes}}")
}

cli_alert_info("Found {length(cube_files)} cube file{?s}")

# Define column sets
cols_core <- c(
  "grid", "basisofrecord", "source_file",
  "eeacellcode", "yearmonth",
  "specieskey", "species",
  "family", "familykey",
  "order", "orderkey",
  "class", "classkey",
  "occurrences",
  "familycount", "genuscount", "ordercount", "classcount"
)

# ===========================================================================
# PHASE 1: CORE SUMMARIES
# ===========================================================================

if (MAKE_CORE_SUMMARIES) {
  cli_h2("Phase 1: Core Summaries (Cell, Time)")
  
  cell_aggs <- list()
  time_aggs <- list()
  key_stats <- list()
  
  cli_progress_bar("Processing cube files", total = length(cube_files), clear = FALSE)
  
  for (i in seq_along(cube_files)) {
    f <- cube_files[i]
    cli_progress_update()
    
    dt <- read_fst_cols(f, cols_core)
    
    if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
    if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
    
    have_cell <- "eeacellcode" %in% names(dt)
    have_time <- "yearmonth" %in% names(dt)
    have_spkey <- "specieskey" %in% names(dt)
    
    # File-level statistics
    key_stats[[length(key_stats) + 1]] <- data.table(
      file = basename(f),
      grid = if (!all(is.na(dt$grid))) unique(dt$grid)[1] else NA_character_,
      basisOfRecord = if (!all(is.na(dt$basisofrecord))) unique(dt$basisofrecord)[1] else NA_character_,
      rows = nrow(dt),
      total_occurrences = safe_sum(dt$occurrences),
      n_cells = if (have_cell) uniqueN(dt$eeacellcode) else NA_integer_,
      n_months = if (have_time) uniqueN(dt$yearmonth) else NA_integer_,
      n_specieskeys = if (have_spkey) uniqueN(dt$specieskey) else NA_integer_
    )
    
    # Cell summary: cell × basis
    if (have_cell) {
      cell_dt <- dt[, .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_
      ), by = .(grid, basisofrecord, eeacellcode)]
      
      cell_all <- dt[, .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_
      ), by = .(grid, eeacellcode)]
      cell_all[, basisofrecord := "all"]
      
      cell_dt <- rbindlist(list(cell_dt, cell_all), use.names = TRUE, fill = TRUE)
      cell_aggs[[length(cell_aggs) + 1]] <- cell_dt
    }
    
    # Time summary: yearmonth × basis
    if (have_time) {
      time_dt <- dt[, .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_cells = if (have_cell) uniqueN(eeacellcode) else NA_integer_
      ), by = .(grid, basisofrecord, yearmonth)]
      
      time_all <- dt[, .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_cells = if (have_cell) uniqueN(eeacellcode) else NA_integer_
      ), by = .(grid, yearmonth)]
      time_all[, basisofrecord := "all"]
      
      time_dt <- rbindlist(list(time_dt, time_all), use.names = TRUE, fill = TRUE)
      time_aggs[[length(time_aggs) + 1]] <- time_dt
    }
    
    rm(dt)
    invisible(gc())
  }
  
  cli_progress_done()
  
  # Combine and write core summaries
  cli_alert_info("Combining core summaries...")
  
  key_df <- rbindlist(key_stats, use.names = TRUE, fill = TRUE)
  grid_levels <- unique(key_df$grid) |> na.omit()
  cli_alert_info("Detected grid levels: {paste(grid_levels, collapse = ', ')}")
  
  # Cell summary
  if (length(cell_aggs) > 0) {
    cell_all <- rbindlist(cell_aggs, use.names = TRUE, fill = TRUE)
    cell_all <- cell_all[, .(
      occurrences = safe_sum(occurrences),
      n_species = safe_max(n_species)
    ), by = .(grid, basisofrecord, eeacellcode)]
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(cell_all, g, p_derived, glue("cell_summary_{grid_suffix}.csv"))
    }
    rm(cell_all)
  }
  
  # Time summary
  if (length(time_aggs) > 0) {
    time_all <- rbindlist(time_aggs, use.names = TRUE, fill = TRUE)
    time_all <- time_all[, .(
      occurrences = safe_sum(occurrences),
      n_species = safe_max(n_species),
      n_cells = safe_max(n_cells)
    ), by = .(grid, basisofrecord, yearmonth)]
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(time_all, g, p_derived, glue("time_summary_{grid_suffix}.csv"))
    }
    rm(time_all)
  }
  
  # Write cube key summary
  fwrite(key_df, here(p_derived, "cube_key_summary.csv"))
  cli_alert_success("cube_key_summary.csv: {nrow(key_df)} rows")
  
  rm(cell_aggs, time_aggs)
  invisible(gc())
}

# ===========================================================================
# PHASE 2: CELL × TIME SUMMARY
# ===========================================================================

if (MAKE_CELL_TIME) {
  cli_h2("Phase 2: Cell × Time Summary")
  
  cell_time_aggs <- list()
  
  cli_progress_bar("Processing cube files", total = length(cube_files), clear = FALSE)
  
  for (f in cube_files) {
    cli_progress_update()
    
    dt <- read_fst_cols(f, c("grid", "basisofrecord", "eeacellcode", "yearmonth", 
                              "specieskey", "occurrences"))
    
    if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
    if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
    
    if (!all(c("eeacellcode", "yearmonth") %in% names(dt))) {
      rm(dt)
      next
    }
    
    have_spkey <- "specieskey" %in% names(dt)
    
    ct_dt <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_
    ), by = .(grid, basisofrecord, eeacellcode, yearmonth)]
    
    ct_all <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_
    ), by = .(grid, eeacellcode, yearmonth)]
    ct_all[, basisofrecord := "all"]
    
    ct_dt <- rbindlist(list(ct_dt, ct_all), use.names = TRUE, fill = TRUE)
    cell_time_aggs[[length(cell_time_aggs) + 1]] <- ct_dt
    
    rm(dt, ct_dt, ct_all)
    invisible(gc())
  }
  
  cli_progress_done()
  
  if (length(cell_time_aggs) > 0) {
    cli_alert_info("Combining cell × time summaries...")
    
    cell_time_all <- rbindlist(cell_time_aggs, use.names = TRUE, fill = TRUE)
    cell_time_all <- cell_time_all[, .(
      occurrences = safe_sum(occurrences),
      n_species = safe_max(n_species)
    ), by = .(grid, basisofrecord, eeacellcode, yearmonth)]
    
    grid_levels <- unique(cell_time_all$grid) |> na.omit()
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(cell_time_all, g, p_derived, glue("cell_time_summary_{grid_suffix}.csv"))
    }
    
    rm(cell_time_all)
  }
  
  rm(cell_time_aggs)
  invisible(gc())
}

# ===========================================================================
# PHASE 3: ORDER-LEVEL SUMMARIES
# ===========================================================================

if (MAKE_ORDER_SUMMARIES) {
  cli_h2("Phase 3: Order-Level Summaries")
  
  order_cell_aggs <- list()
  order_time_aggs <- list()
  family_time_aggs <- list()
  
  cli_progress_bar("Processing cube files", total = length(cube_files), clear = FALSE)
  
  for (f in cube_files) {
    cli_progress_update()
    
    dt <- read_fst_cols(f, c("grid", "basisofrecord", "eeacellcode", "yearmonth",
                              "order", "orderkey", "family", "familykey",
                              "specieskey", "occurrences"))
    
    if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
    if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
    
    have_cell <- "eeacellcode" %in% names(dt)
    have_time <- "yearmonth" %in% names(dt)
    have_order <- "order" %in% names(dt)
    have_family <- "family" %in% names(dt)
    have_spkey <- "specieskey" %in% names(dt)
    
    # Order × Cell summary
    if (have_order && have_cell) {
      oc_dt <- dt[!is.na(order), .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_families = if (have_family) uniqueN(family) else NA_integer_
      ), by = .(grid, basisofrecord, order, eeacellcode)]
      
      oc_all <- dt[!is.na(order), .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_families = if (have_family) uniqueN(family) else NA_integer_
      ), by = .(grid, order, eeacellcode)]
      oc_all[, basisofrecord := "all"]
      
      oc_dt <- rbindlist(list(oc_dt, oc_all), use.names = TRUE, fill = TRUE)
      order_cell_aggs[[length(order_cell_aggs) + 1]] <- oc_dt
    }
    
    # Order × Time summary
    if (have_order && have_time) {
      ot_dt <- dt[!is.na(order), .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_families = if (have_family) uniqueN(family) else NA_integer_,
        n_cells = if (have_cell) uniqueN(eeacellcode) else NA_integer_
      ), by = .(grid, basisofrecord, order, yearmonth)]
      
      ot_all <- dt[!is.na(order), .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_families = if (have_family) uniqueN(family) else NA_integer_,
        n_cells = if (have_cell) uniqueN(eeacellcode) else NA_integer_
      ), by = .(grid, order, yearmonth)]
      ot_all[, basisofrecord := "all"]
      
      ot_dt <- rbindlist(list(ot_dt, ot_all), use.names = TRUE, fill = TRUE)
      order_time_aggs[[length(order_time_aggs) + 1]] <- ot_dt
    }
    
    # Family × Time summary
    if (have_family && have_time) {
      ft_dt <- dt[!is.na(family), .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_cells = if (have_cell) uniqueN(eeacellcode) else NA_integer_
      ), by = .(grid, basisofrecord, order, family, yearmonth)]
      
      ft_all <- dt[!is.na(family), .(
        occurrences = safe_sum(occurrences),
        n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_,
        n_cells = if (have_cell) uniqueN(eeacellcode) else NA_integer_
      ), by = .(grid, order, family, yearmonth)]
      ft_all[, basisofrecord := "all"]
      
      ft_dt <- rbindlist(list(ft_dt, ft_all), use.names = TRUE, fill = TRUE)
      family_time_aggs[[length(family_time_aggs) + 1]] <- ft_dt
    }
    
    rm(dt)
    invisible(gc())
  }
  
  cli_progress_done()
  
  # Combine and write order-level summaries
  cli_alert_info("Combining order-level summaries...")
  
  if (length(order_cell_aggs) > 0) {
    order_cell_all <- rbindlist(order_cell_aggs, use.names = TRUE, fill = TRUE)
    order_cell_all <- order_cell_all[, .(
      occurrences = safe_sum(occurrences),
      n_species = safe_max(n_species),
      n_families = safe_max(n_families)
    ), by = .(grid, basisofrecord, order, eeacellcode)]
    
    grid_levels <- unique(order_cell_all$grid) |> na.omit()
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(order_cell_all, g, p_derived, glue("order_cell_summary_{grid_suffix}.csv"))
    }
    rm(order_cell_all)
  }
  
  if (length(order_time_aggs) > 0) {
    order_time_all <- rbindlist(order_time_aggs, use.names = TRUE, fill = TRUE)
    order_time_all <- order_time_all[, .(
      occurrences = safe_sum(occurrences),
      n_species = safe_max(n_species),
      n_families = safe_max(n_families),
      n_cells = safe_max(n_cells)
    ), by = .(grid, basisofrecord, order, yearmonth)]
    
    grid_levels <- unique(order_time_all$grid) |> na.omit()
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(order_time_all, g, p_derived, glue("order_time_summary_{grid_suffix}.csv"))
    }
    rm(order_time_all)
  }
  
  if (length(family_time_aggs) > 0) {
    family_time_all <- rbindlist(family_time_aggs, use.names = TRUE, fill = TRUE)
    family_time_all <- family_time_all[, .(
      occurrences = safe_sum(occurrences),
      n_species = safe_max(n_species),
      n_cells = safe_max(n_cells)
    ), by = .(grid, basisofrecord, order, family, yearmonth)]
    
    grid_levels <- unique(family_time_all$grid) |> na.omit()
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(family_time_all, g, p_derived, glue("family_time_summary_{grid_suffix}.csv"))
    }
    rm(family_time_all)
  }
  
  rm(order_cell_aggs, order_time_aggs, family_time_aggs)
  invisible(gc())
}

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 06a)")

core_files <- list.files(p_derived, pattern = "^(cell|time)_summary.*\\.csv$")
cell_time_files <- list.files(p_derived, pattern = "^cell_time_summary.*\\.csv$")
order_files <- list.files(p_derived, pattern = "^(order|family).*summary.*\\.csv$")

summary_info <- tibble::tribble(
  ~Category, ~Count, ~Location,
  "Core summaries (cell, time)", length(core_files), "derived/",
  "Cell × time summaries", length(cell_time_files), "derived/",
  "Order/family summaries", length(order_files), "derived/"
)

print(summary_info)

cli_alert_success("Core summaries complete!")
cli_alert_info("Output location: {.path {p_derived}}")
cli_alert_info("Next step: Run scripts/06b_make_species_summaries.R")
