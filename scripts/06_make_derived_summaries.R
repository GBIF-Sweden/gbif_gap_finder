# scripts/06_make_derived_summaries.R
# ==============================================================================
# Create Analysis-Ready Derived Summaries with Bias Correction
# ==============================================================================
# This script creates a comprehensive set of summary tables for gap analysis:
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
# SPECIES-LEVEL SUMMARIES (split by order for manageable file sizes):
#   - species_summary_{Order}: species totals per order
#   - species_cell_{Order}: species × cell per order
#   - species_time_{Order}: species × yearmonth per order
#   - species_cell_time_{Order}: full detail per order
#
# All species-level tables include bias correction columns:
#   - familycount, ordercount, classcount
#   - relative_family, relative_order, relative_class
#
# Memory strategy: Reads only required columns, aggregates per-file,
# then combines aggregates (not raw data). Species tables split by order.

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
MAKE_SPECIES_BY_ORDER <- TRUE

# Paths -------------------------------------------------------------------
p_cubes <- here(p_data_proc, "cubes")
p_derived <- here(p_data_proc, "derived")

# Create output directories
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_order", "species_summary"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_order", "species_cell"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_order", "species_time"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_order", "species_cell_time"), showWarnings = FALSE, recursive = TRUE)

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

# Helper functions --------------------------------------------------------

#' Safely sum numeric values
#' @param x Numeric vector
#' @return Sum with NA handling
safe_sum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

#' Safely take max of numeric values
#' @param x Numeric vector
#' @return Max with NA handling, returns NA if all NA
safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(as.numeric(x), na.rm = TRUE)
}

#' Read selected columns from fst file
#' @param path Path to .fst file
#' @param cols Character vector of columns to read
#' @return data.table with selected columns
read_fst_cols <- function(path, cols) {
  meta <- fst::metadata_fst(path)
  available_cols <- meta$columnNames
  
  # Only read columns that exist
  cols_to_read <- intersect(cols, available_cols)
  
  # Occurrences column is required
  if (!("occurrences" %in% available_cols)) {
    cli_abort("Column 'occurrences' missing in: {.path {basename(path)}}")
  }
  
  df <- fst::read_fst(path, columns = cols_to_read)
  setDT(df)
  df
}

#' Add "all" basis of record rollup
#' @param dt data.table with basisofrecord column
#' @param group_cols Character vector of grouping columns (excluding grid and basisofrecord)
#' @param value_cols Character vector of value columns to aggregate
#' @param agg_funcs Named list of aggregation functions per column
#' @return data.table with added "all" rows
add_overall_basis <- function(dt, group_cols, value_cols = "occurrences", 
                               agg_funcs = list(occurrences = safe_sum)) {
  # Build aggregation expression
  agg_expr <- lapply(names(agg_funcs), function(col) {
    call(as.character(agg_funcs[[col]]), as.name(col))
  })
  names(agg_expr) <- names(agg_funcs)
  
  overall <- dt[, lapply(.SD, safe_sum), 
                by = c("grid", group_cols),
                .SDcols = value_cols]
  overall[, basisofrecord := "all"]
  
  rbindlist(list(dt, overall), use.names = TRUE, fill = TRUE)
}

#' Write grid-specific subset to CSV
#' @param dt data.table with grid column
#' @param grid_value Grid identifier to filter
#' @param out_dir Output directory
#' @param filename Output filename
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

#' Clean string for use in filename
#' @param x Character string
#' @return Cleaned string safe for filenames
clean_for_filename <- function(x) {
  x <- str_replace_all(x, "[^A-Za-z0-9]", "_")
  x <- str_replace_all(x, "_+", "_")
  str_remove(x, "^_|_$")
}

#' Calculate relative occurrence columns
#' @param dt data.table with occurrences and count columns
#' @return data.table with added relative columns
add_relative_occurrences <- function(dt) {
  if ("familycount" %in% names(dt)) {
    dt[, relative_family := fifelse(
      !is.na(familycount) & familycount > 0, 
      occurrences / familycount, 
      NA_real_
    )]
  }
  if ("ordercount" %in% names(dt)) {
    dt[, relative_order := fifelse(
      !is.na(ordercount) & ordercount > 0, 
      occurrences / ordercount, 
      NA_real_
    )]
  }
  if ("classcount" %in% names(dt)) {
    dt[, relative_class := fifelse(
      !is.na(classcount) & classcount > 0, 
      occurrences / classcount, 
      NA_real_
    )]
  }
  dt
}

# Locate cube files -------------------------------------------------------
cli_h1("Derived Summaries with Bias Correction")
cli_h2("Locating Cube Files")

cube_files <- list.files(p_cubes, pattern = "\\.fst$", full.names = TRUE)

if (length(cube_files) == 0) {
  cli_abort("No .fst cube files found in: {.path {p_cubes}}")
}

cli_alert_info("Found {length(cube_files)} cube file{?s}")

# Define column sets ------------------------------------------------------
cols_core <- c(
  # Provenance
  "grid", "basisofrecord", "source_file",
  # Spatial
  "eeacellcode",
  # Temporal  
  "yearmonth",
  # Taxonomic identifiers
  "specieskey", "species",
  "family", "familykey",
  "order", "orderkey",
  "class", "classkey",
  # Metrics
  "occurrences",
  # Higher taxon counts for bias correction
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
    
    # Ensure provenance columns exist
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
      
      # Add "all" basis rollup
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
      
      # Add "all" basis rollup
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
  
  # Detect grid levels
  key_df <- rbindlist(key_stats, use.names = TRUE, fill = TRUE)
  grid_levels <- unique(key_df$grid) |> na.omit()
  cli_alert_info("Detected grid levels: {paste(grid_levels, collapse = ', ')}")
  
  # Cell summary
  if (length(cell_aggs) > 0) {
    cell_all <- rbindlist(cell_aggs, use.names = TRUE, fill = TRUE)
    cell_all <- cell_all[, .(
      occurrences = safe_sum(occurrences),
      n_species = safe_max(n_species)  # Take max since we can't re-count unique
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
    
    # Aggregate: cell × yearmonth × basis
    ct_dt <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = if (have_spkey) uniqueN(specieskey) else NA_integer_
    ), by = .(grid, basisofrecord, eeacellcode, yearmonth)]
    
    # Add "all" basis
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
    
    # Get grid levels
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
  
  # Get grid levels
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
# PHASE 4: SPECIES-LEVEL SUMMARIES BY ORDER (with smart family splitting)
# ===========================================================================

if (MAKE_SPECIES_BY_ORDER) {
  cli_h2("Phase 4: Species-Level Summaries by Order/Family")
  
  # ---------------------------------------------------------------------------
  # CONFIGURATION: Thresholds for splitting large orders into families
  # ---------------------------------------------------------------------------
  
  # If an order has more than this many rows across all cubes, split by family
  LARGE_ORDER_THRESHOLD <- 500000  # 500K rows
  
  # Columns for species-level analysis (including bias correction counts)
  cols_species <- c(
    "grid", "basisofrecord",
    "eeacellcode", "yearmonth",
    "specieskey", "species",
    "family", "familykey",
    "order", "orderkey",
    "class", "classkey",
    "occurrences",
    "familycount", "ordercount", "classcount"
  )
  
  # ---------------------------------------------------------------------------
  # STEP 1: Identify all orders and estimate their sizes
  # ---------------------------------------------------------------------------
  cli_alert_info("Scanning orders and estimating sizes...")
  
  order_sizes <- list()
  
  for (f in cube_files) {
    dt <- read_fst_cols(f, c("order", "family", "grid"))
    
    if (all(c("order", "grid") %in% names(dt))) {
      # Count rows per order per grid
      counts <- dt[!is.na(order) & order != "", .N, by = .(grid, order)]
      order_sizes[[length(order_sizes) + 1]] <- counts
      
      # Also get family info for large orders
      if ("family" %in% names(dt)) {
        family_counts <- dt[!is.na(order) & order != "" & !is.na(family), 
                            .N, by = .(grid, order, family)]
        # Store for later use
        if (!exists("family_size_list")) family_size_list <- list()
        family_size_list[[length(family_size_list) + 1]] <- family_counts
      }
    }
    rm(dt)
    invisible(gc())
  }
  
  # Combine order sizes
  order_totals <- rbindlist(order_sizes, use.names = TRUE, fill = TRUE)
  order_totals <- order_totals[, .(total_rows = sum(N)), by = .(grid, order)]
  
  # Combine family sizes
  if (exists("family_size_list") && length(family_size_list) > 0) {
    family_totals <- rbindlist(family_size_list, use.names = TRUE, fill = TRUE)
    family_totals <- family_totals[, .(total_rows = sum(N)), by = .(grid, order, family)]
  } else {
    family_totals <- data.table(grid = character(), order = character(), 
                                family = character(), total_rows = numeric())
  }
  
  # Identify large orders that need family splitting
  large_orders <- order_totals[total_rows > LARGE_ORDER_THRESHOLD]
  small_orders <- order_totals[total_rows <= LARGE_ORDER_THRESHOLD]
  
  cli_alert_info("Found {nrow(order_totals)} order × grid combinations")
  cli_alert_info("Large orders (>{scales::comma(LARGE_ORDER_THRESHOLD)} rows): {nrow(large_orders)}")
  cli_alert_info("Small orders: {nrow(small_orders)}")
  
  if (nrow(large_orders) > 0) {
    cli_alert_info("Large orders will be split by family:")
    print(large_orders[order(-total_rows)][1:min(10, nrow(large_orders))])
  }
  
  grid_levels <- unique(order_totals$grid)
  
  # ---------------------------------------------------------------------------
  # STEP 2: Process SMALL orders (single file per order)
  # ---------------------------------------------------------------------------
  if (nrow(small_orders) > 0) {
    cli_h3("Processing Small Orders (single file each)")
    
    orders_to_process <- unique(small_orders$order)
    
    cli_progress_bar("Small orders", total = length(orders_to_process), clear = FALSE)
    
    for (current_order in orders_to_process) {
      cli_progress_update()
      
      if (is.na(current_order) || current_order == "") next
      
      order_clean <- clean_for_filename(current_order)
      
      # Collect data for this order from all cube files
      species_summary_aggs <- list()
      species_cell_aggs <- list()
      species_time_aggs <- list()
      species_cell_time_aggs <- list()
      
      for (f in cube_files) {
        dt <- read_fst_cols(f, cols_species)
        
        if (!all(c("order", "specieskey") %in% names(dt))) {
          rm(dt)
          next
        }
        
        dt <- dt[order == current_order]
        
        if (nrow(dt) == 0) {
          rm(dt)
          next
        }
        
        if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
        if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
        
        have_cell <- "eeacellcode" %in% names(dt)
        have_time <- "yearmonth" %in% names(dt)
        have_species <- "species" %in% names(dt)
        have_family <- "family" %in% names(dt)
        have_class <- "class" %in% names(dt)
        have_familycount <- "familycount" %in% names(dt)
        have_ordercount <- "ordercount" %in% names(dt)
        have_classcount <- "classcount" %in% names(dt)
        
        species_cols <- c("specieskey")
        if (have_species) species_cols <- c(species_cols, "species")
        if (have_family) species_cols <- c(species_cols, "family")
        if (have_class) species_cols <- c(species_cols, "class")
        
        # --- Species Summary ---
        ss_by <- c("grid", "basisofrecord", species_cols)
        ss_dt <- dt[, .(
          occurrences = safe_sum(occurrences),
          familycount = if (have_familycount) safe_max(familycount) else NA_real_,
          ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
          classcount = if (have_classcount) safe_max(classcount) else NA_real_
        ), by = ss_by]
        
        ss_all <- dt[, .(
          occurrences = safe_sum(occurrences),
          familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
          ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
          classcount = if (have_classcount) safe_sum(classcount) else NA_real_
        ), by = c("grid", species_cols)]
        ss_all[, basisofrecord := "all"]
        
        ss_dt <- rbindlist(list(ss_dt, ss_all), use.names = TRUE, fill = TRUE)
        species_summary_aggs[[length(species_summary_aggs) + 1]] <- ss_dt
        
        # --- Species × Cell ---
        if (have_cell) {
          sc_by <- c("grid", "basisofrecord", species_cols, "eeacellcode")
          sc_dt <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_max(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_max(classcount) else NA_real_
          ), by = sc_by]
          
          sc_all <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_sum(classcount) else NA_real_
          ), by = c("grid", species_cols, "eeacellcode")]
          sc_all[, basisofrecord := "all"]
          
          sc_dt <- rbindlist(list(sc_dt, sc_all), use.names = TRUE, fill = TRUE)
          species_cell_aggs[[length(species_cell_aggs) + 1]] <- sc_dt
        }
        
        # --- Species × Time ---
        if (have_time) {
          st_by <- c("grid", "basisofrecord", species_cols, "yearmonth")
          st_dt <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_max(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_max(classcount) else NA_real_
          ), by = st_by]
          
          st_all <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_sum(classcount) else NA_real_
          ), by = c("grid", species_cols, "yearmonth")]
          st_all[, basisofrecord := "all"]
          
          st_dt <- rbindlist(list(st_dt, st_all), use.names = TRUE, fill = TRUE)
          species_time_aggs[[length(species_time_aggs) + 1]] <- st_dt
        }
        
        # --- Species × Cell × Time ---
        if (have_cell && have_time) {
          sct_by <- c("grid", "basisofrecord", species_cols, "eeacellcode", "yearmonth")
          sct_dt <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_max(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_max(classcount) else NA_real_
          ), by = sct_by]
          
          sct_all <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_sum(classcount) else NA_real_
          ), by = c("grid", species_cols, "eeacellcode", "yearmonth")]
          sct_all[, basisofrecord := "all"]
          
          sct_dt <- rbindlist(list(sct_dt, sct_all), use.names = TRUE, fill = TRUE)
          species_cell_time_aggs[[length(species_cell_time_aggs) + 1]] <- sct_dt
        }
        
        rm(dt)
        invisible(gc())
      }
      
      # Combine and write for this order
      write_order_summaries(
        order_clean, grid_levels,
        species_summary_aggs, species_cell_aggs, 
        species_time_aggs, species_cell_time_aggs,
        p_derived
      )
      
      rm(species_summary_aggs, species_cell_aggs, species_time_aggs, species_cell_time_aggs)
      invisible(gc())
    }
    
    cli_progress_done()
  }
  
  # ---------------------------------------------------------------------------
  # STEP 3: Process LARGE orders (split by family)
  # ---------------------------------------------------------------------------
  if (nrow(large_orders) > 0) {
    cli_h3("Processing Large Orders (split by family)")
    
    # Create family subdirectory structure
    dir.create(here(p_derived, "by_family", "species_summary"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here(p_derived, "by_family", "species_cell"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here(p_derived, "by_family", "species_time"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here(p_derived, "by_family", "species_cell_time"), showWarnings = FALSE, recursive = TRUE)
    
    large_order_names <- unique(large_orders$order)
    
    for (current_order in large_order_names) {
      cli_alert_info("Processing large order: {current_order}")
      
      # Get families in this order
      families_in_order <- family_totals[order == current_order, unique(family)]
      families_in_order <- families_in_order[!is.na(families_in_order) & families_in_order != ""]
      
      cli_alert_info("  → {length(families_in_order)} families to process")
      
      cli_progress_bar(
        glue("Families in {current_order}"), 
        total = length(families_in_order), 
        clear = FALSE
      )
      
      for (current_family in families_in_order) {
        cli_progress_update()
        
        family_clean <- clean_for_filename(current_family)
        order_clean <- clean_for_filename(current_order)
        
        # Collect data for this family
        species_summary_aggs <- list()
        species_cell_aggs <- list()
        species_time_aggs <- list()
        species_cell_time_aggs <- list()
        
        for (f in cube_files) {
          dt <- read_fst_cols(f, cols_species)
          
          if (!all(c("order", "family", "specieskey") %in% names(dt))) {
            rm(dt)
            next
          }
          
          # Filter to this family (within the order)
          dt <- dt[order == current_order & family == current_family]
          
          if (nrow(dt) == 0) {
            rm(dt)
            next
          }
          
          if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
          if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
          
          have_cell <- "eeacellcode" %in% names(dt)
          have_time <- "yearmonth" %in% names(dt)
          have_species <- "species" %in% names(dt)
          have_class <- "class" %in% names(dt)
          have_familycount <- "familycount" %in% names(dt)
          have_ordercount <- "ordercount" %in% names(dt)
          have_classcount <- "classcount" %in% names(dt)
          
          species_cols <- c("specieskey")
          if (have_species) species_cols <- c(species_cols, "species")
          # Include order and family for context
          species_cols <- c(species_cols, "order", "family")
          if (have_class) species_cols <- c(species_cols, "class")
          
          # --- Species Summary ---
          ss_by <- c("grid", "basisofrecord", species_cols)
          ss_dt <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_max(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_max(classcount) else NA_real_
          ), by = ss_by]
          
          ss_all <- dt[, .(
            occurrences = safe_sum(occurrences),
            familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
            ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
            classcount = if (have_classcount) safe_sum(classcount) else NA_real_
          ), by = c("grid", species_cols)]
          ss_all[, basisofrecord := "all"]
          
          ss_dt <- rbindlist(list(ss_dt, ss_all), use.names = TRUE, fill = TRUE)
          species_summary_aggs[[length(species_summary_aggs) + 1]] <- ss_dt
          
          # --- Species × Cell ---
          if (have_cell) {
            sc_by <- c("grid", "basisofrecord", species_cols, "eeacellcode")
            sc_dt <- dt[, .(
              occurrences = safe_sum(occurrences),
              familycount = if (have_familycount) safe_max(familycount) else NA_real_,
              ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
              classcount = if (have_classcount) safe_max(classcount) else NA_real_
            ), by = sc_by]
            
            sc_all <- dt[, .(
              occurrences = safe_sum(occurrences),
              familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
              ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
              classcount = if (have_classcount) safe_sum(classcount) else NA_real_
            ), by = c("grid", species_cols, "eeacellcode")]
            sc_all[, basisofrecord := "all"]
            
            sc_dt <- rbindlist(list(sc_dt, sc_all), use.names = TRUE, fill = TRUE)
            species_cell_aggs[[length(species_cell_aggs) + 1]] <- sc_dt
          }
          
          # --- Species × Time ---
          if (have_time) {
            st_by <- c("grid", "basisofrecord", species_cols, "yearmonth")
            st_dt <- dt[, .(
              occurrences = safe_sum(occurrences),
              familycount = if (have_familycount) safe_max(familycount) else NA_real_,
              ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
              classcount = if (have_classcount) safe_max(classcount) else NA_real_
            ), by = st_by]
            
            st_all <- dt[, .(
              occurrences = safe_sum(occurrences),
              familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
              ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
              classcount = if (have_classcount) safe_sum(classcount) else NA_real_
            ), by = c("grid", species_cols, "yearmonth")]
            st_all[, basisofrecord := "all"]
            
            st_dt <- rbindlist(list(st_dt, st_all), use.names = TRUE, fill = TRUE)
            species_time_aggs[[length(species_time_aggs) + 1]] <- st_dt
          }
          
          # --- Species × Cell × Time ---
          if (have_cell && have_time) {
            sct_by <- c("grid", "basisofrecord", species_cols, "eeacellcode", "yearmonth")
            sct_dt <- dt[, .(
              occurrences = safe_sum(occurrences),
              familycount = if (have_familycount) safe_max(familycount) else NA_real_,
              ordercount = if (have_ordercount) safe_max(ordercount) else NA_real_,
              classcount = if (have_classcount) safe_max(classcount) else NA_real_
            ), by = sct_by]
            
            sct_all <- dt[, .(
              occurrences = safe_sum(occurrences),
              familycount = if (have_familycount) safe_sum(familycount) else NA_real_,
              ordercount = if (have_ordercount) safe_sum(ordercount) else NA_real_,
              classcount = if (have_classcount) safe_sum(classcount) else NA_real_
            ), by = c("grid", species_cols, "eeacellcode", "yearmonth")]
            sct_all[, basisofrecord := "all"]
            
            sct_dt <- rbindlist(list(sct_dt, sct_all), use.names = TRUE, fill = TRUE)
            species_cell_time_aggs[[length(species_cell_time_aggs) + 1]] <- sct_dt
          }
          
          rm(dt)
          invisible(gc())
        }
        
        # Combine and write for this family
        # Use naming convention: Order_Family
        combined_name <- paste0(order_clean, "_", family_clean)
        
        write_family_summaries(
          combined_name, grid_levels,
          species_summary_aggs, species_cell_aggs,
          species_time_aggs, species_cell_time_aggs,
          p_derived
        )
        
        rm(species_summary_aggs, species_cell_aggs, species_time_aggs, species_cell_time_aggs)
        invisible(gc())
      }
      
      cli_progress_done()
    }
  }
}

# ---------------------------------------------------------------------------
# Helper function: Write order-level summaries
# ---------------------------------------------------------------------------
write_order_summaries <- function(order_clean, grid_levels,
                                  species_summary_aggs, species_cell_aggs,
                                  species_time_aggs, species_cell_time_aggs,
                                  p_derived) {
  
  # Species Summary
  if (length(species_summary_aggs) > 0) {
    combined <- rbindlist(species_summary_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "family", "class"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_order", "species_summary"),
                        glue("species_summary_{order_clean}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
  
  # Species × Cell
  if (length(species_cell_aggs) > 0) {
    combined <- rbindlist(species_cell_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "family", "class", "eeacellcode"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_order", "species_cell"),
                        glue("species_cell_{order_clean}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
  
  # Species × Time
  if (length(species_time_aggs) > 0) {
    combined <- rbindlist(species_time_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "family", "class", "yearmonth"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_order", "species_time"),
                        glue("species_time_{order_clean}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
  
  # Species × Cell × Time
  if (length(species_cell_time_aggs) > 0) {
    combined <- rbindlist(species_cell_time_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "family", "class", "eeacellcode", "yearmonth"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_order", "species_cell_time"),
                        glue("species_cell_time_{order_clean}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
}

# ---------------------------------------------------------------------------
# Helper function: Write family-level summaries (for large orders)
# ---------------------------------------------------------------------------
write_family_summaries <- function(combined_name, grid_levels,
                                   species_summary_aggs, species_cell_aggs,
                                   species_time_aggs, species_cell_time_aggs,
                                   p_derived) {
  
  # Species Summary
  if (length(species_summary_aggs) > 0) {
    combined <- rbindlist(species_summary_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_family", "species_summary"),
                        glue("species_summary_{combined_name}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
  
  # Species × Cell
  if (length(species_cell_aggs) > 0) {
    combined <- rbindlist(species_cell_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class", "eeacellcode"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_family", "species_cell"),
                        glue("species_cell_{combined_name}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
  
  # Species × Time
  if (length(species_time_aggs) > 0) {
    combined <- rbindlist(species_time_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class", "yearmonth"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_family", "species_time"),
                        glue("species_time_{combined_name}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
  
  # Species × Cell × Time
  if (length(species_cell_time_aggs) > 0) {
    combined <- rbindlist(species_cell_time_aggs, use.names = TRUE, fill = TRUE)
    group_cols <- intersect(c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class", "eeacellcode", "yearmonth"), names(combined))
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    combined <- add_relative_occurrences(combined)
    
    for (g in grid_levels) {
      grid_suffix <- str_extract(g, "\\d+km")
      write_grid_subset(combined, g,
                        here(p_derived, "by_family", "species_cell_time"),
                        glue("species_cell_time_{combined_name}_{grid_suffix}.csv"))
    }
    rm(combined)
  }
}
# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary")

# Count output files
core_files <- list.files(p_derived, pattern = "^(cell|time)_summary.*\\.csv$")
cell_time_files <- list.files(p_derived, pattern = "^cell_time_summary.*\\.csv$")
order_files <- list.files(p_derived, pattern = "^(order|family).*summary.*\\.csv$")
species_files <- list.files(here(p_derived, "by_order"), pattern = "\\.csv$", recursive = TRUE)

summary_info <- tibble::tribble(
  ~Category, ~Count, ~Location,
  "Core summaries (cell, time)", length(core_files), "derived/",
  "Cell × time summaries", length(cell_time_files), "derived/",
  "Order/family summaries", length(order_files), "derived/",
  "Species by order", length(species_files), "derived/by_order/"
)

print(summary_info)

cli_alert_success("Derived summaries complete!")
cli_alert_info("Output location: {.path {p_derived}}")
cli_alert_info("Species tables include bias correction columns:")
cli_alert_info("  - familycount, ordercount, classcount")
cli_alert_info("  - relative_family, relative_order, relative_class")
