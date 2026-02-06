# scripts/06b_make_species_summaries.R
# ==============================================================================
# Create Species-Level Derived Summaries with Bias Correction (OPTIMIZED)
# ==============================================================================
# 
# OPTIMIZATION: Read each cube file ONCE, process all orders/families in memory.
# Previous version read all cube files for EACH order - O(files × orders)
# This version reads each file once - O(files)
#
# Output structure:
#   - by_order/: Small orders (< threshold rows)
#   - by_family/: Large orders split by family
#
# All tables include bias correction columns:
#   - familycount, ordercount, classcount
#   - relative_family, relative_order, relative_class

library(here)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------

# Orders with more rows than this will be split by family
LARGE_ORDER_THRESHOLD <- 500000

# Process these summary types (set FALSE to skip for faster runs)
MAKE_SPECIES_SUMMARY <- TRUE
MAKE_SPECIES_CELL <- TRUE
MAKE_SPECIES_TIME <- TRUE
MAKE_SPECIES_CELL_TIME <- TRUE  # This is the slowest - set FALSE for quick runs

# Paths -------------------------------------------------------------------
p_cubes <- here(p_data_proc, "cubes")
p_derived <- here(p_data_proc, "derived")

# Create output directories
for (subdir in c("by_order", "by_family")) {
  for (type in c("species_summary", "species_cell", "species_time", "species_cell_time")) {
    dir.create(here(p_derived, subdir, type), showWarnings = FALSE, recursive = TRUE)
  }
}

# Validate inputs ---------------------------------------------------------
if (!dir.exists(p_cubes)) {
  cli_abort("Cubes directory not found: {.path {p_cubes}}")
}

if (!requireNamespace("fst", quietly = TRUE)) {
  cli_abort("Package {.pkg fst} is required")
}

# ===========================================================================
# HELPER FUNCTIONS
# ===========================================================================

safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(as.numeric(x), na.rm = TRUE)
}

read_fst_cols <- function(path, cols) {
  meta <- fst::metadata_fst(path)
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

add_relative_occurrences <- function(dt) {
  if ("familycount" %in% names(dt)) {
    dt[, relative_family := fifelse(familycount > 0, occurrences / familycount, NA_real_)]
  }
  if ("ordercount" %in% names(dt)) {
    dt[, relative_order := fifelse(ordercount > 0, occurrences / ordercount, NA_real_)]
  }
  if ("classcount" %in% names(dt)) {
    dt[, relative_class := fifelse(classcount > 0, occurrences / classcount, NA_real_)]
  }
  dt
}

# ===========================================================================
# STEP 1: SCAN FOR ORDER SIZES
# ===========================================================================

cli_h1("Species-Level Summaries (Optimized)")
cli_h2("Step 1: Scanning Order Sizes")

cube_files <- list.files(p_cubes, pattern = "\\.fst$", full.names = TRUE)
cli_alert_info("Found {length(cube_files)} cube files")

# Quick scan for order sizes
order_sizes <- list()
family_info <- list()

cli_progress_bar("Scanning", total = length(cube_files), clear = FALSE)
for (f in cube_files) {
  cli_progress_update()
  dt <- read_fst_cols(f, c("grid", "order", "family"))
  
  if ("order" %in% names(dt)) {
    order_sizes[[f]] <- dt[!is.na(order) & order != "", .N, by = .(grid, order)]
    if ("family" %in% names(dt)) {
      family_info[[f]] <- dt[!is.na(order) & !is.na(family) & order != "" & family != "", 
                              .(grid, order, family)] |> unique()
    }
  }
  rm(dt); gc()
}
cli_progress_done()

order_totals <- rbindlist(order_sizes)[, .(total_rows = sum(N)), by = .(grid, order)]
family_lookup <- rbindlist(family_info) |> unique()

# Classify orders
large_order_names <- order_totals[total_rows > LARGE_ORDER_THRESHOLD, unique(order)]
small_order_names <- order_totals[total_rows <= LARGE_ORDER_THRESHOLD, unique(order)]
grid_levels <- unique(order_totals$grid)

cli_alert_info("Small orders: {length(small_order_names)}")
cli_alert_info("Large orders (split by family): {length(large_order_names)}")

if (length(large_order_names) > 0) {
  cli_alert_info("Large orders: {paste(large_order_names[1:min(5, length(large_order_names))], collapse = ', ')}")
}

# ===========================================================================
# STEP 2: SINGLE-PASS AGGREGATION
# ===========================================================================

cli_h2("Step 2: Aggregating (single pass through files)")

# Initialize accumulators for each output type
# Using environment for fast reference semantics
accum <- new.env(hash = TRUE)

# Keys: "order|{order}" for small orders, "family|{order}|{family}" for large orders
init_accum <- function(key) {
  if (!exists(key, envir = accum)) {
    accum[[key]] <- list(
      summary = list(),
      cell = list(),
      time = list(),
      cell_time = list()
    )
  }
}

# Columns to read
cols_species <- c(
  "grid", "basisofrecord", "eeacellcode", "yearmonth",
  "specieskey", "species", "family", "order", "class",
  "occurrences", "familycount", "ordercount", "classcount"
)

cli_progress_bar("Processing cubes", total = length(cube_files), clear = FALSE)

for (f in cube_files) {
  cli_progress_update()
  
  dt <- read_fst_cols(f, cols_species)
  
  if (!("order" %in% names(dt)) || !("specieskey" %in% names(dt))) {
    rm(dt); next
  }
  
  # Ensure columns exist
  if (!("grid" %in% names(dt))) dt[, grid := "grid10km"]
  if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := "unknown"]
  if (!("family" %in% names(dt))) dt[, family := NA_character_]
  if (!("class" %in% names(dt))) dt[, class := NA_character_]
  if (!("species" %in% names(dt))) dt[, species := NA_character_]
  if (!("familycount" %in% names(dt))) dt[, familycount := NA_real_]
  if (!("ordercount" %in% names(dt))) dt[, ordercount := NA_real_]
  if (!("classcount" %in% names(dt))) dt[, classcount := NA_real_]
  
  have_cell <- "eeacellcode" %in% names(dt)
  have_time <- "yearmonth" %in% names(dt)
  
  # Filter to valid orders
  dt <- dt[!is.na(order) & order != ""]
  if (nrow(dt) == 0) { rm(dt); next }
  
  # Split into small orders and large orders
  dt[, is_large_order := order %in% large_order_names]
  
  # ----- PROCESS SMALL ORDERS (by order) -----
  dt_small <- dt[is_large_order == FALSE]
  
  if (nrow(dt_small) > 0) {
    orders_in_chunk <- unique(dt_small$order)
    
    for (ord in orders_in_chunk) {
      key <- paste0("order|", ord)
      init_accum(key)
      
      chunk <- dt_small[order == ord]
      
      # Species Summary
      if (MAKE_SPECIES_SUMMARY) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class)]
        accum[[key]]$summary[[length(accum[[key]]$summary) + 1]] <- agg
      }
      
      # Species × Cell
      if (MAKE_SPECIES_CELL && have_cell) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class, eeacellcode)]
        accum[[key]]$cell[[length(accum[[key]]$cell) + 1]] <- agg
      }
      
      # Species × Time
      if (MAKE_SPECIES_TIME && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class, yearmonth)]
        accum[[key]]$time[[length(accum[[key]]$time) + 1]] <- agg
      }
      
      # Species × Cell × Time
      if (MAKE_SPECIES_CELL_TIME && have_cell && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, family, class, eeacellcode, yearmonth)]
        accum[[key]]$cell_time[[length(accum[[key]]$cell_time) + 1]] <- agg
      }
    }
  }
  
  # ----- PROCESS LARGE ORDERS (by family) -----
  dt_large <- dt[is_large_order == TRUE]
  
  if (nrow(dt_large) > 0) {
    # Process by order+family combination
    combos <- unique(dt_large[!is.na(family) & family != "", .(order, family)])
    
    for (i in seq_len(nrow(combos))) {
      ord <- combos$order[i]
      fam <- combos$family[i]
      key <- paste0("family|", ord, "|", fam)
      init_accum(key)
      
      chunk <- dt_large[order == ord & family == fam]
      
      # Species Summary
      if (MAKE_SPECIES_SUMMARY) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class)]
        accum[[key]]$summary[[length(accum[[key]]$summary) + 1]] <- agg
      }
      
      # Species × Cell
      if (MAKE_SPECIES_CELL && have_cell) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class, eeacellcode)]
        accum[[key]]$cell[[length(accum[[key]]$cell) + 1]] <- agg
      }
      
      # Species × Time
      if (MAKE_SPECIES_TIME && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class, yearmonth)]
        accum[[key]]$time[[length(accum[[key]]$time) + 1]] <- agg
      }
      
      # Species × Cell × Time
      if (MAKE_SPECIES_CELL_TIME && have_cell && have_time) {
        agg <- chunk[, .(
          occurrences = safe_sum(occurrences),
          familycount = safe_max(familycount),
          ordercount = safe_max(ordercount),
          classcount = safe_max(classcount)
        ), by = .(grid, basisofrecord, specieskey, species, order, family, class, eeacellcode, yearmonth)]
        accum[[key]]$cell_time[[length(accum[[key]]$cell_time) + 1]] <- agg
      }
    }
  }
  
  rm(dt, dt_small, dt_large)
  gc()
}

cli_progress_done()

# ===========================================================================
# STEP 3: COMBINE AND WRITE OUTPUT FILES
# ===========================================================================

cli_h2("Step 3: Writing Output Files")

all_keys <- ls(accum)
cli_alert_info("Processing {length(all_keys)} taxon groups")

cli_progress_bar("Writing files", total = length(all_keys), clear = FALSE)

for (key in all_keys) {
  cli_progress_update()
  
  data <- accum[[key]]
  
  # Parse key
  parts <- str_split(key, "\\|")[[1]]
  key_type <- parts[1]
  
  if (key_type == "order") {
    order_name <- parts[2]
    name_clean <- clean_for_filename(order_name)
    out_subdir <- "by_order"
    group_cols_summary <- c("grid", "basisofrecord", "specieskey", "species", "family", "class")
    group_cols_cell <- c("grid", "basisofrecord", "specieskey", "species", "family", "class", "eeacellcode")
    group_cols_time <- c("grid", "basisofrecord", "specieskey", "species", "family", "class", "yearmonth")
    group_cols_cell_time <- c("grid", "basisofrecord", "specieskey", "species", "family", "class", "eeacellcode", "yearmonth")
  } else {
    order_name <- parts[2]
    family_name <- parts[3]
    name_clean <- paste0(clean_for_filename(order_name), "_", clean_for_filename(family_name))
    out_subdir <- "by_family"
    group_cols_summary <- c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class")
    group_cols_cell <- c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class", "eeacellcode")
    group_cols_time <- c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class", "yearmonth")
    group_cols_cell_time <- c("grid", "basisofrecord", "specieskey", "species", "order", "family", "class", "eeacellcode", "yearmonth")
  }
  
  # Helper to combine, add "all" basis, and write
  write_summary_type <- function(agg_list, group_cols, type_name) {
    if (length(agg_list) == 0) return()
    
    combined <- rbindlist(agg_list, use.names = TRUE, fill = TRUE)
    
    # Filter to existing columns
    group_cols <- intersect(group_cols, names(combined))
    
    # Re-aggregate
    combined <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_max(familycount),
      ordercount = safe_max(ordercount),
      classcount = safe_max(classcount)
    ), by = group_cols]
    
    # Add "all" basisofrecord
    group_cols_no_basis <- setdiff(group_cols, "basisofrecord")
    all_basis <- combined[, .(
      occurrences = safe_sum(occurrences),
      familycount = safe_sum(familycount),
      ordercount = safe_sum(ordercount),
      classcount = safe_sum(classcount),
      basisofrecord = "all"
    ), by = group_cols_no_basis]
    
    combined <- rbindlist(list(combined, all_basis), use.names = TRUE, fill = TRUE)
    
    # Add relative occurrences
    combined <- add_relative_occurrences(combined)
    
    # Write per grid
    for (g in unique(combined$grid)) {
      subset <- combined[grid == g]
      if (nrow(subset) == 0) next
      
      grid_suffix <- str_extract(g, "\\d+km")
      if (is.na(grid_suffix)) grid_suffix <- g
      
      out_path <- here(p_derived, out_subdir, type_name, 
                       glue("{type_name}_{name_clean}_{grid_suffix}.csv"))
      fwrite(subset, out_path)
    }
    
    rm(combined, all_basis)
  }
  
  # Write each summary type
  if (MAKE_SPECIES_SUMMARY) {
    write_summary_type(data$summary, group_cols_summary, "species_summary")
  }
  if (MAKE_SPECIES_CELL) {
    write_summary_type(data$cell, group_cols_cell, "species_cell")
  }
  if (MAKE_SPECIES_TIME) {
    write_summary_type(data$time, group_cols_time, "species_time")
  }
  if (MAKE_SPECIES_CELL_TIME) {
    write_summary_type(data$cell_time, group_cols_cell_time, "species_cell_time")
  }
  
  # Clear from accumulator to free memory
  rm(list = key, envir = accum)
  gc()
}

cli_progress_done()

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary")

by_order_files <- length(list.files(here(p_derived, "by_order"), pattern = "\\.csv$", recursive = TRUE))
by_family_files <- length(list.files(here(p_derived, "by_family"), pattern = "\\.csv$", recursive = TRUE))

cli_alert_success("Species summaries complete!")
cli_alert_info("Files in by_order/: {by_order_files}")
cli_alert_info("Files in by_family/: {by_family_files}")
cli_alert_info("Total files: {by_order_files + by_family_files}")
