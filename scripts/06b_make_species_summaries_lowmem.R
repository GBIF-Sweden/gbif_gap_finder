# scripts/06b_make_species_summaries.R
# ==============================================================================
# Create Species-Level Derived Summaries with Bias Correction
# ==============================================================================
# This script creates species-level summary tables, split intelligently:
#
# SMALL ORDERS (< threshold rows): Single file per order
#   - by_order/species_summary/species_summary_{Order}_{grid}.csv
#   - by_order/species_cell/species_cell_{Order}_{grid}.csv
#   - by_order/species_time/species_time_{Order}_{grid}.csv
#   - by_order/species_cell_time/species_cell_time_{Order}_{grid}.csv
#
# LARGE ORDERS (> threshold rows): Split by family
#   - by_family/species_summary/species_summary_{Order}_{Family}_{grid}.csv
#   - by_family/species_cell/species_cell_{Order}_{Family}_{grid}.csv
#   - by_family/species_time/species_time_{Order}_{Family}_{grid}.csv
#   - by_family/species_cell_time/species_cell_time_{Order}_{Family}_{grid}.csv
#
# All species-level tables include bias correction columns:
#   - familycount, ordercount, classcount
#   - relative_family, relative_order, relative_class
#
# Run 06a_make_core_summaries.R first to create order-level summaries.

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

# Threshold for splitting large orders into families
# Orders with more rows than this will be processed family-by-family
LARGE_ORDER_THRESHOLD <- 1000000  # 1000K rows - adjust if needed

# Toggle species × cell × time output (large files, not used by apps)
MAKE_SPECIES_CELL_TIME <- FALSE

# Paths -------------------------------------------------------------------
p_cubes <- here(p_data_proc, "cubes")
p_derived <- here(p_data_proc, "derived")

# Create output directories
dir.create(here(p_derived, "by_order", "species_summary"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_order", "species_cell"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_order", "species_time"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_family", "species_summary"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_family", "species_cell"), showWarnings = FALSE, recursive = TRUE)
dir.create(here(p_derived, "by_family", "species_time"), showWarnings = FALSE, recursive = TRUE)
if (MAKE_SPECIES_CELL_TIME) {
  dir.create(here(p_derived, "by_order", "species_cell_time"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here(p_derived, "by_family", "species_cell_time"), showWarnings = FALSE, recursive = TRUE)
}

# Validate inputs ---------------------------------------------------------
if (!dir.exists(p_cubes)) {
  cli_abort("Cubes directory not found: {.path {p_cubes}}")
}

if (!requireNamespace("fst", quietly = TRUE)) {
  cli_abort(c(
    "Package {.pkg fst} is required for reading cube files",
    "i" = "Install with: {.code install.packages('fst')}"
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
    return(invisible(NULL))
  }
  
  out_path <- here(out_dir, filename)
  fwrite(subset, out_path)
  cli_alert_success("{filename}: {scales::comma(nrow(subset))} rows")
}

#' Clean string for use in filename
clean_for_filename <- function(x) {
  x <- str_replace_all(x, "[^A-Za-z0-9]", "_")
  x <- str_replace_all(x, "_+", "_")
  str_remove(x, "^_|_$")
}

#' Calculate relative occurrence columns
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

#' Write order-level summaries (for small orders)
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

#' Write family-level summaries (for large orders)
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

#' Process a single taxon group (order or family) and create all 4 summary types
process_taxon_group <- function(cube_files, cols_species, filter_expr, 
                                 name_clean, grid_levels, p_derived, 
                                 output_type = "order") {
  
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
    
    # Recode missing order/family as "Unplaced" (consistent with scanning step)
    dt[is.na(order) | order == "", order := "Unplaced"]
    if ("family" %in% names(dt)) {
      dt[is.na(family) | family == "", family := "Unplaced"]
    }
    
    # Apply filter
    dt <- dt[eval(filter_expr)]
    
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
    if (output_type == "family") {
      species_cols <- c(species_cols, "order", "family")
    } else {
      if (have_family) species_cols <- c(species_cols, "family")
    }
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
    if (MAKE_SPECIES_CELL_TIME && have_cell && have_time) {
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
  
  # Write outputs
  if (output_type == "order") {
    write_order_summaries(name_clean, grid_levels,
                          species_summary_aggs, species_cell_aggs,
                          species_time_aggs, species_cell_time_aggs,
                          p_derived)
  } else {
    write_family_summaries(name_clean, grid_levels,
                           species_summary_aggs, species_cell_aggs,
                           species_time_aggs, species_cell_time_aggs,
                           p_derived)
  }
  
  rm(species_summary_aggs, species_cell_aggs, species_time_aggs, species_cell_time_aggs)
  invisible(gc())
}

# ===========================================================================
# MAIN PROCESSING
# ===========================================================================

cli_h1("Species-Level Summaries (Script 06b)")
cli_h2("Locating Cube Files")

cube_files <- list.files(p_cubes, pattern = "\\.fst$", full.names = TRUE)

if (length(cube_files) == 0) {
  cli_abort("No .fst cube files found in: {.path {p_cubes}}")
}

cli_alert_info("Found {length(cube_files)} cube file{?s}")
cli_alert_info("Large order threshold: {scales::comma(LARGE_ORDER_THRESHOLD)} rows")

# Columns for species-level analysis
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
# STEP 1: Scan orders and estimate sizes
# ---------------------------------------------------------------------------
cli_h2("Step 1: Scanning Orders")

order_sizes <- list()
family_size_list <- list()

cli_progress_bar("Scanning cube files", total = length(cube_files), clear = FALSE)

for (f in cube_files) {
  cli_progress_update()
  
  dt <- read_fst_cols(f, c("order", "family", "grid"))
  
  if (all(c("order", "grid") %in% names(dt))) {
    # Recode missing order/family as "Unplaced" so these species
    # are not silently dropped from downstream analyses
    dt[is.na(order) | order == "", order := "Unplaced"]
    counts <- dt[, .N, by = .(grid, order)]
    order_sizes[[length(order_sizes) + 1]] <- counts
    
    if ("family" %in% names(dt)) {
      dt[is.na(family) | family == "", family := "Unplaced"]
      family_counts <- dt[, .N, by = .(grid, order, family)]
      family_size_list[[length(family_size_list) + 1]] <- family_counts
    }
  }
  rm(dt)
  invisible(gc())
}

cli_progress_done()

# Combine sizes
order_totals <- rbindlist(order_sizes, use.names = TRUE, fill = TRUE)
order_totals <- order_totals[, .(total_rows = sum(N)), by = .(grid, order)]

family_totals <- rbindlist(family_size_list, use.names = TRUE, fill = TRUE)
family_totals <- family_totals[, .(total_rows = sum(N)), by = .(grid, order, family)]

# Classify orders
large_orders <- order_totals[total_rows > LARGE_ORDER_THRESHOLD]
small_orders <- order_totals[total_rows <= LARGE_ORDER_THRESHOLD]

grid_levels <- unique(order_totals$grid) |> na.omit()

cli_alert_info("Found {uniqueN(order_totals$order)} unique orders")
cli_alert_info("Small orders (single file): {uniqueN(small_orders$order)}")
cli_alert_info("Large orders (split by family): {uniqueN(large_orders$order)}")

if (nrow(large_orders) > 0) {
  cli_alert_info("Large orders:")
  print(large_orders[, .(order, total_rows = sum(total_rows)), by = order][order(-total_rows)][1:min(10, .N)])
}

# ---------------------------------------------------------------------------
# STEP 2: Process SMALL orders
# ---------------------------------------------------------------------------
if (nrow(small_orders) > 0) {
  cli_h2("Step 2: Processing Small Orders")
  
  orders_to_process <- unique(small_orders$order)
  
  cli_progress_bar("Small orders", total = length(orders_to_process), clear = FALSE)
  
  for (current_order in orders_to_process) {
    cli_progress_update()
    
    if (is.na(current_order) || current_order == "") next
    # Note: "Unplaced" orders are valid — they were recoded in Step 1
    
    order_clean <- clean_for_filename(current_order)
    filter_expr <- substitute(order == x, list(x = current_order))
    
    process_taxon_group(
      cube_files = cube_files,
      cols_species = cols_species,
      filter_expr = filter_expr,
      name_clean = order_clean,
      grid_levels = grid_levels,
      p_derived = p_derived,
      output_type = "order"
    )
  }
  
  cli_progress_done()
}

# ---------------------------------------------------------------------------
# STEP 3: Process LARGE orders (by family)
# ---------------------------------------------------------------------------
if (nrow(large_orders) > 0) {
  cli_h2("Step 3: Processing Large Orders (by family)")
  
  large_order_names <- unique(large_orders$order)
  
  for (current_order in large_order_names) {
    cli_alert_info("Processing large order: {current_order}")
    
    # Get families in this order (NA/empty already recoded to "Unplaced" in Step 1)
    families_in_order <- family_totals[order == current_order, unique(family)]
    
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
      combined_name <- paste0(order_clean, "_", family_clean)
      
      filter_expr <- substitute(
        order == o & family == f, 
        list(o = current_order, f = current_family)
      )
      
      process_taxon_group(
        cube_files = cube_files,
        cols_species = cols_species,
        filter_expr = filter_expr,
        name_clean = combined_name,
        grid_levels = grid_levels,
        p_derived = p_derived,
        output_type = "family"
      )
    }
    
    cli_progress_done()
  }
}

# ===========================================================================
# SUMMARY
# ===========================================================================

cli_h1("Summary (Script 06b)")

# Count output files
by_order_files <- list.files(here(p_derived, "by_order"), pattern = "\\.csv$", recursive = TRUE)
by_family_files <- list.files(here(p_derived, "by_family"), pattern = "\\.csv$", recursive = TRUE)

summary_info <- tibble::tribble(
  ~Category, ~Count, ~Location,
  "Species by order (small orders)", length(by_order_files), "derived/by_order/",
  "Species by family (large orders)", length(by_family_files), "derived/by_family/"
)

print(summary_info)

cli_alert_success("Species summaries complete!")
cli_alert_info("Output location: {.path {p_derived}}")
cli_alert_info("Species tables include bias correction columns:")
cli_alert_info("  - familycount, ordercount, classcount")
cli_alert_info("  - relative_family, relative_order, relative_class")
