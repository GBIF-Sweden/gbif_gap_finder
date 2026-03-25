# scripts/06a_make_core_summaries.R
# ============================================================================
# Core & Order-Level Derived Summaries + Grid Lookups
# ============================================================================
# Purpose:
#   Create aggregate summary tables from GBIF parquet cubes.
#
# GRID LOOKUPS:
#   - grid_lookup_10km.csv   Maps poly_id → eeacellcode
#   - grid_lookup_50km.csv
#
# CORE SUMMARIES:
#   - cell_summary_<grid>.csv         Cell × basis
#   - time_summary_<grid>.csv         Yearmonth × basis
#   - cell_time_summary_<grid>.csv    Cell × yearmonth × basis
#   - order_cell_summary_<grid>.csv   Order × cell × basis
#   - order_time_summary_<grid>.csv   Order × yearmonth × basis
#   - family_time_summary_<grid>.csv  Family × yearmonth × basis
#   - publisher_summary_<grid>.csv    Publisher × basis (NEW)
#   - cube_key_summary.csv            File-level stats
#
# Inputs:  data/{CC}/proc/cubes/*.parquet (from 04)
#          data/{CC}/proc/grids_*.gpkg (from 02)
# Outputs: data/{CC}/proc/derived/*.csv
#
# Dependencies: scripts/00_setup.R, data.table, arrow, sf
# ============================================================================

library(here)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(data.table)
library(glue)
library(cli)
library(sf)
library(arrow)

source(here("scripts", "00_setup.R"))

# ============================================================================
# Configuration
# ============================================================================

MAKE_GRID_LOOKUPS    <- TRUE
MAKE_CORE_SUMMARIES  <- TRUE
MAKE_CELL_TIME       <- TRUE
MAKE_ORDER_SUMMARIES <- TRUE
MAKE_PUBLISHER_SUMMARY <- TRUE

p_cubes   <- here(p_data_proc, "cubes")
p_derived <- here(p_data_proc, "derived")
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Helper Functions
# ============================================================================

safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)
safe_max <- function(x) { if (all(is.na(x))) NA_real_ else max(as.numeric(x), na.rm = TRUE) }

#' Read a parquet cube into data.table, adding yearmonth and grid columns
read_cube <- function(parquet_path, cols = NULL, grid_label = NULL) {
  if (!file.exists(parquet_path)) cli_abort("Not found: {.path {parquet_path}}")

  ds <- open_dataset(parquet_path)
  if (!is.null(cols)) {
    available <- intersect(cols, names(ds$schema))
    ds <- ds |> select(all_of(available))
  }
  dt <- as.data.table(collect(ds))

  # Create yearmonth from year + month (for backward compatibility)
  if (all(c("year", "month") %in% names(dt)) && !"yearmonth" %in% names(dt)) {
    dt[, yearmonth := year * 100L + as.integer(month)]
  }

  # Add grid label
  if (!is.null(grid_label)) dt[, grid := grid_label]

  # Recode missing taxonomy
  if ("order" %in% names(dt))  dt[is.na(order)  | order  == "", order  := "Unplaced"]
  if ("family" %in% names(dt)) dt[is.na(family) | family == "", family := "Unplaced"]

  dt
}

#' Add "all" basis rows to a summary
add_all_basis <- function(dt, group_cols, agg_exprs) {
  group_no_basis <- setdiff(group_cols, "basisofrecord")
  all_dt <- dt[, lapply(.SD, safe_sum), by = group_no_basis, .SDcols = intersect(c("occurrences"), names(dt))]

  # Handle n_species etc. with max instead of sum
  for (col in intersect(c("n_species", "n_families", "n_cells", "n_publishers", "n_datasets"), names(dt))) {
    vals <- dt[, .(v = safe_max(get(col))), by = group_no_basis]
    all_dt <- merge(all_dt, vals, by = group_no_basis, all.x = TRUE)
    setnames(all_dt, "v", col)
  }

  all_dt[, basisofrecord := "all"]
  rbindlist(list(dt, all_dt), use.names = TRUE, fill = TRUE)
}

#' Write grid-specific output
write_grid_output <- function(dt, grid_value, out_dir, filename) {
  subset <- if ("grid" %in% names(dt)) dt[grid == grid_value] else dt
  if (nrow(subset) == 0) return(invisible(NULL))
  fwrite(subset, here(out_dir, filename))
  cli_alert_success("{filename}: {scales::comma(nrow(subset))} rows")
}

# ============================================================================
# Locate Parquet Files
# ============================================================================

cli_h1("Core & Order-Level Summaries (Script 06a)")

parquet_files <- list.files(p_cubes, pattern = "\\.parquet$", full.names = TRUE)
if (length(parquet_files) == 0) cli_abort("No parquet files in: {.path {p_cubes}}")

# Detect grid resolutions from filenames
grid_map <- list()
for (pf in parquet_files) {
  bn <- basename(pf)
  if (grepl("10km", bn)) grid_map[["grid10km"]] <- pf
  else if (grepl("50km", bn)) grid_map[["grid50km"]] <- pf
}

cli_alert_info("Found {length(grid_map)} parquet cube(s): {paste(names(grid_map), collapse = ', ')}")

# ============================================================================
# Phase 0: Grid Lookup Tables
# ============================================================================

if (MAKE_GRID_LOOKUPS) {
  cli_h2("Phase 0: Grid Lookup Tables")

  grid_gpkg <- list(
    grid10km = list(path = here(p_data_proc, "grids_10km.gpkg"), output = "grid_lookup_10km.csv", label = "10km"),
    grid50km = list(path = here(p_data_proc, "grids_50km.gpkg"), output = "grid_lookup_50km.csv", label = "50km")
  )

  for (nm in names(grid_gpkg)) {
    gi <- grid_gpkg[[nm]]
    if (!file.exists(gi$path)) { cli_alert_warning("{nm} grid not found"); next }

    grid <- st_read(gi$path, quiet = TRUE)
    code_field <- guess_cellcode_field(names(grid))
    if (is.na(code_field)) { cli_alert_warning("No cell code field in {nm}"); next }

    poly_ids <- glue("{gi$label}_{str_pad(seq_len(nrow(grid)), width = 6, pad = '0')}")
    lookup <- tibble(poly_id = as.character(poly_ids), eeacellcode = as.character(grid[[code_field]]))
    write_csv(lookup, here(p_derived, gi$output))
    cli_alert_success("{gi$output}: {scales::comma(nrow(lookup))} cells")
    rm(grid); gc()
  }
}

# ============================================================================
# Phase 1: Core Summaries (Cell, Time) — one resolution at a time
# ============================================================================

if (MAKE_CORE_SUMMARIES) {
  cli_h2("Phase 1: Core Summaries (Cell, Time)")

  key_stats <- list()

  for (grid_name in names(grid_map)) {
    grid_suffix <- str_extract(grid_name, "\\d+km")
    pf <- grid_map[[grid_name]]
    cli_h3("{grid_name}")

    dt <- read_cube(pf, cols = c("specieskey", "species", "basisofrecord",
      "eeacellcode", "year", "month", "occurrences",
      "publishingorgkey", "datasetkey"), grid_label = grid_name)

    cli_alert_info("{scales::comma(nrow(dt))} rows loaded")

    # File-level stats
    key_stats[[grid_name]] <- data.table(
      grid = grid_name,
      rows = nrow(dt),
      total_occurrences = safe_sum(dt$occurrences),
      n_cells = uniqueN(dt$eeacellcode),
      n_months = uniqueN(dt$yearmonth),
      n_species = uniqueN(dt$specieskey),
      n_publishers = if ("publishingorgkey" %in% names(dt)) uniqueN(dt$publishingorgkey) else NA_integer_,
      n_datasets = if ("datasetkey" %in% names(dt)) uniqueN(dt$datasetkey) else NA_integer_
    )

    # Cell summary: cell × basis
    cell_dt <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey)
    ), by = .(grid, basisofrecord, eeacellcode)]

    cell_all <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey)
    ), by = .(grid, eeacellcode)]
    cell_all[, basisofrecord := "all"]

    cell_combined <- rbindlist(list(cell_dt, cell_all), use.names = TRUE, fill = TRUE)
    fwrite(cell_combined, here(p_derived, glue("cell_summary_{grid_suffix}.csv")))
    cli_alert_success("cell_summary_{grid_suffix}.csv: {scales::comma(nrow(cell_combined))} rows")
    rm(cell_dt, cell_all, cell_combined)

    # Time summary: yearmonth × basis
    time_dt <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey),
      n_cells = uniqueN(eeacellcode)
    ), by = .(grid, basisofrecord, yearmonth)]

    time_all <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey),
      n_cells = uniqueN(eeacellcode)
    ), by = .(grid, yearmonth)]
    time_all[, basisofrecord := "all"]

    time_combined <- rbindlist(list(time_dt, time_all), use.names = TRUE, fill = TRUE)
    fwrite(time_combined, here(p_derived, glue("time_summary_{grid_suffix}.csv")))
    cli_alert_success("time_summary_{grid_suffix}.csv: {scales::comma(nrow(time_combined))} rows")
    rm(time_dt, time_all, time_combined)

    rm(dt); gc()
  }

  # Write cube key summary
  key_df <- rbindlist(key_stats, fill = TRUE)
  fwrite(key_df, here(p_derived, "cube_key_summary.csv"))
  cli_alert_success("cube_key_summary.csv")
}

# ============================================================================
# Phase 2: Cell × Time Summary
# ============================================================================

if (MAKE_CELL_TIME) {
  cli_h2("Phase 2: Cell \u00d7 Time Summary")

  for (grid_name in names(grid_map)) {
    grid_suffix <- str_extract(grid_name, "\\d+km")
    pf <- grid_map[[grid_name]]
    cli_h3("{grid_name}")

    dt <- read_cube(pf, cols = c("specieskey", "basisofrecord",
      "eeacellcode", "year", "month", "occurrences"), grid_label = grid_name)

    ct_dt <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey)
    ), by = .(grid, basisofrecord, eeacellcode, yearmonth)]

    ct_all <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey)
    ), by = .(grid, eeacellcode, yearmonth)]
    ct_all[, basisofrecord := "all"]

    ct_combined <- rbindlist(list(ct_dt, ct_all), use.names = TRUE, fill = TRUE)
    fwrite(ct_combined, here(p_derived, glue("cell_time_summary_{grid_suffix}.csv")))
    cli_alert_success("cell_time_summary_{grid_suffix}.csv: {scales::comma(nrow(ct_combined))} rows")

    rm(dt, ct_dt, ct_all, ct_combined); gc()
  }
}

# ============================================================================
# Phase 3: Order-Level Summaries
# ============================================================================

if (MAKE_ORDER_SUMMARIES) {
  cli_h2("Phase 3: Order-Level Summaries")

  for (grid_name in names(grid_map)) {
    grid_suffix <- str_extract(grid_name, "\\d+km")
    pf <- grid_map[[grid_name]]
    cli_h3("{grid_name}")

    dt <- read_cube(pf, cols = c("specieskey", "basisofrecord",
      "eeacellcode", "year", "month", "order", "family", "occurrences"),
      grid_label = grid_name)

    # --- Order × Cell ---
    oc_dt <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family)), by = .(grid, basisofrecord, order, eeacellcode)]
    oc_all <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family)), by = .(grid, order, eeacellcode)]
    oc_all[, basisofrecord := "all"]
    oc_combined <- rbindlist(list(oc_dt, oc_all), use.names = TRUE, fill = TRUE)
    fwrite(oc_combined, here(p_derived, glue("order_cell_summary_{grid_suffix}.csv")))
    cli_alert_success("order_cell_summary_{grid_suffix}.csv: {scales::comma(nrow(oc_combined))} rows")
    rm(oc_dt, oc_all, oc_combined)

    # --- Order × Time ---
    ot_dt <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family), n_cells = uniqueN(eeacellcode)),
      by = .(grid, basisofrecord, order, yearmonth)]
    ot_all <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family), n_cells = uniqueN(eeacellcode)),
      by = .(grid, order, yearmonth)]
    ot_all[, basisofrecord := "all"]
    ot_combined <- rbindlist(list(ot_dt, ot_all), use.names = TRUE, fill = TRUE)
    fwrite(ot_combined, here(p_derived, glue("order_time_summary_{grid_suffix}.csv")))
    cli_alert_success("order_time_summary_{grid_suffix}.csv: {scales::comma(nrow(ot_combined))} rows")
    rm(ot_dt, ot_all, ot_combined)

    # --- Family × Time ---
    ft_dt <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_cells = uniqueN(eeacellcode)),
      by = .(grid, basisofrecord, order, family, yearmonth)]
    ft_all <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_cells = uniqueN(eeacellcode)),
      by = .(grid, order, family, yearmonth)]
    ft_all[, basisofrecord := "all"]
    ft_combined <- rbindlist(list(ft_dt, ft_all), use.names = TRUE, fill = TRUE)
    fwrite(ft_combined, here(p_derived, glue("family_time_summary_{grid_suffix}.csv")))
    cli_alert_success("family_time_summary_{grid_suffix}.csv: {scales::comma(nrow(ft_combined))} rows")

    rm(dt, ft_dt, ft_all, ft_combined); gc()
  }
}

# ============================================================================
# Phase 4: Publisher Summary (NEW)
# ============================================================================

if (MAKE_PUBLISHER_SUMMARY) {
  cli_h2("Phase 4: Publisher Summary")

  for (grid_name in names(grid_map)) {
    grid_suffix <- str_extract(grid_name, "\\d+km")
    pf <- grid_map[[grid_name]]
    cli_h3("{grid_name}")

    dt <- read_cube(pf, cols = c("specieskey", "basisofrecord",
      "publishingorgkey", "datasetkey", "eeacellcode",
      "year", "month", "year_published", "month_published",
      "occurrences"), grid_label = grid_name)

    # Publisher overview: who contributes what
    has_dataset <- "datasetkey" %in% names(dt)
    pub_dt <- dt[!is.na(publishingorgkey) & publishingorgkey != "", .(
      total_occurrences = safe_sum(occurrences),
      n_species = as.double(uniqueN(specieskey)),
      n_cells = as.double(uniqueN(eeacellcode)),
      n_datasets = if (has_dataset) as.double(uniqueN(datasetkey)) else NA_real_,
      min_year = as.double(min(as.integer(year), na.rm = TRUE)),
      max_year = as.double(max(as.integer(year), na.rm = TRUE))
    ), by = .(grid, publishingorgkey)]

    # Fix Inf/-Inf from empty groups
    pub_dt[is.infinite(min_year), min_year := NA_real_]
    pub_dt[is.infinite(max_year), max_year := NA_real_]

    fwrite(pub_dt, here(p_derived, glue("publisher_summary_{grid_suffix}.csv")))
    cli_alert_success("publisher_summary_{grid_suffix}.csv: {nrow(pub_dt)} publishers")

    # Publisher × cell: which cells depend on which publishers
    pub_cell <- dt[, .(
      n_publishers = uniqueN(publishingorgkey),
      total_occurrences = safe_sum(occurrences)
    ), by = .(grid, eeacellcode)]

    fwrite(pub_cell, here(p_derived, glue("publisher_cell_dependency_{grid_suffix}.csv")))
    n_single <- sum(pub_cell$n_publishers == 1)
    cli_alert_info("Cells with single publisher: {n_single}/{nrow(pub_cell)}")

    # Published vs observed: year_published summary
    if ("year_published" %in% names(dt)) {
      pub_vs_obs <- dt[, .(
        occurrences = safe_sum(occurrences),
        n_species = uniqueN(specieskey)
      ), by = .(grid, year_published, month_published)]

      fwrite(pub_vs_obs, here(p_derived, glue("published_time_summary_{grid_suffix}.csv")))
      cli_alert_success("published_time_summary_{grid_suffix}.csv: {scales::comma(nrow(pub_vs_obs))} rows")
    }

    rm(dt, pub_dt, pub_cell); gc()
  }
}

# ============================================================================
# Summary
# ============================================================================

cli_h1("Summary (Script 06a)")

output_files <- list.files(p_derived, pattern = "\\.csv$")
cli_alert_success("Total output files: {length(output_files)}")

categories <- list(
  "Grid lookups" = "grid_lookup",
  "Cell summaries" = "^cell_summary",
  "Time summaries" = "^time_summary",
  "Cell × time" = "^cell_time",
  "Order summaries" = "^order_",
  "Family summaries" = "^family_",
  "Publisher summaries" = "^publisher_|^published_",
  "Cube key" = "^cube_key"
)

for (cat_name in names(categories)) {
  n <- sum(grepl(categories[[cat_name]], output_files))
  if (n > 0) cli_alert_info("  {cat_name}: {n} files")
}

cli_alert_success("Core summaries complete!")
cli_alert_info("Next: source('scripts/06b_make_species_summaries.R')")
