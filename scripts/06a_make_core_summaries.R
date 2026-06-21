# scripts/06a_make_core_summaries.R
# ============================================================================
# Core & Order-Level Derived Summaries + Grid Lookups
# ============================================================================
# Purpose:
#   Create aggregate summary tables from GBIF parquet cubes.
#   These are FULL-SCOPE summaries (all GBIF data). Taxonomy-scoped
#   variants (_dyntaxa, threatened, invasive, sensitive) are produced
#   by script 09c after reconciliation is complete.
#
# GRID LOOKUPS:
#   - grid_lookup_10km.csv   Maps poly_id -> eeacellcode
#   - grid_lookup_50km.csv
#
# CORE SUMMARIES:
#   - cell_summary_<grid>.csv         Cell x basis
#   - time_summary_<grid>.csv         Yearmonth x basis
#   - cell_time_summary_<grid>.csv    Cell x yearmonth x basis
#   - order_cell_summary_<grid>.csv   Order x cell x basis
#   - order_time_summary_<grid>.csv   Order x yearmonth x basis
#   - family_time_summary_<grid>.csv  Family x yearmonth x basis
#   - publisher_summary_<grid>.csv    Publisher x basis
#   - cube_key_summary.csv            File-level stats
#
# Inputs:  data/{CC}/proc/cubes/*.parquet (from 04)
#          data/{CC}/proc/grids_*.gpkg (from 02)
# Outputs: data/{CC}/proc/derived/*.csv
#
# Dependencies: scripts/00_setup.R, data.table, arrow, sf
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# Script-specific packages
library(arrow)
library(httr)

# ============================================================================
# Configuration
# ============================================================================

MAKE_GRID_LOOKUPS    <- TRUE
MAKE_CORE_SUMMARIES  <- TRUE
MAKE_CELL_TIME       <- TRUE
MAKE_ORDER_SUMMARIES <- TRUE
MAKE_PUBLISHER_SUMMARY <- TRUE

# p_cubes, p_derived are defined in R/globals.R
# Directories created by ensure_dirs() in 00_setup.R

# ============================================================================
# Helper Functions
# ============================================================================

# read_cube(), safe_sum(), safe_max() are defined in R/globals.R

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
    fwrite(lookup, here(p_derived, gi$output))
    cli_alert_success("{gi$output}: {scales::comma(nrow(lookup))} cells")
    rm(grid); gc()
  }
}

# ============================================================================
# Phase 1: Core Summaries (Cell, Time) -- one resolution at a time
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

    # Cell summary: cell x basis
    cell_dt <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey)
    ), by = .(grid, basisofrecord, eeacellcode)]
    cell_all <- dt[, .(
      occurrences = safe_sum(occurrences),
      n_species = uniqueN(specieskey)
    ), by = .(grid, eeacellcode)]
    cell_all[, basisofrecord := "all"]
    cell_result <- rbindlist(list(cell_dt, cell_all), use.names = TRUE, fill = TRUE)
    fwrite(cell_result, here(p_derived, glue("cell_summary_{grid_suffix}.csv")))
    cli_alert_success("cell_summary_{grid_suffix}.csv: {scales::comma(nrow(cell_result))} rows")
    rm(cell_dt, cell_all, cell_result)

    # Time summary: yearmonth x basis
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
    time_result <- rbindlist(list(time_dt, time_all), use.names = TRUE, fill = TRUE)
    fwrite(time_result, here(p_derived, glue("time_summary_{grid_suffix}.csv")))
    cli_alert_success("time_summary_{grid_suffix}.csv: {scales::comma(nrow(time_result))} rows")
    rm(time_dt, time_all, time_result)

    rm(dt); gc()
  }

  # Write cube key summary
  key_df <- rbindlist(key_stats, fill = TRUE)
  fwrite(key_df, here(p_derived, "cube_key_summary.csv"))
  cli_alert_success("cube_key_summary.csv")
}

# ============================================================================
# Phase 2: Cell x Time Summary
# ============================================================================

if (MAKE_CELL_TIME) {
  cli_h2("Phase 2: Cell x Time Summary")

  for (grid_name in names(grid_map)) {
    grid_suffix <- str_extract(grid_name, "\\d+km")
    pf <- grid_map[[grid_name]]
    cli_h3("{grid_name}")

    dt <- read_cube(pf, cols = c("specieskey", "species", "basisofrecord",
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
    ct_result <- rbindlist(list(ct_dt, ct_all), use.names = TRUE, fill = TRUE)
    fwrite(ct_result, here(p_derived, glue("cell_time_summary_{grid_suffix}.csv")))
    cli_alert_success("cell_time_summary_{grid_suffix}.csv: {scales::comma(nrow(ct_result))} rows")

    rm(dt, ct_dt, ct_all, ct_result); gc()
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

    dt <- read_cube(pf, cols = c("specieskey", "species", "basisofrecord",
      "eeacellcode", "year", "month", "order", "family", "occurrences"),
      grid_label = grid_name)

    # --- Order x Cell ---
    oc_dt <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family)), by = .(grid, basisofrecord, order, eeacellcode)]
    oc_all <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family)), by = .(grid, order, eeacellcode)]
    oc_all[, basisofrecord := "all"]
    oc_result <- rbindlist(list(oc_dt, oc_all), use.names = TRUE, fill = TRUE)
    fwrite(oc_result, here(p_derived, glue("order_cell_summary_{grid_suffix}.csv")))
    cli_alert_success("order_cell_summary_{grid_suffix}.csv: {scales::comma(nrow(oc_result))} rows")
    rm(oc_dt, oc_all, oc_result)

    # --- Order x Time ---
    ot_dt <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family), n_cells = uniqueN(eeacellcode)),
      by = .(grid, basisofrecord, order, yearmonth)]
    ot_all <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_families = uniqueN(family), n_cells = uniqueN(eeacellcode)),
      by = .(grid, order, yearmonth)]
    ot_all[, basisofrecord := "all"]
    ot_result <- rbindlist(list(ot_dt, ot_all), use.names = TRUE, fill = TRUE)
    fwrite(ot_result, here(p_derived, glue("order_time_summary_{grid_suffix}.csv")))
    cli_alert_success("order_time_summary_{grid_suffix}.csv: {scales::comma(nrow(ot_result))} rows")
    rm(ot_dt, ot_all, ot_result)

    # --- Family x Time ---
    ft_dt <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_cells = uniqueN(eeacellcode)),
      by = .(grid, basisofrecord, order, family, yearmonth)]
    ft_all <- dt[, .(occurrences = safe_sum(occurrences), n_species = uniqueN(specieskey),
      n_cells = uniqueN(eeacellcode)),
      by = .(grid, order, family, yearmonth)]
    ft_all[, basisofrecord := "all"]
    ft_result <- rbindlist(list(ft_dt, ft_all), use.names = TRUE, fill = TRUE)
    fwrite(ft_result, here(p_derived, glue("family_time_summary_{grid_suffix}.csv")))
    cli_alert_success("family_time_summary_{grid_suffix}.csv: {scales::comma(nrow(ft_result))} rows")
    rm(ft_dt, ft_all, ft_result)

    rm(dt); gc()
  }
}

# ============================================================================
# Phase 4: Publisher Summary
# ============================================================================

if (MAKE_PUBLISHER_SUMMARY) {
  cli_h2("Phase 4: Publisher Summary")

  for (grid_name in names(grid_map)) {
    grid_suffix <- str_extract(grid_name, "\\d+km")
    pf <- grid_map[[grid_name]]
    cli_h3("{grid_name}")

    dt <- read_cube(pf, cols = c("specieskey", "species", "basisofrecord",
      "publishingorgkey", "datasetkey", "eeacellcode",
      "kingdom", "phylum", "class", "order", "family",
      "year", "month",
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

    pub_dt[is.infinite(min_year), min_year := NA_real_]
    pub_dt[is.infinite(max_year), max_year := NA_real_]

    # Dominant basis of record per publisher
    pub_bor <- dt[!is.na(publishingorgkey) & publishingorgkey != "", .(
      occ = safe_sum(occurrences)
    ), by = .(publishingorgkey, basisofrecord)]
    pub_bor_total <- pub_bor[, .(total = sum(occ)), by = publishingorgkey]
    pub_bor <- merge(pub_bor, pub_bor_total, by = "publishingorgkey")
    pub_bor[, pct := round(100 * occ / total, 1)]
    pub_dominant_bor <- pub_bor[pub_bor[, .I[which.max(occ)], by = publishingorgkey]$V1]
    pub_dt <- merge(pub_dt,
      pub_dominant_bor[, .(publishingorgkey, dominant_bor = basisofrecord,
                           dominant_bor_pct = pct)],
      by = "publishingorgkey", all.x = TRUE)

    fwrite(pub_dt, here(p_derived, glue("publisher_summary_{grid_suffix}.csv")))
    cli_alert_success("publisher_summary_{grid_suffix}.csv: {nrow(pub_dt)} publishers")

    # Publisher x taxonomy cross-tab: publisher x order (for taxonomic filtering)
    pub_tax <- dt[!is.na(publishingorgkey) & publishingorgkey != "" &
                  !is.na(order) & order != "", .(
      total_occurrences = safe_sum(occurrences),
      n_species = as.double(uniqueN(specieskey)),
      n_cells = as.double(uniqueN(eeacellcode))
    ), by = .(grid, publishingorgkey, kingdom, class, order)]
    fwrite(pub_tax, here(p_derived, glue("publisher_taxonomy_{grid_suffix}.csv")))
    cli_alert_success("publisher_taxonomy_{grid_suffix}.csv: {scales::comma(nrow(pub_tax))} rows")

    # Publisher x cell x order: for taxonomy-filtered dependency maps
    pub_cell_tax <- dt[!is.na(publishingorgkey) & publishingorgkey != "" &
                       !is.na(order) & order != "", .(
      occurrences = safe_sum(occurrences)
    ), by = .(grid, eeacellcode, publishingorgkey, kingdom, class, order)]
    fwrite(pub_cell_tax, here(p_derived, glue("publisher_cell_taxonomy_{grid_suffix}.csv")))
    cli_alert_success("publisher_cell_taxonomy_{grid_suffix}.csv: {scales::comma(nrow(pub_cell_tax))} rows")

    # Publisher x cell: which cells depend on which publishers (unfiltered)
    pub_cell <- dt[, .(
      n_publishers = uniqueN(publishingorgkey),
      total_occurrences = safe_sum(occurrences)
    ), by = .(grid, eeacellcode)]

    fwrite(pub_cell, here(p_derived, glue("publisher_cell_dependency_{grid_suffix}.csv")))
    n_single <- sum(pub_cell$n_publishers == 1)
    cli_alert_info("Cells with single publisher: {n_single}/{nrow(pub_cell)}")

    rm(dt, pub_dt, pub_cell, pub_bor, pub_bor_total, pub_dominant_bor, pub_tax, pub_cell_tax); gc()
  }

  # Resolve publisher UUIDs to names via GBIF Registry API (cached)
  cli_h3("Resolving Publisher Names")

  publisher_cache_path <- here(p_data_proc, "publisher_name_cache.rds")
  publisher_cache <- if (file.exists(publisher_cache_path)) readRDS(publisher_cache_path) else list()

  pub_10km_path <- here(p_derived, "publisher_summary_10km.csv")
  if (file.exists(pub_10km_path)) {
    pub_all <- fread(pub_10km_path)
    uuids <- unique(pub_all$publishingorgkey)
    uuids <- uuids[!is.na(uuids) & uuids != ""]
    new_uuids <- setdiff(uuids, names(publisher_cache))

    if (length(new_uuids) > 0) {
      cli_alert_info("Querying GBIF for {length(new_uuids)} new publisher names...")
      for (i in seq_along(new_uuids)) {
        uuid <- new_uuids[i]
        tryCatch({
          resp <- httr::GET(paste0("https://api.gbif.org/v1/organization/", uuid))
          if (httr::status_code(resp) == 200) {
            info <- httr::content(resp, as = "parsed")
            publisher_cache[[uuid]] <- list(
              title = info$title %||% NA_character_,
              country = info$country %||% NA_character_
            )
          } else {
            publisher_cache[[uuid]] <- list(title = NA_character_, country = NA_character_)
          }
        }, error = function(e) {
          publisher_cache[[uuid]] <<- list(title = NA_character_, country = NA_character_)
        })
        if (i %% 50 == 0) cli_alert_info("  {i}/{length(new_uuids)} resolved")
        Sys.sleep(0.1)
      }
      saveRDS(publisher_cache, publisher_cache_path)
      cli_alert_success("Cached {length(publisher_cache)} publisher names")
    } else {
      cli_alert_info("All {length(uuids)} publishers already cached")
    }

    pub_names <- data.table(
      publishingorgkey = names(publisher_cache),
      publisher_name = vapply(publisher_cache, function(x) x$title %||% NA_character_, character(1)),
      publisher_country = vapply(publisher_cache, function(x) x$country %||% NA_character_, character(1))
    )
    fwrite(pub_names, here(p_derived, "publisher_names.csv"))
    cli_alert_success("publisher_names.csv: {nrow(pub_names)} names")

    for (grid_suffix in c("10km", "50km")) {
      summary_path <- here(p_derived, glue("publisher_summary_{grid_suffix}.csv"))
      if (file.exists(summary_path)) {
        ps <- fread(summary_path)
        ps <- merge(ps, pub_names, by = "publishingorgkey", all.x = TRUE)
        fwrite(ps, summary_path)
      }
    }
    cli_alert_success("Publisher summaries enriched with names")
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
  "Cell x time" = "^cell_time",
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
