# scripts/04_convert_cubes_parquet.R
# ============================================================================
# Convert GBIF Occurrence Cube CSVs to Parquet
# ============================================================================
# Purpose:
#   Convert the raw GBIF SQL API cube downloads (CSV/TSV) to parquet
#   format for fast columnar access by downstream scripts.
#
#   Raw CSVs are kept untouched in data/{CC}/raw/cubes/.
#   Parquet files are written to data/{CC}/proc/cubes/.
#
# Inputs:
#   - data/{CC}/raw/cubes/cube_10km.csv
#   - data/{CC}/raw/cubes/cube_50km.csv
#
# Outputs:
#   - data/{CC}/proc/cubes/cube_10km.parquet
#   - data/{CC}/proc/cubes/cube_50km.parquet
#   - data/{CC}/proc/cubes/cube_manifest.csv
#
# Expected columns (17 — 14 base dimensions/measure + 3 b-cubed measures):
#   specieskey, species, kingdom, phylum, class, order, family,
#   basisofrecord, publishingorgkey, datasetkey, eeacellcode,
#   year, month, occurrences,
#   mincoordinateuncertaintyinmeters, mintemporaluncertainty, distinctobservers
#
# Dependencies: scripts/00_setup.R, data.table, arrow
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# Script-specific package
if (!requireNamespace("arrow", quietly = TRUE)) {
  cli_abort(c("Package {.pkg arrow} is required",
    "i" = "Install: {.code install.packages('arrow')}"))
}
library(arrow)

# ============================================================================
# Configuration
# ============================================================================

cube_raw_dir  <- here(raw_gbif_cube_dir)
cube_proc_dir <- here(p_data_proc, "cubes")
# Directory created by ensure_dirs() in 00_setup.R

cube_files <- list(
  grid10km = list(
    csv     = here(cube_raw_dir, cfg_get("files.cubes.grid10km", "cube_10km.csv")),
    parquet = here(cube_proc_dir, "cube_10km.parquet")
  ),
  grid50km = list(
    csv     = here(cube_raw_dir, cfg_get("files.cubes.grid50km", "cube_50km.csv")),
    parquet = here(cube_proc_dir, "cube_50km.parquet")
  )
)

# Expected columns from the SQL API download
expected_cols <- c("specieskey", "species", "kingdom", "phylum", "class",
  "order", "family", "basisofrecord", "publishingorgkey", "datasetkey",
  "eeacellcode", "year", "month", "occurrences",
  # b-cubed standard measures (b3verse schema migration)
  "mincoordinateuncertaintyinmeters", "mintemporaluncertainty", "distinctobservers")

cli_h1("Convert GBIF Cubes to Parquet \u2014 {COUNTRY_CODE}")

# ============================================================================
# Convert each cube
# ============================================================================

manifest <- list()

for (grid_name in names(cube_files)) {
  cf <- cube_files[[grid_name]]
  cli_h2("{grid_name}")

  # Check if parquet already exists
  if (file.exists(cf$parquet)) {
    pq_size <- round(file.size(cf$parquet) / 1024^2, 1)
    cli_alert_info("Parquet exists: {basename(cf$parquet)} ({pq_size} MB) \u2014 skipping")
    cli_alert_info("Delete to re-convert")

    # Still add to manifest
    ds <- open_dataset(cf$parquet)
    manifest[[grid_name]] <- data.frame(
      grid = grid_name, rows = ds$num_rows %||% NA_integer_,
      parquet_mb = pq_size, status = "skipped"
    )
    next
  }

  # Check if CSV exists
  if (!file.exists(cf$csv)) {
    cli_alert_warning("CSV not found: {.path {cf$csv}} \u2014 skipping")
    next
  }

  csv_size <- round(file.size(cf$csv) / 1024^2, 1)
  cli_alert_info("Reading CSV: {basename(cf$csv)} ({csv_size} MB)")

  # Read CSV
  dt <- fread(cf$csv, showProgress = TRUE, encoding = "UTF-8")
  cli_alert_success("Read {scales::comma(nrow(dt))} rows, {ncol(dt)} columns")

  # Validate columns. Analytically-required columns are a HARD STOP: a shifted or
  # absent basisofrecord/kingdom silently zeroes the by-basis and taxonomic
  # analyses downstream, so fail here rather than write a corrupt parquet.
  missing_cols  <- setdiff(expected_cols, names(dt))
  extra_cols    <- setdiff(names(dt), expected_cols)
  required_cols <- c("specieskey", "eeacellcode", "year", "month", "occurrences",
                     "basisofrecord", "kingdom", "publishingorgkey", "datasetkey")
  missing_required <- setdiff(required_cols, names(dt))
  if (length(missing_required) > 0) {
    cli_abort(c(
      "Cube {grid_name} is missing required column(s): {paste(missing_required, collapse = ', ')}",
      "x" = "Refusing to write parquet — downstream \\
            coverage/taxonomic analyses would be silently wrong.",
      "i" = "Check the SQL API download column order for {.path {basename(cf$csv)}}."
    ))
  }
  if (length(missing_cols) > 0) {
    cli_alert_warning(
      "Missing (non-required) expected columns: {paste(missing_cols, collapse = ', ')}"
    )
  }
  if (length(extra_cols) > 0) {
    cli_alert_info("Extra columns: {paste(extra_cols, collapse = ', ')}")
  }

  # Remove empty eeacellcode rows
  n_empty <- sum(dt$eeacellcode == "" | is.na(dt$eeacellcode))
  if (n_empty > 0) {
    cli_alert_warning("Removing {scales::comma(n_empty)} rows with empty eeacellcode")
    dt <- dt[eeacellcode != "" & !is.na(eeacellcode)]
  }

  # Summary stats
  cli_alert_info("Unique species: {scales::comma(uniqueN(dt$specieskey))}")
  cli_alert_info("Unique cells: {scales::comma(uniqueN(dt$eeacellcode))}")
  cli_alert_info("Total occurrences: {scales::comma(sum(as.numeric(dt$occurrences)))}")
  cli_alert_info("Year range: {min(dt$year, na.rm = TRUE)} \u2013 {max(dt$year, na.rm = TRUE)}")

  if ("publishingorgkey" %in% names(dt))
    cli_alert_info("Unique publishers: {scales::comma(uniqueN(dt$publishingorgkey))}")
  if ("datasetkey" %in% names(dt))
    cli_alert_info("Unique datasets: {scales::comma(uniqueN(dt$datasetkey))}")

  # Basis of record breakdown
  cli_alert_info("Basis of record:")
  print(dt[, .(rows = .N, occurrences = sum(as.numeric(occurrences))),
    by = basisofrecord][order(-occurrences)])

  # Write parquet
  cli_alert_info("Writing parquet: {.path {basename(cf$parquet)}}")
  write_parquet(dt, cf$parquet)

  pq_size <- round(file.size(cf$parquet) / 1024^2, 1)
  compression <- round(csv_size / pq_size, 1)
  cli_alert_success(
    "{grid_name}: {scales::comma(nrow(dt))} rows, {pq_size} MB parquet ({compression}x compression)"
  )

  manifest[[grid_name]] <- data.frame(
    grid = grid_name,
    rows = nrow(dt),
    csv_mb = csv_size,
    parquet_mb = pq_size,
    compression = compression,
    n_species = uniqueN(dt$specieskey),
    n_cells = uniqueN(dt$eeacellcode),
    total_occ = sum(as.numeric(dt$occurrences)),
    n_dropped_empty_cell = n_empty,
    status = "converted"
  )

  rm(dt)
  invisible(gc())
}

# Fail loud if no cube parquet was produced. Otherwise an empty manifest slips
# through and only surfaces downstream as a cryptic stopifnot() in the targets
# graph (the cube_parquet target asserts the manifest exists).
n_parquet <- sum(vapply(cube_files, function(cf) file.exists(cf$parquet), logical(1)))
if (n_parquet == 0L) {
  cli_abort(c(
    "No cube parquet was produced (0 of {length(cube_files)} resolutions).",
    "x" = "No cube_*.csv found in {.path {cube_raw_dir}} and no parquet already present.",
    "i" = "Run script 01a to download the cubes, or place the CSVs in the raw cubes dir."
  ))
}
if (n_parquet < length(cube_files)) {
  cli_alert_warning(
    "Only {n_parquet}/{length(cube_files)} cube parquet(s) present \u2014 the missing resolution's analyses will be incomplete."
  )
}

# ============================================================================
# Write manifest
# ============================================================================

if (length(manifest) > 0) {
  manifest_df <- rbindlist(manifest, fill = TRUE)
  manifest_path <- here(cube_proc_dir, "cube_manifest.csv")
  fwrite(manifest_df, manifest_path)
  cli_alert_success("Manifest: {.path {manifest_path}}")
  print(manifest_df)
}

# ============================================================================
# Verify parquet files can be read
# ============================================================================

cli_h2("Verification")

for (grid_name in names(cube_files)) {
  pq <- cube_files[[grid_name]]$parquet
  if (file.exists(pq)) {
    ds <- open_dataset(pq)
    cli_alert_success("{grid_name}: {scales::comma(ds$num_rows)} rows, {length(ds$schema)} columns")
  }
}

cli_alert_success("Cube conversion complete!")
cli_alert_info("Next: source('scripts/05_validate_inputs.R')")
