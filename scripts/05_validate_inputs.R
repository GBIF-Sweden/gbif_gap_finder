# scripts/05_validate_inputs.R
# ============================================================================
# Phase 1 Validation: Quality Assurance Checks
# ============================================================================
# Purpose:
#   Validate all processed inputs (grids, cubes, taxa reference) and
#   write a comprehensive Markdown QA report to logs/.
#
# Inputs:
#   - data/{CC}/proc/grids_10km.gpkg, grids_50km.gpkg
#   - data/{CC}/proc/cubes/cube_manifest.csv, cubes/*.parquet
#   - data/{CC}/proc/taxa_reference_current.rds
#
# Outputs:
#   - logs/validation_report_<timestamp>.md
#
# Dependencies: scripts/00_setup.R, sf, readr, dplyr, stringr
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# ============================================================================
# Configuration
# ============================================================================

ts_label    <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_path <- here(p_logs, glue("validation_report_{ts_label}.md"))

# ============================================================================
# Helper Functions
# ============================================================================

#' Check if path is a file (not a directory)
file_exists_safe <- function(path) {
  file.exists(path) && !dir.exists(path)
}

#' Read RDS safely
read_rds_safe <- function(path) {
  if (!file_exists_safe(path)) {
    cli_abort("File not found: {.path {path}}")
  }
  tryCatch(
    readRDS(path),
    error = function(e) {
      cli_abort("Failed to read RDS: {e$message}")
    }
  )
}

# ============================================================================
# Markdown Report Builder
# ============================================================================

md_lines <- character()

md_add <- function(...) {
  md_lines <<- c(md_lines, paste0(..., collapse = ""))
}

md_h1 <- function(text)   md_add("\n# ", text, "\n")
md_h2 <- function(text)   md_add("\n## ", text, "\n")
md_h3 <- function(text)   md_add("\n### ", text, "\n")
md_hr <- function()        md_add("\n---\n")

md_check <- function(text,
                     status = c("ok", "warn", "fail")) {
  icon <- switch(
    match.arg(status),
    ok   = "\u2705",
    warn = "\u26a0\ufe0f",
    fail = "\u274c"
  )
  md_add("- ", icon, " ", text, "\n")
}

# ============================================================================
# Start Validation
# ============================================================================

cli_h1("Phase 1 Validation")

md_h1("Phase 1 Validation Report")
md_add("\n**Run time:** ", timestamp(), "\n")
md_add("**Project root:** `", here(), "`\n")
md_add("**Processed data:** `", p_data_proc, "`\n")

# ============================================================================
# Section 1: Grid Layers
# ============================================================================

cli_h2("Validating Grid Layers")
md_hr()
md_h2("1) Grid Layers (10km & 50km)")

grid_checks <- tibble::tribble(
  ~resolution, ~path,
  "10km",      out_grid_10km_gpkg,
  "50km",      out_grid_50km_gpkg
)

grid_results <- grid_checks |>
  mutate(
    exists    = map_lgl(path, file_exists_safe),
    grid_data = map(path, \(p) {
      if (file_exists_safe(p)) st_read(p, quiet = TRUE) else NULL
    })
  )

# Report existence
walk2(grid_results$resolution, grid_results$exists, \(res, ok) {
  if (ok) {
    md_check(glue("{res} grid file found"), "ok")
    cli_alert_success("{res} grid found")
  } else {
    md_check(glue("{res} grid file MISSING"), "fail")
    cli_alert_danger("{res} grid MISSING")
  }
})

# Detailed checks for existing grids
grid_results |>
  filter(exists) |>
  pwalk(\(resolution, path, grid_data, ...) {
    md_add("\n#### ", resolution, " grid details\n")

    epsg <- st_crs(grid_data)$epsg
    md_add("- EPSG: `", epsg, "`\n")

    if (!is.na(epsg) && epsg == CRS_ETRS89_LAEA) {
      md_check("CRS is ETRS89-LAEA (EPSG:3035)", "ok")
      cli_alert_success("{resolution}: CRS correct")
    } else {
      md_check("CRS is NOT ETRS89-LAEA", "warn")
      cli_alert_warning("{resolution}: CRS issue")
    }

    geom_types <- unique(st_geometry_type(grid_data))
    md_add(
      "- Geometry types: `",
      paste(geom_types, collapse = ", "), "`\n"
    )

    invalid_n <- sum(!st_is_valid(grid_data))
    md_add("- Invalid geometries: `", invalid_n, "`\n")

    if (invalid_n == 0) {
      md_check("All geometries valid", "ok")
      cli_alert_success("{resolution}: Geometries valid")
    } else {
      md_check(
        glue("{invalid_n} invalid geometries"), "warn"
      )
      cli_alert_warning(
        "{resolution}: {invalid_n} invalid"
      )
    }

    n_cells <- nrow(grid_data)
    md_add(
      "- Total cells: `", scales::comma(n_cells), "`\n"
    )
    cli_alert_info(
      "{resolution}: {scales::comma(n_cells)} cells"
    )

    cell_field <- guess_cellcode_field(names(grid_data))
    md_add("- Cell code field: `", cell_field, "`\n")

    if (!is.na(cell_field)) {
      n_unique <- length(unique(grid_data[[cell_field]]))
      if (n_unique == n_cells) {
        md_check("Cell codes are unique", "ok")
      } else {
        md_check("Cell codes NOT unique", "warn")
      }
    } else {
      md_check("Could not identify cell code field", "warn")
    }
  })

# ============================================================================
# Section 2: GBIF Cube Outputs
# ============================================================================

cli_h2("Validating GBIF Cube Outputs")
md_hr()
md_h2("2) GBIF Occurrence Cube Outputs")

manifest_file <- here(p_data_proc, "cubes", "cube_manifest.csv")

if (file_exists_safe(manifest_file)) {
  md_check("Cube manifest found", "ok")
  cli_alert_success("Manifest found")

  manifest <- read_csv(manifest_file, show_col_types = FALSE)

  md_add("- Manifest entries: `", nrow(manifest), "`\n")

  # Check parquet files exist
  parquet_dir <- here(p_data_proc, "cubes")
  parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(parquet_files) >= 2) {
    md_check(glue("{length(parquet_files)} parquet files found"), "ok")
    cli_alert_success("{length(parquet_files)} parquet files present")
  } else {
    md_check("Fewer than 2 parquet files found", "warn")
    cli_alert_warning("Expected 2 parquet files")
  }

  # Validate each parquet file
  md_h3("2.1 Parquet File Checks")

  if (requireNamespace("arrow", quietly = TRUE)) {
    for (pq_file in parquet_files) {
      md_add("\n**File:** `", basename(pq_file), "`\n")

      tryCatch({
        ds <- arrow::open_dataset(pq_file)
        pq_cols <- names(ds$schema)
        pq_size <- round(file.size(pq_file) / 1024^2, 1)

        md_add("- Size: `", pq_size, " MB`\n")
        md_add("- Columns: `", length(pq_cols), "`\n")
        md_add("- Column names: `", paste(pq_cols, collapse = ", "), "`\n")

        # Check required columns
        required_cols <- c("specieskey", "eeacellcode", "year", "month", "occurrences")
        missing_cols <- setdiff(required_cols, pq_cols)

        if (length(missing_cols) == 0) {
          md_check("All required columns present", "ok")
        } else {
          md_check(glue("Missing: {paste(missing_cols, collapse = ', ')}"), "warn")
        }

        # Check new columns
        new_cols <- c("publishingorgkey", "datasetkey")
        present_new <- intersect(new_cols, pq_cols)
        md_check(glue("{length(present_new)}/2 new columns present ({paste(present_new, collapse = ', ')})"), "ok")

        cli_alert_success("{basename(pq_file)}: {length(pq_cols)} columns, {pq_size} MB")
      }, error = function(e) {
        md_check(glue("Read failed: {e$message}"), "fail")
        cli_alert_danger("{basename(pq_file)}: read failed")
      })
    }
  } else {
    md_check("arrow package not available — cannot validate parquet", "warn")
  }

} else {
  md_check("Cube manifest MISSING — run script 04 first", "fail")
  cli_alert_danger("Manifest missing")
}

# ============================================================================
# Section 3: Taxa Reference
# ============================================================================

cli_h2("Validating Taxa Reference")
md_hr()
md_h2("3) Taxa Reference")

taxa_ref_path <- here(p_data_proc, "taxa_reference_current.rds")

if (file_exists_safe(taxa_ref_path)) {
  md_check("Taxa reference found", "ok")
  cli_alert_success("Taxa reference found")

  taxa_ref  <- read_rds_safe(taxa_ref_path)
  col_names <- names(taxa_ref)
  col_lower <- str_to_lower(col_names)

  md_add("- Rows: `", scales::comma(nrow(taxa_ref)), "`\n")
  md_add("- Columns: `", ncol(taxa_ref), "`\n")

  # DwC column checks
  dwc_checks <- tribble(
    ~field,                  ~variants,
    "taxonID",               c("taxonid", "taxonID"),
    "scientificName",        c("scientificname", "scientificName"),
    "taxonRank",             c("taxonrank", "taxonRank"),
    "acceptedNameUsageID",   c("acceptednameusageid",
                               "acceptedNameUsageID"),
    "threatStatus_backbone",  c("threatstatus_dyntaxa",
                               "threatStatus_backbone"),
    "threatStatus_redlist",  c("threatstatus_redlist",
                               "threatStatus_redlist")
  )

  md_add("\n#### Column Checks\n")

  walk2(dwc_checks$field, dwc_checks$variants, \(field, vars) {
    if (any(str_to_lower(vars) %in% col_lower)) {
      md_check(glue("{field} present"), "ok")
    } else {
      md_check(glue("{field} MISSING"), "warn")
    }
  })

  # Key uniqueness
  key_candidates <- c("taxonid", "taxonID", "id")
  key_field <- intersect(
    str_to_lower(key_candidates), col_lower
  )[1]

  if (!is.na(key_field)) {
    actual_key   <- col_names[col_lower == key_field][1]
    n_duplicates <- sum(duplicated(taxa_ref[[actual_key]]))

    md_add("- Key field: `", actual_key, "`\n")
    md_add("- Duplicates: `", n_duplicates, "`\n")

    if (n_duplicates == 0) {
      md_check("No duplicate keys", "ok")
    } else {
      md_check(glue("{n_duplicates} duplicate keys"), "warn")
    }
  }

  # Threat status coverage (both columns)
  for (threat_col in c("threatStatus_backbone",
                        "threatStatus_redlist")) {
    if (threat_col %in% col_names) {
      na_rate <- mean(is.na(taxa_ref[[threat_col]]))
      md_add(
        "- ", threat_col, " NA rate: `",
        round(na_rate, 3), "`\n"
      )

      if (na_rate < 0.95) {
        md_check(
          glue("{threat_col} has meaningful coverage"), "ok"
        )
      } else {
        md_check(glue("{threat_col} mostly NA"), "warn")
      }
    }
  }

} else {
  md_check("Taxa reference MISSING", "fail")
  cli_alert_danger("Taxa reference missing")
}

# ============================================================================
# Write Report
# ============================================================================

writeLines(md_lines, report_path)

cli_alert_success(
  "Validation report: {.path {report_path}}"
)
cli_alert_info("Review report for any warnings or failures")
cli_alert_info(
  "Next: source('scripts/06a_make_core_summaries.R')"
)
