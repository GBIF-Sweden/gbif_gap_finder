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

# CRITICAL failures accumulate here; the script writes the full report and then
# aborts at the end if any are present, so bad inputs stop the pipeline instead
# of flowing silently into wrong coverage/taxonomic numbers.
critical_failures <- character(0)

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

        # Check required columns (basisofrecord + kingdom drive the by-basis and
        # taxonomic analyses; treat any missing required column as CRITICAL).
        required_cols <- c("specieskey", "eeacellcode", "year", "month", "occurrences",
                           "basisofrecord", "kingdom")
        missing_cols <- setdiff(required_cols, pq_cols)

        if (length(missing_cols) == 0) {
          md_check("All required columns present", "ok")
        } else {
          md_check(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"), "fail")
          critical_failures <<- c(critical_failures,
            glue("{basename(pq_file)}: missing required columns \\
                 {paste(missing_cols, collapse = ', ')}"))
        }

        # Check publisher/dataset dimension columns
        new_cols <- c("publishingorgkey", "datasetkey")
        present_new <- intersect(new_cols, pq_cols)
        md_check(glue("{length(present_new)}/2 publisher/dataset columns present \\
                      ({paste(present_new, collapse = ', ')})"), "ok")

        # b-cubed standard measures (b3verse schema migration). Informational,
        # not required: a pre-migration cube simply won't carry them.
        bcubed_cols    <- c("mincoordinateuncertaintyinmeters",
                            "mintemporaluncertainty", "distinctobservers")
        present_bcubed <- intersect(bcubed_cols, pq_cols)
        md_check(glue("{length(present_bcubed)}/3 b-cubed measures present \\
                      ({if (length(present_bcubed)) paste(present_bcubed, collapse = ', ') else 'none'})"),
                 if (length(present_bcubed) == 3) "ok" else "warn")

        # Freshness guard: 04 is existence-gated, so a re-downloaded raw cube can
        # be NEWER than this parquet — downstream would then read the PREVIOUS
        # download (fresh raw + stale processed). Treat parquet-older-than-CSV as
        # CRITICAL so a stale processed layer can never reach the analyses.
        raw_csv <- here(raw_gbif_cube_dir, switch(
          basename(pq_file),
          "cube_10km.parquet" = cfg_get("files.cubes.grid10km", "cube_10km.csv"),
          "cube_50km.parquet" = cfg_get("files.cubes.grid50km", "cube_50km.csv"),
          sub("\\.parquet$", ".csv", basename(pq_file))))
        if (file_exists_safe(raw_csv) && file.mtime(raw_csv) > file.mtime(pq_file)) {
          md_check(glue("STALE: raw `{basename(raw_csv)}` is NEWER than the parquet \\
                        — delete the parquet and re-run script 04"), "fail")
          critical_failures <<- c(critical_failures, glue(
            "{basename(pq_file)}: parquet older than {basename(raw_csv)} — downstream \\
             is reading the previous download; delete parquet + re-run 04"))
        } else if (file_exists_safe(raw_csv)) {
          md_check("Parquet is at least as new as its raw CSV", "ok")
        }

        # specieskey format sanity. A COL key is alphanumeric (e.g. 6VFN8) OR
        # numeric — numeric COL ids are VALID (e.g. 67343 = Anemone nemorosa). A
        # LEGACY GBIF Backbone nub key is a long integer (>=7 digits). Flag only
        # when such keys are a meaningful share (pre-COL / wrong-backbone download);
        # short numeric COL ids are never flagged.
        sk <- tryCatch(
          as.character(dplyr::collect(dplyr::distinct(dplyr::select(ds, specieskey)))$specieskey),
          error = function(e) character(0))
        sk <- sk[!is.na(sk) & nzchar(sk)]
        if (length(sk)) {
          legacy_like <- grepl("^[0-9]{7,}$", sk)
          frac <- mean(legacy_like)
          md_add("- specieskey: `", scales::comma(length(sk)), "` distinct; `",
                 scales::comma(sum(legacy_like)), "` legacy-style (>=7-digit integer) = `",
                 sprintf("%.2f%%", 100 * frac), "`\n")
          if (frac > 0.02) {
            md_check(glue("{sprintf('%.1f%%', 100 * frac)} of specieskeys look like legacy \\
                          Backbone nub keys (>=7 digits) — likely a pre-COL download"), "fail")
            critical_failures <<- c(critical_failures, glue(
              "{basename(pq_file)}: {sprintf('%.1f%%', 100 * frac)} legacy-style specieskeys \\
               (>=7-digit integer) — likely a pre-COL / wrong-backbone download"))
          } else {
            md_check("specieskey is COL-format (no legacy Backbone nub keys)", "ok")
          }

          # Early warning (NOT fatal): a valid COL universe is overwhelmingly
          # alphanumeric with only a small numeric-COL minority (~0.4%). If the
          # numeric fraction jumps well above that, specieskey may be reverting to
          # an integer backbone -- including SHORT integers the >=7-digit legacy
          # check above cannot see. WARN so a backbone/download change is caught
          # early (a handful of numeric COL ids stays well under the threshold).
          numeric_like <- grepl("^[0-9]+$", sk)
          num_frac     <- mean(numeric_like)
          md_add("- specieskey numeric-key fraction: `",
                 sprintf("%.2f%%", 100 * num_frac), "` (valid COL baseline ~0.4%)\n")
          if (num_frac > 0.05) {
            md_check(glue("{sprintf('%.1f%%', 100 * num_frac)} of specieskeys are purely \\
                          numeric (valid COL baseline ~0.4%) -- specieskey may be reverting \\
                          to an integer backbone; verify the download's backbone"), "warn")
            cli_alert_warning("{sprintf('%.1f%%', 100 * num_frac)} numeric specieskeys \\
                              (COL baseline ~0.4%) -- possible integer-backbone reversion")
          }
        }

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
# Section 2b: Cube cells ⊆ Grid (orphan check)
# ============================================================================
# complete_to_grid() joins cube counts onto the GRID universe with all.x = TRUE,
# so any cube cell absent from the grid is silently dropped and its occurrences
# vanish from coverage and totals. Assert cube cell codes are a subset of the
# grid (a resolution mismatch or a marine/border cell surfaces here).

md_hr(); md_h2("2b) Cube ↔ Grid Alignment")

check_cube_in_grid <- function(pq_file, grid_gpkg, label) {
  if (!file_exists_safe(pq_file) || !file_exists_safe(grid_gpkg)) {
    md_check(glue("{label}: parquet or grid missing — skipped"), "warn")
    return(invisible(NULL))
  }
  grid       <- sf::st_read(grid_gpkg, quiet = TRUE)
  gcol       <- guess_cellcode_field(names(grid))
  grid_codes <- unique(as.character(grid[[gcol]]))
  cube_codes <- unique(as.character(
    dplyr::collect(dplyr::select(arrow::open_dataset(pq_file), eeacellcode))$eeacellcode))
  orphans    <- setdiff(cube_codes, grid_codes)
  md_add("- ", label, ": `", scales::comma(length(cube_codes)), "` cube cells, `",
         scales::comma(length(orphans)), "` not in grid\n")
  if (length(orphans) == 0) {
    md_check(glue("{label}: all cube cells present in the grid"), "ok")
    return(invisible(NULL))
  }

  # A few edge/border cells outside the land-clipped grid are expected data drift
  # in a fresh GBIF pull (e.g. a coastal occurrence). Report exactly which cells
  # and how many occurrences get dropped (never silent), and only FAIL the build
  # when the mismatch is large enough to signal a real problem (wrong grid or
  # resolution), not a handful of edge cells. Tunable tolerance:
  orphan_tol <- 10L
  orphan_occ <- sum(as.numeric(dplyr::collect(dplyr::summarise(
    dplyr::filter(dplyr::select(arrow::open_dataset(pq_file), eeacellcode, occurrences),
                  eeacellcode %in% orphans),
    occ = sum(occurrences, na.rm = TRUE)))$occ))
  show_codes <- paste(utils::head(sort(orphans), 5), collapse = ", ")
  md_add("- ", label, ": dropping `", scales::comma(orphan_occ), "` occurrence(s) in `",
         scales::comma(length(orphans)), "` non-grid cell(s): ", show_codes,
         if (length(orphans) > 5) ", …" else "", "\n")
  if (length(orphans) <= orphan_tol) {
    md_check(glue("{label}: {length(orphans)} edge cell(s) outside the grid, \\
                  {scales::comma(orphan_occ)} occ dropped — within tolerance ({orphan_tol})"), "warn")
    cli_alert_warning("{label}: {length(orphans)} non-grid cell(s), \\
                      {scales::comma(orphan_occ)} occ dropped (within tolerance)")
  } else {
    md_check(glue("{label}: {scales::comma(length(orphans))} cube cell(s) absent from grid \\
                  — {scales::comma(orphan_occ)} occ dropped (exceeds tolerance {orphan_tol})"), "fail")
    critical_failures <<- c(critical_failures,
      glue("{label}: {length(orphans)} cube cell(s) not in the grid (> tolerance {orphan_tol})"))
  }
}

check_cube_in_grid(here(p_data_proc, "cubes", "cube_10km.parquet"), out_grid_10km_gpkg, "10km")
check_cube_in_grid(here(p_data_proc, "cubes", "cube_50km.parquet"), out_grid_50km_gpkg, "50km")

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

if (length(critical_failures) > 0) {
  cli_alert_danger("Validation found {length(critical_failures)} CRITICAL failure(s):")
  for (f in critical_failures) cli_alert_warning(f)
  cli_abort("Input validation failed — see {.path {report_path}}. Fix inputs before continuing.")
}

cli_alert_info("Review report for any warnings or failures")
cli_alert_info(
  "Next: source('scripts/06a_make_core_summaries.R')"
)
