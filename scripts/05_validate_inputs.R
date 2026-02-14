# scripts/05_validate_inputs.R
# ============================================================================
# Phase 1 Validation: Quality Assurance Checks
# ============================================================================
# Purpose:
#   Validate all processed inputs (grids, cubes, taxa reference) and
#   write a comprehensive Markdown QA report to logs/.
#
# Inputs:
#   - data_proc/grids_10km.gpkg, grids_50km.gpkg
#   - data_proc/cube_manifest.csv, cubes/*.fst
#   - data_proc/taxa_reference_current.rds
#
# Outputs:
#   - logs/validation_report_<timestamp>.md
#
# Dependencies: scripts/00_setup.R, sf, readr, dplyr, stringr
# ============================================================================

library(here)
library(sf)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

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

#' Read a sample from a cube file (fst or rds)
#'
#' @param path Path to cube file
#' @param n    Number of rows to sample
#' @return A data.frame
read_cube_sample <- function(path, n = 5000) {
  if (!file_exists_safe(path)) {
    cli_abort("Cube file not found: {.path {path}}")
  }

  ext <- tools::file_ext(path)

  if (ext == "fst") {
    if (!requireNamespace("fst", quietly = TRUE)) {
      cli_abort("Package {.pkg fst} required for .fst files")
    }
    meta <- fst::metadata_fst(path)
    n_read <- min(n, meta$nrOfRows)
    return(fst::read_fst(path, from = 1, to = n_read))
  }

  if (ext == "rds") {
    data <- readRDS(path)
    if (inherits(data, c("data.table", "data.frame"))) {
      return(as.data.frame(head(data, n)))
    }
    cli_abort("Unknown RDS object type")
  }

  cli_abort("Unknown file extension: {ext}")
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

md_code_block <- function(text, lang = "text") {
  md_add("\n```", lang, "\n", text, "\n```\n")
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

manifest_file <- here(p_data_proc, "cube_manifest.csv")
totals_file   <- here(p_data_proc, "cube_totals_by_basisOfRecord.csv")

if (file_exists_safe(manifest_file)) {
  md_check("Cube manifest found", "ok")
  cli_alert_success("Manifest found")

  manifest <- read_csv(manifest_file, show_col_types = FALSE)

  md_add("- Manifest entries: `", nrow(manifest), "`\n")
  md_add(
    "- Full ingests: `", sum(manifest$full_ingest), "`\n"
  )

  # Check processed files exist
  if ("processed_file" %in% names(manifest)) {
    expected <- manifest |>
      filter(full_ingest, !is.na(processed_file)) |>
      pull(processed_file)

    missing_n <- sum(!file.exists(expected))

    if (missing_n == 0) {
      md_check("All expected cube files exist", "ok")
      cli_alert_success("All cube files present")
    } else {
      md_check(
        glue("{missing_n} cube files MISSING"), "fail"
      )
      cli_alert_danger("{missing_n} files missing")
    }
  }

  # Sample cube schema check
  md_h3("2.1 Column Checks (Sample Cubes)")

  sample_cubes <- manifest |>
    filter(full_ingest, !is.na(processed_file)) |>
    slice_head(n = 3)

  if (nrow(sample_cubes) > 0) {
    walk(seq_len(nrow(sample_cubes)), \(i) {
      info <- sample_cubes[i, ]

      md_add(
        "\n**Sample:** `", basename(info$processed_file),
        "`\n"
      )
      md_add("- Grid: ", info$grid, "\n")
      md_add("- Basis: ", info$basisOfRecord, "\n")

      sample_data <- tryCatch(
        read_cube_sample(info$processed_file, n = 2000),
        error = function(e) {
          md_check(
            glue("Read failed: {e$message}"), "fail"
          )
          NULL
        }
      )

      if (!is.null(sample_data)) {
        cols_lower    <- str_to_lower(names(sample_data))
        required_cols <- c(
          "specieskey", "eeacellcode",
          "yearmonth", "occurrences"
        )
        missing_cols  <- setdiff(required_cols, cols_lower)

        if (length(missing_cols) == 0) {
          md_check("All required columns present", "ok")
        } else {
          md_check(
            glue(
              "Missing: {paste(missing_cols, collapse = ', ')}"
            ),
            "warn"
          )
        }

        if ("occurrences" %in% cols_lower) {
          occ_col <- names(sample_data)[
            cols_lower == "occurrences"
          ][1]
          occ_vals <- as.numeric(sample_data[[occ_col]])

          if (all(is.finite(occ_vals), na.rm = TRUE)) {
            md_check("Occurrences column is numeric", "ok")
          }
          if (min(occ_vals, na.rm = TRUE) >= 0) {
            md_check("Occurrences >= 0", "ok")
          } else {
            md_check("NEGATIVE occurrences detected", "fail")
          }
        }
      }
    })
  } else {
    md_check("No fully ingested cubes to sample", "warn")
  }

} else {
  md_check("Cube manifest MISSING", "fail")
  cli_alert_danger("Manifest missing")
}

# Check totals file
if (file_exists_safe(totals_file)) {
  md_h3("2.2 Totals Summary")
  md_check("Cube totals file found", "ok")

  totals <- read_csv(totals_file, show_col_types = FALSE)
  md_add("- Total entries: `", nrow(totals), "`\n")

  if (all(c("grid", "basisOfRecord", "total_occurrences") %in%
          names(totals))) {
    top_5 <- totals |>
      arrange(desc(total_occurrences)) |>
      slice_head(n = 5) |>
      select(grid, basisOfRecord, total_occurrences)

    md_add("\n**Top 5 by occurrence count:**\n")
    md_code_block(
      capture.output(print(top_5, row.names = FALSE))
    )
  }
} else {
  md_check("Cube totals MISSING", "fail")
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
