# run.R
# ============================================================================
# Helper Script for Running the GBIF Gap Finder Pipeline
# ============================================================================
# Source this file for convenient pipeline commands.
# Works with both targets-based and script-based workflows.
#
# Usage:
#   source("run.R")
#   status()         # Check pipeline progress
#   run_all()        # Full pipeline
#   launch_gap_finder() # Start the Gap Finder app

library(targets)
library(here)
library(cli)

# ============================================================================
# Banner
# ============================================================================

cli_h1("GBIF Gap Finder")

cli_h2("Common commands")
cli_dl(c(
  "tar_make()"       = "Run the full pipeline",
  "tar_visnetwork()" = "Visualise the pipeline graph",
  "tar_progress()"   = "Check pipeline status",
  "tar_read(name)"   = "Read a cached result"
))

cli_h2("Phase-specific runs")
cli_dl(c(
  "run_download()"        = "Download raw data (taxonomy, red list, invasives, ...)",
  "run_resolve_sources()" = "Resolve + validate data-source DOIs",
  "run_phase_1()"         = "Ingestion (grids, taxonomy, cubes)",
  "run_phase_2()"         = "Validation",
  "run_phase_3()"         = "Derived summaries",
  "run_phase_4()"         = "Gap analysis (spatial, temporal, taxonomic)",
  "run_phase_5()"         = "Integrated overview",
  "run_gap_finder_prep()" = "Prepare data for the Gap Finder app",
  "run_reports()"         = "Render all R Markdown reports",
  "run_reconcile()"       = "Check headline numbers agree across layers (script 12)",
  "run_metrics()"         = "Refresh the metrics.md current-figures snapshot (script 13)"
))

cli_h2("Shiny apps")
cli_dl(c(
  "launch_gap_finder()" = "Launch the Gap Finder app"
))

# ============================================================================
# Convenience Functions
# ============================================================================

#' Download raw data (script 01a — taxonomy, red list, invasives, sensitive, admin)
run_download <- function() {
  tar_make(names = raw_data)
}

#' Resolve + validate data-source DOIs/citations from keys (script 01b)
run_resolve_sources <- function() {
  tar_make(names = data_sources_meta)
}

#' Run Phase 1: Data Ingestion (scripts 01a, 01b, 02-04)
run_phase_1 <- function() {
  tar_make(names = c(raw_data, data_sources_meta, grids, taxa_reference, cube_parquet))
}

#' Run Phase 2: Validation (script 05)
run_phase_2 <- function() {
  tar_make(names = validation_report)
}

#' Run Phase 3: Derived Summaries (scripts 06a, 06b)
run_phase_3 <- function() {
  tar_make(names = c(core_summaries, species_summaries))
}

#' Run Phase 4: Gap Analysis (scripts 07, 08, 09a, 09b, 09c)
run_phase_4 <- function() {
  tar_make(names = c(spatial_gaps, temporal_gaps, reconcile_taxonomy,
                     taxonomic_gaps, scope_summaries))
}

#' Run Phase 5: Integrated Overview (script 10)
run_phase_5 <- function() {
  tar_make(names = gap_overview)
}

#' Prepare data for the Gap Finder app (script 11)
run_gap_finder_prep <- function() {
  tar_make(names = gap_finder_data)
}

#' Render all R Markdown reports
run_reports <- function() {
  tar_make(names = c(
    report_overview,
    report_spatial_gaps,
    report_temporal_gaps,
    report_record_types,
    report_taxonomic_gaps,
    report_species_of_concern,
    report_publishers,
    report_priorities
  ))
}

#' Run the cross-layer reconciliation guardrail (script 12).
#'
#' Not part of `tar_make()`. Run after a full build to assert the headline
#' numbers agree across Overview / Taxonomic / Concern; it calls `stop()` on any
#' disagreement.
run_reconcile <- function() {
  source(here("scripts", "12_reconcile.R"))
}

#' Refresh the docs/metrics.md "Current snapshot" block from the latest outputs (script 13).
#'
#' Rewrites only the auto-generated block between the METRICS_SNAPSHOT markers in
#' docs/metrics.md; safe any time after a build. Also runs as the `metrics_snapshot`
#' target in tar_make().
run_metrics <- function() {
  source(here("scripts", "13_metrics_snapshot.R"))
}

#' Locate the prepared Gap Finder bundle (per-country data/<CC>/, or legacy flat).
gap_finder_data_path <- function() {
  hits <- Sys.glob(here("shiny_app", "gap_finder", "data", "*", "shiny_data.rds"))
  if (length(hits) >= 1) return(hits[1])
  here("shiny_app", "gap_finder", "data", "shiny_data.rds")  # legacy fallback
}

#' Launch the Gap Finder app
launch_gap_finder <- function() {
  gap_data_path <- gap_finder_data_path()
  if (!file.exists(gap_data_path)) {
    cli_alert_warning("Gap app data not found. Preparing first...")
    run_gap_finder_prep()
  }
  cli_alert_info("Launching Gap Finder app...")
  shiny::runApp(here("shiny_app", "gap_finder"))
}

#' Run the full pipeline (core only; reports are separate)
run_all <- function() {
  tar_make()
  cli_alert_success("Core pipeline complete!")
  cli_alert_info("To render reports, run {.code run_reports()}")
}

#' Check pipeline status
status <- function() {
  source(here("scripts", "00_setup.R"))

  cli_h2("Pipeline status")
  print(tar_progress())

  # Output summary -----------------------------------------------------------
  outputs <- character(0)

  manifest_path <- here(p_data_proc, "cubes", "cube_manifest.csv")
  if (file.exists(manifest_path)) {
    manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
    outputs["Parquet cubes"] <- as.character(nrow(manifest))
  }

  gaps_dir <- here(p_data_proc, "gaps")
  if (dir.exists(gaps_dir)) {
    gap_files <- list.files(gaps_dir, pattern = "\\.csv$")
    outputs["Gap analysis files"] <- as.character(length(gap_files))
  }

  tables_dir <- here(p_output, "tables")
  if (dir.exists(tables_dir)) {
    table_files <- list.files(tables_dir, pattern = "\\.csv$", recursive = TRUE)
    outputs["Overview tables"] <- as.character(length(table_files))
  }

  cli_h2("Output summary")
  if (length(outputs) > 0) cli_dl(outputs) else cli_alert_info("No outputs found yet.")

  # Data sources -------------------------------------------------------------
  taxa_path <- here(p_data_proc, "taxa_reference_current.rds")

  invasive_files <- if (dir.exists(raw_invasives_dir)) {
    list.files(raw_invasives_dir, pattern = "\\.(txt|csv)$")
  } else character(0)

  sensitive_files <- if (dir.exists(raw_sensitive_dir)) {
    list.files(raw_sensitive_dir, pattern = "\\.(txt|csv)$")
  } else character(0)

  gap_finder_path <- gap_finder_data_path()

  cli_h2("Data sources")
  cli_dl(c(
    "Taxonomy backbone"     = if (file.exists(taxa_path)) "yes" else "no",
    "Invasive species"      = if (length(invasive_files) > 0) {
      sprintf("yes (%d files)", length(invasive_files))
    } else "no",
    "Sensitive species"     = if (length(sensitive_files) > 0) {
      sprintf("yes (%d files)", length(sensitive_files))
    } else "no",
    "Gap Finder data ready" = if (file.exists(gap_finder_path)) "yes" else "no"
  ))
}

#' Destroy all cached targets and rebuild from scratch
rebuild <- function() {
  cli_alert_warning("This will delete all cached targets and rebuild.")
  if (interactive() && tolower(readline("Continue? (y/n): ")) == "y") {
    tar_destroy()
    tar_make()
  } else {
    cli_alert_info("Cancelled.")
  }
}

cli_alert_info("Run {.code status()} to check progress, or {.code tar_make()} to run.")
