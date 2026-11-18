#!/usr/bin/env Rscript
# ============================================================================
# tools/preview_gaps_filled.R - look at the Gaps filled tab before a release
# ============================================================================
# Adds the closure tables of data/{CC}/proc/closure to a copy of the bundle the
# app ships with, and starts the app from a temporary folder with the tab
# switched on. The shipped bundle (shiny_app/gap_finder/data/{CC}/shiny_data.rds)
# is only read, so the preview needs no data release and changes no tracked
# file.
#
#   Rscript tools/preview_gaps_filled.R              # opens the app in the browser
#   PORT=4000 Rscript tools/preview_gaps_filled.R
#
# Stop the app with Ctrl+C. Requires scripts/14_gap_closure.R to have run.
# ============================================================================

source(here::here("scripts", "00_setup.R"))
source(here::here("R", "closure.R"))

app_dir     <- here::here("shiny_app", "gap_finder")
bundle_path <- file.path(app_dir, "data", COUNTRY_CODE, "shiny_data.rds")
if (!file.exists(bundle_path) || file.size(bundle_path) < 1e6) {
  cli_abort(c("No app bundle at {.path {bundle_path}}",
              "i" = "A file of a few hundred bytes is a Git LFS pointer: run {.code git lfs pull}."))
}

keep_top    <- 1000L
closure_dir <- here::here(p_data_proc, "closure")
types_file  <- here::here(cfg_get("parameters.closure.source_types_file",
                                  glue("configs/dataset_types_{COUNTRY_CODE}.csv")))
cli_progress_step("Reading the closure tables in {.path {closure_dir}}")
cb <- closure_bundle(closure_dir, keep_top = keep_top,
                     datasets_meta = closure_read_dataset_meta(types_file))
if (!length(cb$pairs)) {
  cli_abort("No closure tables in {.path {closure_dir}}: run {.code scripts/14_gap_closure.R} first.")
}

cli_progress_step("Reading the app bundle {.path {bundle_path}}")
bundle <- readRDS(bundle_path)
bundle[grepl("^closure_", names(bundle))] <- NULL
for (nm in names(cb$tables)) {
  bundle[[paste0("closure_", nm)]] <- tibble::as_tibble(cb$tables[[nm]])
}
meta <- closure_bundle_meta(cb, keep_top,
                            notes = cfg_get("parameters.closure.timepoint_notes"))
bundle$metadata[names(meta)] <- meta

preview_dir <- file.path(tempdir(), "gap_finder_preview")
cli_progress_step("Writing the preview to {.path {preview_dir}}")
unlink(preview_dir, recursive = TRUE)
dir.create(file.path(preview_dir, "data", COUNTRY_CODE), recursive = TRUE)
file.copy(file.path(app_dir, "app.R"), preview_dir)
file.copy(file.path(app_dir, "www"), preview_dir, recursive = TRUE)
# Fast gzip: some 200 MB on disk, against about 2 GB uncompressed.
con <- gzfile(file.path(preview_dir, "data", COUNTRY_CODE, "shiny_data.rds"), "wb",
              compression = 1)
saveRDS(bundle, con)
close(con)
rm(bundle)
cli_progress_done()

Sys.setenv(GAP_FINDER_SHOW_GAPS_FILLED = "true", GBIF_GAP_COUNTRY = COUNTRY_CODE)
cli_alert_success("{length(cb$pairs)} pair{?s}: {.val {cb$pairs}}. Open the {.strong Gaps filled} tab; stop with Ctrl+C.")
shiny::runApp(preview_dir, port = as.integer(Sys.getenv("PORT", "3939")),
              launch.browser = TRUE)
