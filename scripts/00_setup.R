# scripts/00_setup.R
# Run this first. It checks packages, loads config/paths, and writes a session log.

# scripts/00_setup.R

# ---- Bootstrap: make sure 'here' exists --------------------------------------
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

# It DOES NOT auto-install packages (use install_missing_packages() explicitly).

# ---- 0) Load dependency helper FIRST (requires 'here' to already be installed) ----
# If 'here' is missing on a new machine, run: install.packages("here")
source(here::here("R", "packages.R"))

# ---- 1) Check packages --------------------------------------------------------
missing <- check_packages()

if (length(missing) > 0) {
  message("\nTo install missing packages (one-time), run:\n",
          "install_missing_packages()\n\n",
          "Then record dependencies with:\n",
          "renv::snapshot()")
}

# ---- 2) Load globals/config/paths --------------------------------------------
source(here::here("R", "globals.R"))

# ---- 3) Print expected raw data locations ------------------------------------
log_msg("Raw data folders (from config.yml or defaults):")
log_msg("  GBIF cube:     ", raw_gbif_cube_dir)
log_msg("  EEA grid 10km: ", raw_grid_10km_dir)
log_msg("  EEA grid 50km: ", raw_grid_50km_dir)
log_msg("  Red List SE:   ", raw_redlist_se_dir)
log_msg("  Red List IUCN: ", raw_redlist_iucn_dir)
log_msg("  Dyntaxa:       ", raw_dyntaxa_dir)

# ---- 4) Record session info (for reproducibility) ----------------------------
session_file <- file.path(p_logs, paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(c(
  paste0("Run time: ", timestamp()),
  paste0("R version: ", R.version.string),
  "",
  "Session info:",
  capture.output(sessionInfo())
), session_file)

log_msg("Wrote session log: ", session_file)

# ---- 5) renv note -------------------------------------------------------------
if (!file.exists(here::here("renv.lock"))) {
  log_msg("NOTE: renv.lock not found. If this is a new project, run renv::init().")
} else {
  log_msg("renv.lock present. Use renv::restore() on a fresh machine.")
}

log_msg("Setup complete.")
