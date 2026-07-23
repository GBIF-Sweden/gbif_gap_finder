# scripts/09c_scope_summaries.R
# ==============================================================================
# Scope-Filtered Summaries + Recent-Period Layer
# ==============================================================================
# Purpose:
#   One place for every computation that needs both (a) the parquet cube
#   and (b) the taxonomic reconciliation / recent-period cutoff.
#
#   Produces taxonomy-scoped variants of all core summaries using the 4-tier
#   matching from 09a, derives the recent-period cutoff from the data, and
#   writes all the spatial/temporal/recency slices the app needs.
#
#   Script 11 becomes a pure loader after this runs.
#
# Scopes produced (as suffixes):
#   - _all         All GBIF species (for the "All GBIF" scope toggle)
#   - _dyntaxa     Species matched to the national taxonomy backbone
#   - _threatened  Species on the Red List (CR/EN/VU/NT)
#   - _invasive    Species flagged as invasive
#   - _sensitive   Species flagged as sensitive
#
# Prerequisites:
#   - 04_convert_cubes_parquet.R (parquet cubes)
#   - 06a_make_core_summaries.R (core summaries)
#   - 09a_reconcile_taxonomy.R (reconciliation table)
#   - 02_ingest_grids.R (grid geometries for zero-filling)
#
# Inputs:
#   - data/{CC}/proc/taxonomic_reconciliation.rds  (from 09a)
#   - data/{CC}/proc/cubes/*.parquet               (from 04)
#   - data/{CC}/proc/grids_*.gpkg                   (from 02, optional)
#
# Outputs (in data/{CC}/proc/derived/):
#
#   Per-scope core summaries (each x 10km/50km):
#     - cell_summary_<scope>_<grid>.csv
#     - time_summary_<scope>_<grid>.csv
#     - cell_time_summary_<scope>_<grid>.csv
#     - order_cell_summary_<scope>_<grid>.csv
#     - order_time_summary_<scope>_<grid>.csv
#     - family_time_summary_<scope>_<grid>.csv
#     - species_time_summary_all_<grid>.csv      (all-scope only)
#
#   Per-scope recent-period layer:
#     - cell_recency_<scope>_<grid>.csv         cell x max yearmonth x staleness
#     - basis_recent_<scope>_<grid>.csv         basis x last-12-months observed
#     - spatial_gaps_<scope>_<grid>.csv         cell x basis x has_data (zero-filled)
#     - cell_last_year_<scope>_<grid>.csv       cell x last-12-months flags
#
#   Not scope-filtered (single version):
#     - tax_cell_recency_<grid>.csv             kingdom x class x cell recency
#     - recent_cutoff.rds                       cutoff_ym + recent_label constants
#     - species_scope_summary.csv                per-species scope flags
#
# Dependencies: scripts/00_setup.R, data.table, arrow, sf, cli, glue, lubridate
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# Script-specific package
library(arrow)

# %||% is defined in R/globals.R

timer_start <- Sys.time()

cli_h1("09c -- Scope-Filtered Summaries & Recent-Period Layer")

# ==============================================================================
# Unclassified-rank bucketing (taxonomy)
# ==============================================================================
# Maps NA/"" ranks to an explicit "Unclassified" bucket so cube-derived
# order/class/family summaries never silently drop occurrences with a blank
# rank. Level-agnostic; extend RANK_COLS_CANON to cover new ranks everywhere.
# (Promote to R/globals.R to share one copy across all scripts + the app.)
if (!exists("RANK_COLS_CANON")) {
  RANK_COLS_CANON <- c("kingdom", "phylum", "class", "order",
                       "superfamily", "family", "subfamily", "tribe", "genus")
}
if (!exists("bucket_unclassified")) {
  bucket_unclassified <- function(df, cols = RANK_COLS_CANON, label = "Unclassified") {
    cols <- intersect(cols, names(df))
    if (length(cols) == 0) return(df)
    if (data.table::is.data.table(df)) {
      for (col in cols) {
        v <- as.character(df[[col]])
        v[is.na(v) | trimws(v) == ""] <- label
        data.table::set(df, j = col, value = v)
      }
    } else {
      for (col in cols) {
        v <- as.character(df[[col]])
        v[is.na(v) | trimws(v) == ""] <- label
        df[[col]] <- v
      }
    }
    df
  }
}

# ==============================================================================
# Configuration
# ==============================================================================

# Directories created by ensure_dirs() in 00_setup.R

exclude_orders <- cfg_get("parameters.taxonomic.exclude_orders", character(0))
THREATENED_CODES <- c("CR", "EN", "VU", "NT")

SCOPE_FLAGS <- c(
  all        = "is_all",
  threatened = "is_threatened",
  invasive   = "is_invasive",
  sensitive  = "is_sensitive"
)
# Note: the "dyntaxa" scope was removed. With the app's scope toggle gone, the
# occurrence-based tabs (Spatial / Temporal / Record Types / Publisher) read the
# full-GBIF outputs (07 / 06a / the "all" scope here), and only the Taxonomic and
# Concern tabs are reference-relative — and those run off the backbone *match*
# (09a reconciliation + 09b match_summary), not a scope-filtered cube summary.
# The in_dyntaxa flag is still computed in the scope lookup below (it is cheap and
# documents the backbone membership), it just no longer drives a scope output.


# ==============================================================================
# Step 1: Load Reconciliation & Build Scope Lookup
# ==============================================================================

cli_h2("Loading Reconciliation Table")

recon_path <- here(p_data_proc, "taxonomic_reconciliation.rds")
if (!file.exists(recon_path)) {
  cli_abort(c(
    "Reconciliation table not found: {.path {recon_path}}",
    "i" = "Run script 09a_reconcile_taxonomy.R first"
  ))
}

recon <- as.data.table(readRDS(recon_path))
cli_alert_success("Loaded reconciliation: {scales::comma(nrow(recon))} species")

# Resolve threat status to a single column (first non-NA per row)
recon <- resolve_threat_status(recon,
  c("threatStatus_redlist", "threatStatus_backbone", "threatStatus"))

# Build scope lookup: specieskey -> boolean flags
scope_lookup <- recon[, .(specieskey)]
scope_lookup[, is_all := TRUE]

if ("in_dyntaxa" %in% names(recon)) {
  scope_lookup[, in_dyntaxa := recon$in_dyntaxa]
} else {
  scope_lookup[, in_dyntaxa := recon$match_tier != "unmatched"]
}
scope_lookup[is.na(in_dyntaxa), in_dyntaxa := FALSE]

if ("is_invasive" %in% names(recon)) {
  scope_lookup[, is_invasive := recon$is_invasive]
} else {
  scope_lookup[, is_invasive := FALSE]
}
scope_lookup[is.na(is_invasive), is_invasive := FALSE]

if ("is_sensitive" %in% names(recon)) {
  scope_lookup[, is_sensitive := recon$is_sensitive]
} else {
  scope_lookup[, is_sensitive := FALSE]
}
scope_lookup[is.na(is_sensitive), is_sensitive := FALSE]

scope_lookup[, is_threatened := !is.na(recon$threatStatus) &
  recon$threatStatus %in% THREATENED_CODES]

# Apply order exclusions (e.g. Primates)
if (length(exclude_orders) > 0 && "order" %in% names(recon)) {
  excluded_keys <- recon[order %in% exclude_orders, unique(specieskey)]
  if (length(excluded_keys) > 0) {
    scope_lookup[specieskey %in% excluded_keys, `:=`(
      in_dyntaxa = FALSE,
      is_threatened = FALSE,
      is_invasive = FALSE,
      is_sensitive = FALSE
    )]
    cli_alert_info(
      "Excluded {length(excluded_keys)} species \\
       from orders: {paste(exclude_orders, collapse = ', ')}"
    )
  }
}

scope_lookup <- unique(scope_lookup, by = "specieskey")

# Report per-scope counts
cli_alert_info("Scope lookup: {scales::comma(nrow(scope_lookup))} species")
for (scope_name in names(SCOPE_FLAGS)) {
  flag <- SCOPE_FLAGS[[scope_name]]
  n <- sum(scope_lookup[[flag]])
  cli_alert_info("  {scope_name}: {scales::comma(n)}")
}

# Determine active scopes (skip ones with 0 species)
active_scopes <- names(SCOPE_FLAGS)
for (scope_name in names(SCOPE_FLAGS)) {
  flag <- SCOPE_FLAGS[[scope_name]]
  if (sum(scope_lookup[[flag]]) == 0) {
    cli_alert_warning("Scope '{scope_name}' has 0 species -- skipping")
    active_scopes <- setdiff(active_scopes, scope_name)
  }
}

if (length(active_scopes) == 0) {
  cli_abort("No scopes have any species. Check reconciliation output.")
}
cli_alert_success("Active scopes: {paste(active_scopes, collapse = ', ')}")


# ==============================================================================
# Step 2: Locate Parquet Cubes
# ==============================================================================

cli_h2("Locating Parquet Cubes")

parquet_files <- list.files(p_cubes, pattern = "\\.parquet$", full.names = TRUE)
if (length(parquet_files) == 0) cli_abort("No parquet files in: {.path {p_cubes}}")

grid_map <- list()
for (pf in parquet_files) {
  bn <- basename(pf)
  if (grepl("10km", bn)) grid_map[["10km"]] <- pf
  else if (grepl("50km", bn)) grid_map[["50km"]] <- pf
}
cli_alert_info(
  "Found {length(grid_map)} parquet cube(s): {paste(names(grid_map), collapse = ', ')}"
)


# ==============================================================================
# Step 3: Derive Recent-Period Cutoff (from 10km cube)
# ==============================================================================
# This is computed once and saved as a pipeline constant. Everything downstream
# (09c outputs, script 11, the app) reads recent_cutoff.rds.

cli_h2("Deriving Recent-Period Cutoff")

if (!"10km" %in% names(grid_map)) {
  cli_abort("10km parquet cube is required to derive recent-period cutoff")
}

pf_10km <- grid_map[["10km"]]
ym_probe <- as.data.table(
  arrow::open_dataset(pf_10km) |>
    dplyr::select(year, month) |>
    dplyr::collect()
)
ym_probe[, yearmonth := fifelse(
  !is.na(year) & !is.na(month),
  as.integer(year) * 100L + as.integer(month),
  NA_integer_
)]

# Observed-time cutoff: 12 months ending at the latest observation yearmonth.
all_ym <- sort(unique(ym_probe$yearmonth[!is.na(ym_probe$yearmonth)]))
if (length(all_ym) > 0) {
  latest_ym <- max(all_ym)
  latest_year <- as.integer(substr(as.character(latest_ym), 1, 4))
  latest_month <- as.integer(substr(as.character(latest_ym), 5, 6))
  cutoff_date <- as.Date(paste0(latest_year, "-", sprintf("%02d", latest_month), "-01")) %m-%
    months(11)
  recent_cutoff_ym <- as.integer(format(cutoff_date, "%Y%m"))
  recent_label <- paste0(
    format(cutoff_date, "%b %Y"), " \u2013 ",
    format(as.Date(paste0(latest_year, "-", sprintf("%02d", latest_month), "-01")), "%b %Y")
  )
} else {
  recent_cutoff_ym <- as.integer(paste0(year(Sys.Date()) - 1, "01"))
  recent_label <- paste0(year(Sys.Date()) - 1)
  latest_ym <- NA_integer_
  cli_alert_warning("Could not determine recent period from data, defaulting to {recent_label}")
}

cli_alert_success("Recent period: {recent_label} (cutoff: {recent_cutoff_ym})")

recent_cutoff <- list(
  cutoff_ym   = recent_cutoff_ym,
  latest_ym   = latest_ym,
  label       = recent_label,
  computed_at = Sys.time()
)
saveRDS(recent_cutoff, here(p_data_proc, "recent_cutoff.rds"))
cli_alert_success("Saved: recent_cutoff.rds")
rm(ym_probe, all_ym); gc()


# ==============================================================================
# Step 4: Load Grid Cell Lists (for zero-filling spatial gaps)
# ==============================================================================

cli_h2("Loading Grid Cell Lists")

grid_cells <- list()
for (grid_label in names(grid_map)) {
  gpkg_path <- here(p_data_proc, glue("grids_{grid_label}.gpkg"))
  if (file.exists(gpkg_path)) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      cli_alert_warning("Package 'sf' not installed -- spatial zero-fill will use cube cells only")
      grid_cells[[grid_label]] <- NULL
    } else {
      grid <- sf::st_read(gpkg_path, quiet = TRUE)
      code_field <- guess_cellcode_field(names(grid))
      if (!is.na(code_field)) {
        grid_cells[[grid_label]] <- as.character(grid[[code_field]])
        cli_alert_success("{grid_label}: {scales::comma(length(grid_cells[[grid_label]]))} cells")
      } else {
        cli_alert_warning("{grid_label}: no cell code column found")
        grid_cells[[grid_label]] <- NULL
      }
      rm(grid); gc()
    }
  } else {
    cli_alert_warning("{grid_label} grid not found -- spatial zero-fill will use cube cells only")
    grid_cells[[grid_label]] <- NULL
  }
}


# ==============================================================================
# Helper: Load a cube with scope flags joined
# ==============================================================================

load_cube_scoped <- function(parquet_path, grid_label) {
  # Use the consolidated read_cube from globals.R
  dt <- read_cube(parquet_path,
    cols = c("specieskey", "species", "basisofrecord",
      "eeacellcode", "year", "month",
      "kingdom", "phylum", "class", "order", "family", "occurrences"),
    grid_label = grid_label,
    recode_taxonomy = TRUE)

  dt[, occ_num := as.numeric(occurrences)]

  # Join scope flags
  dt <- merge(dt, scope_lookup, by = "specieskey", all.x = TRUE)
  for (flag in SCOPE_FLAGS) {
    dt[is.na(get(flag)), (flag) := FALSE]
  }

  # The "all" scope is genuinely ALL of GBIF (project decision 2026-07-21): every
  # occurrence, including taxa outside the backbone/reconciliation set. Only the
  # backbone-relative scopes (threatened/invasive/sensitive) stay match-driven.
  # Without this override, "all" was keyed to `recon` via scope_lookup and
  # silently dropped cube species that never entered reconciliation (e.g.
  # microbial kingdoms), so the app's Overview/Spatial/Record-Types totals did
  # not match the full-GBIF dashboard figures.
  dt[, is_all := TRUE]

  dt
}


# ==============================================================================
# Step 5 & 6: Per-Grid Processing (Core Summaries + Recent-Period Layer)
# ==============================================================================

cli_h2("Processing Each Grid Resolution")

for (grid_label in names(grid_map)) {
  pf <- grid_map[[grid_label]]
  cli_h3("Loading {grid_label} cube")

  cube <- load_cube_scoped(pf, grid_label)
  cli_alert_info("Loaded {scales::comma(nrow(cube))} rows")

  # --------------------------------------------------------------------------
  # Per-scope core summaries
  # --------------------------------------------------------------------------

  for (scope_name in active_scopes) {
    flag <- SCOPE_FLAGS[[scope_name]]
    dt_s <- cube[get(flag) == TRUE]
    n_s <- nrow(dt_s)
    if (n_s == 0) next

    cli_alert_info("[{scope_name}] {scales::comma(n_s)} rows -- writing summaries")

    # Cell summary
    cell_dt <- dt_s[, .(occurrences = safe_sum(occ_num), n_species = uniqueN(specieskey)),
      by = .(grid, basisofrecord, eeacellcode)]
    cell_all <- dt_s[, .(occurrences = safe_sum(occ_num), n_species = uniqueN(specieskey)),
      by = .(grid, eeacellcode)]
    cell_all[, basisofrecord := "all"]
    cell_result <- rbindlist(list(cell_dt, cell_all), use.names = TRUE, fill = TRUE)
    fwrite(cell_result, here(p_derived, glue("cell_summary_{scope_name}_{grid_label}.csv")))
    rm(cell_dt, cell_all, cell_result)

    # Time summary
    time_dt <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
      n_species = uniqueN(specieskey), n_cells = uniqueN(eeacellcode)),
      by = .(grid, basisofrecord, yearmonth)]
    time_all <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
      n_species = uniqueN(specieskey), n_cells = uniqueN(eeacellcode)),
      by = .(grid, yearmonth)]
    time_all[, basisofrecord := "all"]
    time_result <- rbindlist(list(time_dt, time_all), use.names = TRUE, fill = TRUE)
    time_result[, `:=`(
      year = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )]
    fwrite(time_result, here(p_derived, glue("time_summary_{scope_name}_{grid_label}.csv")))
    rm(time_dt, time_all, time_result)

    # Cell x Time
    ct_dt <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
      n_species = uniqueN(specieskey)),
      by = .(grid, basisofrecord, eeacellcode, yearmonth)]
    ct_all <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
      n_species = uniqueN(specieskey)),
      by = .(grid, eeacellcode, yearmonth)]
    ct_all[, basisofrecord := "all"]
    ct_result <- rbindlist(list(ct_dt, ct_all), use.names = TRUE, fill = TRUE)
    ct_result[, `:=`(
      year = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )]
    fwrite(ct_result, here(p_derived, glue("cell_time_summary_{scope_name}_{grid_label}.csv")))
    rm(ct_dt, ct_all, ct_result)

    # Order x Cell
    if ("order" %in% names(dt_s)) {
      oc_dt <- dt_s[, .(occurrences = safe_sum(occ_num), n_species = uniqueN(specieskey),
        n_families = uniqueN(family)), by = .(grid, basisofrecord, order, eeacellcode)]
      oc_all <- dt_s[, .(occurrences = safe_sum(occ_num), n_species = uniqueN(specieskey),
        n_families = uniqueN(family)), by = .(grid, order, eeacellcode)]
      oc_all[, basisofrecord := "all"]
      oc_result <- rbindlist(list(oc_dt, oc_all), use.names = TRUE, fill = TRUE)
      oc_result <- bucket_unclassified(oc_result)
      fwrite(oc_result, here(p_derived, glue("order_cell_summary_{scope_name}_{grid_label}.csv")))
      rm(oc_dt, oc_all, oc_result)
    }

    # Order x Time
    if ("order" %in% names(dt_s)) {
      ot_dt <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
        n_species = uniqueN(specieskey), n_cells = uniqueN(eeacellcode)),
        by = .(grid, basisofrecord, order, yearmonth)]
      ot_all <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
        n_species = uniqueN(specieskey), n_cells = uniqueN(eeacellcode)),
        by = .(grid, order, yearmonth)]
      ot_all[, basisofrecord := "all"]
      ot_result <- rbindlist(list(ot_dt, ot_all), use.names = TRUE, fill = TRUE)
      ot_result[, `:=`(
        year = as.integer(substr(as.character(yearmonth), 1, 4)),
        month = as.integer(substr(as.character(yearmonth), 5, 6))
      )]
      ot_result <- bucket_unclassified(ot_result)
      fwrite(ot_result, here(p_derived, glue("order_time_summary_{scope_name}_{grid_label}.csv")))
      rm(ot_dt, ot_all, ot_result)
    }

    # Family x Time
    if (all(c("order", "family") %in% names(dt_s))) {
      ft_dt <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
        n_species = uniqueN(specieskey), n_cells = uniqueN(eeacellcode)),
        by = .(grid, basisofrecord, order, family, yearmonth)]
      ft_all <- dt_s[!is.na(yearmonth), .(occurrences = safe_sum(occ_num),
        n_species = uniqueN(specieskey), n_cells = uniqueN(eeacellcode)),
        by = .(grid, order, family, yearmonth)]
      ft_all[, basisofrecord := "all"]
      ft_result <- rbindlist(list(ft_dt, ft_all), use.names = TRUE, fill = TRUE)
      ft_result[, `:=`(
        year = as.integer(substr(as.character(yearmonth), 1, 4)),
        month = as.integer(substr(as.character(yearmonth), 5, 6))
      )]
      ft_result <- bucket_unclassified(ft_result)
      fwrite(ft_result, here(p_derived, glue("family_time_summary_{scope_name}_{grid_label}.csv")))
      rm(ft_dt, ft_all, ft_result)
    }

    # Species-level observed time — all-scope only, for Taxonomic tab's
    # Observed radio under establishmentMeans sub-scopes. App filters by
    # specieskey from scoped_match_summary, then aggregates up to
    # class/order/family.
    if (scope_name == "all" && "specieskey" %in% names(dt_s) &&
        all(c("order", "family") %in% names(dt_s))) {
      tax_cols_obs <- intersect(c("kingdom", "phylum", "class", "order", "family"),
                                names(dt_s))
      st_species <- dt_s[!is.na(yearmonth),
        .(occurrences = safe_sum(occ_num)),
        by = c("grid", "specieskey", tax_cols_obs, "yearmonth")]
      st_species[, `:=`(
        year = as.integer(substr(as.character(yearmonth), 1, 4)),
        month = as.integer(substr(as.character(yearmonth), 5, 6))
      )]
      if (nrow(st_species) > 0) {
        fwrite(st_species, here(p_derived,
          glue("species_time_summary_{scope_name}_{grid_label}.csv")))
        cli_alert_info(glue(
          "species_time_summary ({scope_name}): ",
          "{scales::comma(nrow(st_species))} rows"
        ))
      }
      rm(st_species)
    }
  }

  # --------------------------------------------------------------------------
  # Per-scope recent-period layer
  # --------------------------------------------------------------------------

  cli_alert_info("Recent-period layer for {grid_label}")

  all_cells_this_grid <- grid_cells[[grid_label]]
  # No fall-back to cube cells. complete_to_grid() (the spatial layer below)
  # aborts loudly if this is NULL/empty rather than silently zero-filling to
  # data-only cells, which would report ~100% coverage and hide every empty
  # cell. A genuinely missing grid should stop the run, not produce a
  # misleading spatial_gaps.

  global_max_ym <- max(cube$yearmonth, na.rm = TRUE)
  data_max_date <- if (!is.na(global_max_ym) && !is.infinite(global_max_ym)) {
    as.Date(paste0(
      substr(as.character(global_max_ym), 1, 4), "-",
      substr(as.character(global_max_ym), 5, 6), "-01"
    ))
  } else as.Date(NA)
  # Staleness reference = cube snapshot date (01b), consistent with script 08.
  # Falls back to the data's own max month if the download metadata is absent.
  latest_date_grid <- get_snapshot_date(fallback = data_max_date)

  for (scope_name in active_scopes) {
    flag <- SCOPE_FLAGS[[scope_name]]
    dt_s <- cube[get(flag) == TRUE]
    if (nrow(dt_s) == 0) next

    # Cell recency
    cr <- dt_s[, .(
      max_yearmonth = if (all(is.na(yearmonth))) NA_integer_ else max(yearmonth, na.rm = TRUE),
      total_occ = safe_sum(occ_num)
    ), by = .(eeacellcode)]
    if (!is.na(latest_date_grid)) {
      cr[, staleness_months := {
        cell_d <- as.Date(paste0(
          substr(as.character(max_yearmonth), 1, 4), "-",
          substr(as.character(max_yearmonth), 5, 6), "-01"
        ))
        as.numeric(difftime(latest_date_grid, cell_d, units = "days")) / 30.44
      }]
      cr[is.na(max_yearmonth), staleness_months := NA_real_]
    } else {
      cr[, staleness_months := NA_real_]
    }
    cr[, basisofrecord := "all"]
    fwrite(cr, here(p_derived, glue("cell_recency_{scope_name}_{grid_label}.csv")))
    rm(cr)

    # Basis recent (observed last-12-months split)
    br <- dt_s[, .(
      occ_total     = safe_sum(occ_num),
      occ_last_year = safe_sum(occ_num[!is.na(yearmonth) & yearmonth >= recent_cutoff_ym]),
      occ_prior     = safe_sum(occ_num[is.na(yearmonth) | yearmonth < recent_cutoff_ym]),
      n_species     = uniqueN(specieskey)
    ), by = .(basisofrecord)]

    br_all <- dt_s[, .(
      basisofrecord = "all",
      occ_total     = safe_sum(occ_num),
      occ_last_year = safe_sum(occ_num[!is.na(yearmonth) & yearmonth >= recent_cutoff_ym]),
      occ_prior     = safe_sum(occ_num[is.na(yearmonth) | yearmonth < recent_cutoff_ym]),
      n_species     = uniqueN(specieskey)
    )]

    basis_recent <- rbindlist(list(br, br_all), use.names = TRUE)
    fwrite(basis_recent, here(p_derived, glue("basis_recent_{scope_name}_{grid_label}.csv")))
    rm(br, br_all, basis_recent)

    # Spatial gaps (zero-filled to the FULL grid via the shared helper).
    # Per-basis counts plus a synthetic "all" basis row, then complete_to_grid()
    # expands to every grid cell (aborting if the grid is missing) and adds the
    # has_data / gap_zero flags. Identical code path to 07, so the two zero-fills
    # cannot drift.
    sg_basis_raw <- dt_s[, .(
      occurrences = safe_sum(occ_num), n_species = uniqueN(specieskey)
    ), by = .(eeacellcode, basisofrecord)]

    sg_all_raw <- dt_s[, .(
      occurrences = safe_sum(occ_num), n_species = uniqueN(specieskey)
    ), by = .(eeacellcode)][, basisofrecord := "all"]

    sg_counts <- rbindlist(list(sg_basis_raw, sg_all_raw), use.names = TRUE)

    spatial_gaps <- complete_to_grid(
      sg_counts, all_cells_this_grid, facet_cols = "basisofrecord"
    )
    fwrite(spatial_gaps, here(p_derived, glue("spatial_gaps_{scope_name}_{grid_label}.csv")))
    rm(sg_basis_raw, sg_all_raw, sg_counts, spatial_gaps)

    # Cell last year (per-cell observed last-12-months split)
    cly <- dt_s[, .(
      occ_last_year = safe_sum(occ_num[!is.na(yearmonth) & yearmonth >= recent_cutoff_ym]),
      occ_prior     = safe_sum(occ_num[is.na(yearmonth) | yearmonth < recent_cutoff_ym])
    ), by = .(eeacellcode)]
    cly[, `:=`(
      newly_covered = occ_prior == 0 & occ_last_year > 0,
      has_last_year_data = occ_last_year > 0
    )]
    fwrite(cly, here(p_derived, glue("cell_last_year_{scope_name}_{grid_label}.csv")))
    rm(cly)
  }

  # --------------------------------------------------------------------------
  # Tax cell recency (kingdom x class x cell) -- not scope-filtered
  # --------------------------------------------------------------------------

  if (all(c("kingdom", "class") %in% names(cube))) {
    cli_alert_info("Tax cell recency (kingdom x class)")
    tcr <- cube[!is.na(kingdom) & kingdom != "", .(
      total_occ     = safe_sum(occ_num),
      max_year      = if (all(is.na(year))) NA_real_ else max(as.numeric(year), na.rm = TRUE),
      max_yearmonth = if (all(is.na(yearmonth))) NA_integer_ else max(yearmonth, na.rm = TRUE)
    ), by = .(eeacellcode, kingdom, class)]

    if (!is.na(latest_date_grid)) {
      tcr[, staleness_months := {
        cell_d <- as.Date(paste0(
          substr(as.character(max_yearmonth), 1, 4), "-",
          substr(as.character(max_yearmonth), 5, 6), "-01"
        ))
        as.numeric(difftime(latest_date_grid, cell_d, units = "days")) / 30.44
      }]
      tcr[is.na(max_yearmonth), staleness_months := NA_real_]
    } else {
      tcr[, staleness_months := NA_real_]
    }

    tcr <- bucket_unclassified(tcr)
    fwrite(tcr, here(p_derived, glue("tax_cell_recency_{grid_label}.csv")))
    cli_alert_success("tax_cell_recency_{grid_label}: {scales::comma(nrow(tcr))} rows")
    rm(tcr)
  }

  cli_alert_success("Done: {grid_label}")
  rm(cube); gc()
}


# ==============================================================================
# Step 7: Species Scope Summary
# ==============================================================================

cli_h2("Writing Species Scope Summary")

species_scope_cols <- intersect(
  c("specieskey", "species", "kingdom", "phylum", "class", "order", "family",
    "match_tier", "establishmentMeans", "occurrenceStatus",
    "threatStatus", "threatStatus_redlist", "threatStatus_backbone"),
  names(recon)
)

species_scope <- recon[, ..species_scope_cols]
species_scope <- merge(species_scope, scope_lookup, by = "specieskey", all.x = TRUE)

for (flag in SCOPE_FLAGS) {
  if (flag %in% names(species_scope)) {
    species_scope[is.na(get(flag)), (flag) := FALSE]
  } else {
    species_scope[, (flag) := FALSE]
  }
}

# Bucket blank ranks so the app's Dyntaxa-mode class/kingdom dropdowns (which
# filter this table on !is.na(class)) keep an explicit "Unclassified" option
# instead of dropping reptiles and other blank-rank taxa.
species_scope <- bucket_unclassified(species_scope)

fwrite(species_scope, here(p_derived, "species_scope_summary.csv"))
cli_alert_success("species_scope_summary.csv: {scales::comma(nrow(species_scope))} species")


# ==============================================================================
# Summary
# ==============================================================================

cli_h1("Summary (Script 09c)")

output_files <- list.files(p_derived, pattern = "\\.csv$")

for (scope_name in active_scopes) {
  pattern <- paste0("_", scope_name, "_")
  scope_files <- grep(pattern, output_files, value = TRUE)
  cli_alert_info("  {scope_name}: {length(scope_files)} files")
}

for (summary_type in c("cell_summary", "time_summary", "cell_time_summary",
                       "order_cell_summary", "order_time_summary", "family_time_summary",
                       "species_time_summary",
                       "cell_recency", "basis_recent",
                       "spatial_gaps", "cell_last_year", "tax_cell_recency")) {
  n <- sum(grepl(paste0("^", summary_type, "_"), output_files))
  if (n > 0) cli_alert_info("  {summary_type}: {n} files")
}

elapsed <- round(difftime(Sys.time(), timer_start, units = "mins"), 1)
cli_alert_info("Elapsed: {elapsed} minutes")
cli_alert_success("Scope-filtered summaries + recent-period layer complete!")
cli_alert_info("Next: source('scripts/10_make_gap_overview.R')")
