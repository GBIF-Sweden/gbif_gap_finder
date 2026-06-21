# scripts/11_prepare_gap_finder_data.R
# ==============================================================================
# Prepare Data for the Gap Finder App
# ==============================================================================
# This script assembles the Shiny data bundle by LOADING pre-computed outputs
# from the pipeline. It performs no heavy computation — everything cube-based
# and scope-filtered is done by 09c, and all tabular summaries come from
# 07/08/09b/10.
#
# Output: shiny_app/gap_finder/data/shiny_data.rds
#
# The bundle contains:
#   - Grid geometries (simplified for web rendering)
#   - Dashboard summary metrics
#   - Per-scope summaries (all, dyntaxa, threatened, invasive, sensitive)
#   - Taxonomic coverage tables (derived from 09b's match_summary)
#   - Publisher data (from 06a)
#   - Priority lists (from 10)
#   - Order/family temporal trends (from 10)
#   - Troudet-style bias data (derived from match_summary + order_temporal)
#   - Metadata
#
# Inputs:
#   - data/{CC}/proc/grids_*.gpkg                     (from 02)
#   - data/{CC}/proc/derived/*.csv                    (from 06a, 09c)
#   - data/{CC}/proc/recent_cutoff.rds                 (from 09c)
#   - data/{CC}/proc/derived/species_scope_summary.csv  (from 09c)
#   - data/{CC}/proc/gaps/*.csv                       (from 07, 08, 09b)
#   - data/{CC}/output/tables/*.csv                   (from 10)
#   - data/{CC}/output/tables/integrated/*.csv        (from 10)
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

cli_h1("Preparing Data for Shiny App (Script 11)")

# ==============================================================================
# Configuration
# ==============================================================================

shiny_output_dir <- here("shiny_app", "gap_finder", "data")
if (!dir.exists(shiny_output_dir)) dir.create(shiny_output_dir, recursive = TRUE)
shiny_data_path <- here(shiny_output_dir, "shiny_data.rds")

shiny_data <- list()

# Resolved data-source provenance (cube + checklist DOIs/citations, contributing
# datasets) produced by 01b_resolve_data_sources.R. Baked into the bundle so the
# app's Data & sources tab can credit sources offline.
ds_meta_path <- here(p_data_proc, "data_sources_meta.rds")
data_sources_meta <- if (file.exists(ds_meta_path)) {
  readRDS(ds_meta_path)
} else {
  cli_alert_warning("data_sources_meta.rds not found — run 01b first; sources will be unattributed.")
  NULL
}

# safe_read() and %||% are defined in R/globals.R

# Add year/month columns from yearmonth if they don't already exist
add_yearmonth_cols <- function(df) {
  if (is.null(df)) return(NULL)
  df <- as_tibble(df)
  if (!"yearmonth" %in% names(df)) return(df)
  # 09c already adds year/month — only compute if missing
  if (all(c("year", "month") %in% names(df))) return(df)
  df |>
    mutate(
      yearmonth = as.integer(gsub("-", "", as.character(yearmonth))),
      year = as.integer(substr(as.character(yearmonth), 1, 4)),
      month = as.integer(substr(as.character(yearmonth), 5, 6))
    )
}


# ==============================================================================
# 1. Grid Geometries
# ==============================================================================

cli_h2("Loading Grid Geometries")

for (grid_label in c("10km", "50km")) {
  gpkg_path <- here(p_data_proc, paste0("grids_", grid_label, ".gpkg"))
  if (!file.exists(gpkg_path)) {
    cli_alert_warning("{grid_label} grid not found")
    next
  }

  grid <- st_read(gpkg_path, quiet = TRUE)
  cellcode_field <- guess_cellcode_field(names(grid))
  if (!is.na(cellcode_field)) {
    grid$eeacellcode <- as.character(grid[[cellcode_field]])
  }

  tol <- if (grid_label == "10km") 500 else 1000
  grid_simple <- grid |>
    st_simplify(dTolerance = tol) |>
    select(eeacellcode) |>
    st_transform(4326)

  shiny_data[[paste0("grid_", grid_label)]] <- grid_simple
  cli_alert_success("{grid_label} grid: {nrow(grid_simple)} cells (simplified)")
  rm(grid, grid_simple); invisible(gc())
}

# Administrative boundaries
admin_dir <- here(raw_admin_dir)
for (lvl in c(1, 2)) {
  admin_path <- here(admin_dir, paste0("admin_level", lvl, ".gpkg"))
  if (file.exists(admin_path)) {
    admin_sf <- tryCatch(st_read(admin_path, quiet = TRUE), error = function(e) NULL)
    if (!is.null(admin_sf)) {
      if (st_crs(admin_sf)$epsg != 4326) admin_sf <- st_transform(admin_sf, 4326)
      shiny_data[[paste0("admin_level", lvl)]] <- admin_sf
      cli_alert_success("Admin level {lvl}: {nrow(admin_sf)} units")
    }
  }
}

# Cell -> admin lookup
if (!is.null(shiny_data$grid_10km) && !is.null(shiny_data$admin_level1)) {
  cli_alert_info("Computing cell -> admin region mapping...")
  grid_centroids <- suppressWarnings(
    st_centroid(shiny_data$grid_10km, of_largest_polygon = TRUE)
  )
  cell_admin <- st_join(grid_centroids, shiny_data$admin_level1, join = st_within) |>
    st_drop_geometry() |>
    as_tibble() |>
    select(eeacellcode, admin_name_level1 = admin_name) |>
    distinct(eeacellcode, .keep_all = TRUE)

  if (!is.null(shiny_data$admin_level2)) {
    cell_admin2 <- st_join(grid_centroids, shiny_data$admin_level2, join = st_within) |>
      st_drop_geometry() |>
      as_tibble() |>
      select(eeacellcode, admin_name_level2 = admin_name) |>
      distinct(eeacellcode, .keep_all = TRUE)
    cell_admin <- cell_admin |> left_join(cell_admin2, by = "eeacellcode")
  }

  shiny_data$cell_admin_lookup <- cell_admin
  cli_alert_success("Cell-admin lookup: {nrow(cell_admin)} cells mapped")
  rm(grid_centroids); invisible(gc())
}


# ==============================================================================
# 2. Recent-Period Cutoff (from 09c)
# ==============================================================================

cli_h2("Loading Recent-Period Cutoff")

recent_cutoff <- safe_read(here(p_data_proc, "recent_cutoff.rds"), type = "rds")
if (!is.null(recent_cutoff)) {
  shiny_data$last_year    <- recent_cutoff$cutoff_ym
  shiny_data$recent_label <- recent_cutoff$label
  cli_alert_success("Recent period: {recent_cutoff$label} (cutoff: {recent_cutoff$cutoff_ym})")
} else {
  cli_alert_warning("recent_cutoff.rds not found -- run 09c first. Using default.")
  shiny_data$last_year    <- as.integer(paste0(year(Sys.Date()) - 1, "01"))
  shiny_data$recent_label <- as.character(year(Sys.Date()) - 1)
}

recent_cutoff_ym <- shiny_data$last_year
recent_label     <- shiny_data$recent_label


# ==============================================================================
# 3. Dashboard Summary
# ==============================================================================

cli_h2("Loading Dashboard Summary")

dashboard <- safe_read(here(p_tables, "dashboard_summary.csv"))
if (!is.null(dashboard)) shiny_data$dashboard <- as_tibble(dashboard)

dashboard_long <- safe_read(here(p_tables, "dashboard_summary_long.csv"))
if (!is.null(dashboard_long)) shiny_data$dashboard_long <- as_tibble(dashboard_long)


# ==============================================================================
# 4. Per-Scope Summaries (from 09c)
# ==============================================================================
# 09c produces scope-suffixed variants of all core summaries.
# We load them under scope-prefixed names so the app can pick the right
# slice based on the user's toggle (dyntaxa / all / threatened / invasive / sensitive).

cli_h2("Loading Per-Scope Summaries (from 09c)")

SCOPES <- c("all", "dyntaxa", "threatened", "invasive", "sensitive")
GRID <- "10km"

load_scope_file <- function(summary_type, scope, grid = GRID) {
  path <- here(p_derived, glue("{summary_type}_{scope}_{grid}.csv"))
  safe_read(path)
}

for (scope in SCOPES) {
  # Time summary
  ts <- load_scope_file("time_summary", scope)
  if (!is.null(ts)) {
    ts <- add_yearmonth_cols(ts)
    shiny_data[[paste0(scope, "_time_summary")]] <- ts
    cli_alert_success("{scope} time_summary: {scales::comma(nrow(ts))} rows")
  }

  # Order time
  ots <- load_scope_file("order_time_summary", scope)
  if (!is.null(ots)) {
    shiny_data[[paste0(scope, "_order_time_summary")]] <- add_yearmonth_cols(ots)
  }

  # Family time
  fts <- load_scope_file("family_time_summary", scope)
  if (!is.null(fts)) {
    shiny_data[[paste0(scope, "_family_time_summary")]] <- add_yearmonth_cols(fts)
  }

  # Species-level observed time (only produced by 09c for "all" scope).
  # Used by the app's Taxonomic tab under establishmentMeans sub-scopes.
  if (scope == "all") {
    sts <- load_scope_file("species_time_summary", scope)
    if (!is.null(sts)) {
      sts <- add_yearmonth_cols(sts)
      shiny_data[[paste0(scope, "_species_time_summary")]] <- sts
      cli_alert_success("{scope} species_time_summary: {scales::comma(nrow(sts))} rows")
    }
  }

  # Cell summary
  cs <- load_scope_file("cell_summary", scope)
  if (!is.null(cs)) {
    shiny_data[[paste0(scope, "_cell_summary")]] <- as_tibble(cs)
  }

  # Cell recency
  cr <- load_scope_file("cell_recency", scope)
  if (!is.null(cr)) {
    shiny_data[[paste0(scope, "_cell_recency")]] <- as_tibble(cr)
  }

  # Basis recent (last-12-months splits)
  br <- load_scope_file("basis_recent", scope)
  if (!is.null(br)) {
    shiny_data[[paste0(scope, "_basis_recent")]] <- as_tibble(br)
  }

  # Spatial gaps (zero-filled)
  sg <- load_scope_file("spatial_gaps", scope)
  if (!is.null(sg)) {
    shiny_data[[paste0(scope, "_spatial_gaps")]] <- as_tibble(sg)
    cli_alert_success("{scope} spatial_gaps: {scales::comma(nrow(sg))} rows")
  }

  # Cell last year
  # 09c produces columns: occ_last_year, occ_prior, newly_covered,
  # has_last_year_data. The app reads `prior` and `last_year` (unprefixed)
  # from the "all"-scope alias shiny_data$cell_last_year. Rename here so
  # downstream code works without changes.
  cly <- load_scope_file("cell_last_year", scope)
  if (!is.null(cly)) {
    cly_tb <- as_tibble(cly) |>
      mutate(
        prior = occ_prior,
        last_year = occ_last_year
      )
    shiny_data[[paste0(scope, "_cell_last_year")]] <- cly_tb
  }

  # Cell time summary
  cts <- load_scope_file("cell_time_summary", scope)
  if (!is.null(cts)) {
    shiny_data[[paste0(scope, "_cell_time_summary")]] <- add_yearmonth_cols(cts)
  }
}

# Backward-compatible aliases: the app currently expects certain names
# without scope prefix (e.g. time_summary_10km, spatial_gaps_10km). Point
# those to the "all" scope so existing app code that doesn't know about
# scope toggling still works.
alias_map <- list(
  time_summary_10km     = "all_time_summary",
  cell_summary_10km     = "all_cell_summary",
  cell_recency_10km     = "all_cell_recency",
  order_time_summary    = "all_order_time_summary",
  family_time_summary   = "all_family_time_summary",
  spatial_gaps_10km     = "all_spatial_gaps",
  cell_last_year        = "all_cell_last_year"
)
for (alias in names(alias_map)) {
  src <- alias_map[[alias]]
  if (!is.null(shiny_data[[src]])) shiny_data[[alias]] <- shiny_data[[src]]
}


# ==============================================================================
# 5. Tax Cell Recency (from 09c)
# ==============================================================================

cli_h2("Loading Tax Cell Recency")

tcr <- safe_read(here(p_derived, paste0("tax_cell_recency_", GRID, ".csv")))
if (!is.null(tcr)) {
  shiny_data$tax_cell_recency <- as_tibble(tcr)
  cli_alert_success("Tax cell recency: {scales::comma(nrow(tcr))} rows")

  # Kingdom-only aggregate (for backward compat)
  shiny_data$kingdom_cell_recency <- shiny_data$tax_cell_recency |>
    group_by(eeacellcode, kingdom) |>
    summarise(
      total_occ = sum(total_occ, na.rm = TRUE),
      max_year = if (all(is.na(max_year))) NA_real_ else max(max_year, na.rm = TRUE),
      max_yearmonth = if (all(is.na(max_yearmonth))) NA_real_ else max(max_yearmonth, na.rm = TRUE),
      staleness_months = if (all(is.na(staleness_months))) NA_real_ else min(staleness_months, na.rm = TRUE),
      .groups = "drop"
    )
  cli_alert_success("Kingdom cell recency: {scales::comma(nrow(shiny_data$kingdom_cell_recency))} rows")
}


# ==============================================================================
# 6. Species Scope Lookup (from 09c)
# ==============================================================================

cli_h2("Loading Species Scope Lookup")

ssl_path <- here(p_derived, "species_scope_summary.csv")
if (file.exists(ssl_path)) {
  shiny_data$species_scope_lookup <- as_tibble(fread(ssl_path))
  n <- nrow(shiny_data$species_scope_lookup)
  cli_alert_success("Species scope lookup: {scales::comma(n)} species")
  cli_alert_info("  in_dyntaxa:    {scales::comma(sum(shiny_data$species_scope_lookup$in_dyntaxa, na.rm = TRUE))}")
  cli_alert_info("  is_threatened: {scales::comma(sum(shiny_data$species_scope_lookup$is_threatened, na.rm = TRUE))}")
  cli_alert_info("  is_invasive:   {scales::comma(sum(shiny_data$species_scope_lookup$is_invasive, na.rm = TRUE))}")
  cli_alert_info("  is_sensitive:  {scales::comma(sum(shiny_data$species_scope_lookup$is_sensitive, na.rm = TRUE))}")
} else {
  cli_alert_warning("species_scope_summary.csv not found -- run 09c")
}


# ==============================================================================
# 7. Taxonomic Coverage Tables (derived from 09b's match_summary)
# ==============================================================================

cli_h2("Loading Taxonomic Coverage Tables")

match_summary <- safe_read(here(p_gaps, "taxonomic_match_summary.csv"))
if (!is.null(match_summary)) {
  shiny_data$taxonomic_match_summary <- as_tibble(match_summary)
  cli_alert_success("Match summary: {scales::comma(nrow(match_summary))} taxa")

  threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"),
                          names(match_summary))[1]

  # Coverage by threat status
  if (!is.na(threat_col)) {
    shiny_data$tax_by_threat <- match_summary |> as_tibble() |>
      mutate(threatStatus = .data[[threat_col]]) |>
      filter(!is.na(threatStatus), threatStatus != "",
             threatStatus %in% c("CR", "EN", "VU", "NT", "LC", "DD")) |>
      group_by(threatStatus) |>
      summarise(
        n_ref_total = n(),
        n_in_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = n_ref_total - n_in_gbif,
        pct_coverage = round(100 * n_in_gbif / n_ref_total, 1),
        .groups = "drop"
      )
    cli_alert_success("tax_by_threat: {nrow(shiny_data$tax_by_threat)} categories")

    # Missing threatened (CR/EN/VU/NT) for the Priorities tab
    shiny_data$priority_taxa_missing <- match_summary |> as_tibble() |>
      mutate(threatStatus = .data[[threat_col]]) |>
      filter(threatStatus %in% c("CR", "EN", "VU", "NT"), !matched_any) |>
      select(any_of(c("scientificName", "taxonRank", "threatStatus",
                       "kingdom", "phylum", "class", "order", "family")))
    cli_alert_success("priority_taxa_missing: {nrow(shiny_data$priority_taxa_missing)}")
  }

  # Hierarchy-level coverage (kingdom / phylum / class / order / family)
  grp_fn <- function(df, grp_cols) {
    df |> as_tibble() |>
      filter(if_all(all_of(grp_cols), ~ !is.na(.x) & .x != "")) |>
      group_by(across(all_of(grp_cols))) |>
      summarise(
        n_taxa = n(),
        n_in_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = n_taxa - n_in_gbif,
        pct_coverage = round(100 * n_in_gbif / n_taxa, 1),
        .groups = "drop"
      ) |>
      arrange(desc(n_taxa))
  }

  if ("kingdom" %in% names(match_summary)) {
    shiny_data$tax_by_kingdom <- grp_fn(match_summary, "kingdom")
    cli_alert_success("tax_by_kingdom: {nrow(shiny_data$tax_by_kingdom)} kingdoms")
  }
  if (all(c("kingdom", "phylum") %in% names(match_summary))) {
    shiny_data$tax_by_phylum <- grp_fn(match_summary, c("kingdom", "phylum"))
  }
  if (all(c("kingdom", "phylum", "class") %in% names(match_summary))) {
    shiny_data$tax_by_class <- grp_fn(match_summary, c("kingdom", "phylum", "class"))
  }
  if (all(c("kingdom", "phylum", "class", "order") %in% names(match_summary))) {
    shiny_data$tax_by_order <- grp_fn(match_summary, c("kingdom", "phylum", "class", "order"))
    cli_alert_success("tax_by_order: {nrow(shiny_data$tax_by_order)} orders")
  }
  if (all(c("kingdom", "phylum", "class", "order", "family") %in% names(match_summary))) {
    shiny_data$tax_by_family <- grp_fn(match_summary, c("kingdom", "phylum", "class", "order", "family"))
    cli_alert_success("tax_by_family: {nrow(shiny_data$tax_by_family)} families")
  }

  # Scope-filter tables (for the scope dropdown)
  if ("establishmentMeans" %in% names(match_summary)) {
    shiny_data$tax_by_establishment <- match_summary |> as_tibble() |>
      group_by(establishmentMeans) |>
      summarise(
        n_ref_total = n(),
        n_in_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = n_ref_total - n_in_gbif,
        pct_coverage = round(100 * n_in_gbif / n_ref_total, 2),
        .groups = "drop"
      ) |> arrange(desc(n_ref_total))
  }
  if ("occurrenceStatus" %in% names(match_summary)) {
    shiny_data$tax_by_occurrence_status <- match_summary |> as_tibble() |>
      group_by(occurrenceStatus) |>
      summarise(
        n_ref_total = n(),
        n_in_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = n_ref_total - n_in_gbif,
        pct_coverage = round(100 * n_in_gbif / n_ref_total, 2),
        .groups = "drop"
      ) |> arrange(desc(n_ref_total))
  }
  if ("is_invasive" %in% names(match_summary)) {
    shiny_data$tax_by_invasive <- match_summary |> as_tibble() |>
      group_by(is_invasive) |>
      summarise(
        n_ref_total = n(),
        n_in_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = n_ref_total - n_in_gbif,
        pct_coverage = round(100 * n_in_gbif / n_ref_total, 2),
        .groups = "drop"
      )
  }
}

# Pre-computed tax_by_rank from script 10
tax_by_rank <- safe_read(here(p_tables, "overview_taxonomic_by_rank.csv"))
if (!is.null(tax_by_rank)) shiny_data$tax_by_rank <- as_tibble(tax_by_rank)


# ==============================================================================
# 8. Spatial Overviews (from 10)
# ==============================================================================

cli_h2("Loading Spatial Overviews")

spatial_files <- list(
  spatial_overview  = "overview_spatial_by_basis.csv",
  spatial_by_grid   = "overview_spatial_by_grid.csv",
  comparison_grids  = "comparison_grid_resolutions.csv"
)
for (key in names(spatial_files)) {
  df <- safe_read(here(p_tables, spatial_files[[key]]))
  if (!is.null(df)) shiny_data[[key]] <- as_tibble(df)
}

# Priority cells (zero / low / stale) from 10
priority_files <- list(
  list(primary = "priority_cells_zero_coverage.csv", alt = "priority_zero_coverage_cells.csv", key = "priority_zero_cells"),
  list(primary = "priority_cells_low_coverage.csv",  alt = NULL,                                 key = "priority_low_cells"),
  list(primary = "priority_cells_stale.csv",          alt = "priority_stale_cells.csv",          key = "priority_stale_cells")
)
for (spec in priority_files) {
  df <- safe_read(here(p_integrated, spec$primary))
  if ((is.null(df) || nrow(df) == 0) && !is.null(spec$alt)) {
    df <- safe_read(here(p_integrated, spec$alt))
  }
  if (!is.null(df) && nrow(df) > 0) {
    shiny_data[[spec$key]] <- as_tibble(df)
    cli_alert_success("{spec$key}: {scales::comma(nrow(df))}")
  }
}


# ==============================================================================
# 9. Temporal Overviews (from 10)
# ==============================================================================

cli_h2("Loading Temporal Overviews")

temporal_files <- list(
  temporal_year    = "overview_temporal_year.csv",
  temporal_month   = "overview_temporal_month_seasonal.csv",
  temporal_decade  = "overview_temporal_decade.csv",
  temporal_heatmap = "overview_temporal_heatmap_10km.csv"
)
for (key in names(temporal_files)) {
  df <- safe_read(here(p_tables, temporal_files[[key]]))
  if (!is.null(df)) shiny_data[[key]] <- as_tibble(df)
}


# ==============================================================================
# 10. Order / Family Temporal Trends (from 10)
# ==============================================================================

cli_h2("Loading Order/Family Temporal Trends")

order_temporal <- safe_read(here(p_integrated, "order_temporal_trends.csv"))
if (!is.null(order_temporal)) {
  shiny_data$order_temporal <- as_tibble(order_temporal)
  cli_alert_success("Order temporal trends loaded")

  current_year <- year(Sys.Date())

  # 5-year bins
  shiny_data$order_5yr <- shiny_data$order_temporal |>
    filter(year >= 1970, year <= current_year) |>
    mutate(
      period_start = floor(year / 5) * 5,
      period = paste0(period_start, "-", period_start + 4)
    ) |>
    group_by(grid, order, period, period_start) |>
    summarise(
      occurrences = sum(total_occurrences, na.rm = TRUE),
      n_cells = sum(n_cells, na.rm = TRUE),
      .groups = "drop"
    )

  # Top 25 orders
  shiny_data$top_orders <- shiny_data$order_temporal |>
    group_by(order) |>
    summarise(total = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |> slice_head(n = 25)

  # Recent vs historical change for top 12
  recent_cutoff_year <- current_year - 10
  historical_cutoff <- current_year - 20
  shiny_data$order_change <- shiny_data$order_temporal |>
    filter(order %in% shiny_data$top_orders$order[1:12]) |>
    mutate(era = case_when(
      year >= recent_cutoff_year ~ "Recent",
      year >= historical_cutoff ~ "Historical",
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(era)) |>
    group_by(order, era) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occurrences, values_fill = 0) |>
    filter(Historical > 0) |>
    mutate(
      pct_change = round(100 * (Recent - Historical) / Historical, 1),
      direction = ifelse(pct_change >= 0, "Increased", "Decreased")
    ) |>
    arrange(desc(pct_change))
}

family_summary <- safe_read(here(p_integrated, "family_summary.csv"))
if (!is.null(family_summary)) shiny_data$family_summary <- as_tibble(family_summary)


# ==============================================================================
# 11. Priority Lists (from 10)
# ==============================================================================

priority_summary <- safe_read(here(p_tables, "priority_summary.csv"))
if (!is.null(priority_summary)) shiny_data$priority_summary <- as_tibble(priority_summary)

if (is.null(shiny_data$priority_taxa_missing)) {
  priority_taxa_all <- safe_read(here(p_integrated, "priority_taxa_all.csv"))
  if (!is.null(priority_taxa_all)) shiny_data$priority_taxa_all <- as_tibble(priority_taxa_all)
}


# ==============================================================================
# 12. Overview Last-Year Stats
# ==============================================================================
# Derived from already-loaded time_summary + cell_last_year (both from 09c).

cli_h2("Computing Overview Last-Year Stats")

overview_ts <- shiny_data$all_time_summary %||% shiny_data$dyntaxa_time_summary
overview_cly <- shiny_data$all_cell_last_year %||% shiny_data$dyntaxa_cell_last_year

if (!is.null(overview_ts)) {
  ts_all <- overview_ts |> filter(basisofrecord == "all")

  shiny_data$yearly_totals <- ts_all |>
    group_by(year) |>
    summarise(
      total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
      n_cells = sum(as.numeric(n_cells), na.rm = TRUE),
      .groups = "drop"
    )

  recent_occ <- ts_all |>
    filter(yearmonth >= recent_cutoff_ym) |>
    summarise(
      total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
      n_cells = sum(as.numeric(n_cells), na.rm = TRUE)
    )
  prior_occ <- ts_all |>
    filter(yearmonth < recent_cutoff_ym) |>
    summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE))

  shiny_data$overview_last_year <- list(
    label = recent_label,
    cutoff_ym = recent_cutoff_ym,
    occ_last_year = recent_occ$total_occ[1],
    occ_prior = prior_occ$total_occ[1],
    cells_active_last_year = recent_occ$n_cells[1]
  )
  cli_alert_success(
    "Overview last-year: {scales::comma(recent_occ$total_occ[1])} occurrences in {recent_label}"
  )
}

if (!is.null(overview_cly) && !is.null(shiny_data$overview_last_year)) {
  shiny_data$overview_last_year$cells_newly_covered <- sum(overview_cly$newly_covered, na.rm = TRUE)
  cli_alert_success(
    "Cells newly covered: {scales::comma(shiny_data$overview_last_year$cells_newly_covered)}"
  )
}

# Resolved cells
if (!is.null(overview_cly) && !is.null(shiny_data$priority_zero_cells)) {
  zero_codes <- shiny_data$priority_zero_cells$eeacellcode
  resolved <- overview_cly |>
    filter(eeacellcode %in% zero_codes, occ_last_year > 0)
  shiny_data$priority_resolved_last_year <- resolved
  if (!is.null(shiny_data$overview_last_year)) {
    shiny_data$overview_last_year$cells_resolved <- nrow(resolved)
  }
  cli_alert_success("Priority resolved: {nrow(resolved)} formerly-zero cells got data")
}


# ==============================================================================
# 13. Troudet-Style Bias Data
# ==============================================================================
# Known species richness (match_summary) vs occurrence count (order_temporal),
# split by prior/last_year. The app's scope toggles handle establishmentMeans
# on the fly; the dyntaxa/threatened/invasive/sensitive scope toggles use the
# pre-computed scope summaries (see section 4).

cli_h2("Computing Troudet-Style Bias Data")

if (!is.null(match_summary) && !is.null(order_temporal)) {
  year_cutoff <- as.integer(substr(as.character(recent_cutoff_ym), 1, 4))

  # Class level
  known_by_class <- match_summary |> as_tibble() |>
    filter(!is.na(class), class != "") |>
    group_by(kingdom, phylum, class) |>
    summarise(
      n_known_species = n(),
      n_in_gbif = sum(matched_any, na.rm = TRUE),
      .groups = "drop"
    )

  order_to_class <- match_summary |> as_tibble() |>
    filter(!is.na(order), order != "", !is.na(class), class != "") |>
    distinct(kingdom, phylum, class, order)

  occ_by_class <- order_temporal |> as_tibble() |>
    inner_join(order_to_class, by = "order") |>
    mutate(era = ifelse(year >= year_cutoff, "last_year", "prior")) |>
    group_by(kingdom, phylum, class, era) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)

  if (!"prior" %in% names(occ_by_class)) occ_by_class$prior <- 0
  if (!"last_year" %in% names(occ_by_class)) occ_by_class$last_year <- 0

  shiny_data$troudet_bias <- known_by_class |>
    left_join(occ_by_class, by = c("kingdom", "phylum", "class")) |>
    mutate(
      prior = replace_na(prior, 0),
      last_year = replace_na(last_year, 0),
      total_occ = prior + last_year,
      total_known = sum(n_known_species),
      total_occ_all = sum(total_occ),
      pct_known = n_known_species / total_known,
      ideal_occ = pct_known * total_occ_all,
      bias = total_occ - ideal_occ
    ) |>
    select(kingdom, phylum, class, n_known_species, n_in_gbif,
           occ_prior = prior, occ_last_year = last_year, total_occ,
           ideal_occ, bias) |>
    arrange(desc(abs(bias)))
  cli_alert_success("Troudet bias (class): {nrow(shiny_data$troudet_bias)} classes")

  # Order level
  known_by_order <- match_summary |> as_tibble() |>
    filter(!is.na(order), order != "") |>
    group_by(kingdom, phylum, class, order) |>
    summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop")

  occ_by_order <- order_temporal |> as_tibble() |>
    inner_join(order_to_class, by = "order") |>
    mutate(era = ifelse(year >= year_cutoff, "last_year", "prior")) |>
    group_by(kingdom, phylum, class, order, era) |>
    summarise(occurrences = sum(total_occurrences, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)

  if (!"prior" %in% names(occ_by_order)) occ_by_order$prior <- 0
  if (!"last_year" %in% names(occ_by_order)) occ_by_order$last_year <- 0

  shiny_data$troudet_bias_order <- known_by_order |>
    left_join(occ_by_order, by = c("kingdom", "phylum", "class", "order")) |>
    mutate(
      prior = replace_na(prior, 0),
      last_year = replace_na(last_year, 0),
      total_occ = prior + last_year,
      total_known = sum(n_known_species),
      total_occ_all = sum(total_occ),
      pct_known = n_known_species / total_known,
      ideal_occ = pct_known * total_occ_all,
      bias = total_occ - ideal_occ
    ) |>
    select(kingdom, phylum, class, order, n_known_species, n_in_gbif,
           occ_prior = prior, occ_last_year = last_year, total_occ,
           ideal_occ, bias) |>
    arrange(desc(abs(bias)))
  cli_alert_success("Troudet bias (order): {nrow(shiny_data$troudet_bias_order)} orders")

  # ----- Family-level Troudet bias -----
  # Needed by the app's Troudet chart when the user drills down to an order
  # (the chart shows families within that order). Uses family_time_summary
  # from 09c ("all" scope) plus match_summary for the known species counts.

  fts_all <- shiny_data$all_family_time_summary
  if (!is.null(fts_all) && "family" %in% names(match_summary) && "order" %in% names(match_summary)) {

    # Build order+family -> higher taxonomy map from match_summary
    order_to_family <- match_summary |> as_tibble() |>
      filter(!is.na(family), family != "",
             !is.na(order), order != "",
             !is.na(class), class != "") |>
      distinct(kingdom, phylum, class, order, family)

    # Observed occurrence splits by family
    occ_by_family <- fts_all |> as_tibble() |>
      filter(basisofrecord == "all") |>
      inner_join(order_to_family, by = c("order", "family")) |>
      mutate(era = ifelse(year >= year_cutoff, "last_year", "prior")) |>
      group_by(kingdom, phylum, class, order, family, era) |>
      summarise(occurrences = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = era, values_from = occurrences, values_fill = 0)

    if (!"prior" %in% names(occ_by_family)) occ_by_family$prior <- 0
    if (!"last_year" %in% names(occ_by_family)) occ_by_family$last_year <- 0

    # Known species per family from match_summary
    known_by_family <- match_summary |> as_tibble() |>
      filter(!is.na(family), family != "") |>
      group_by(kingdom, phylum, class, order, family) |>
      summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop")

    shiny_data$troudet_bias_family <- known_by_family |>
      left_join(occ_by_family, by = c("kingdom", "phylum", "class", "order", "family")) |>
      mutate(
        prior = replace_na(prior, 0),
        last_year = replace_na(last_year, 0),
        total_occ = prior + last_year,
        total_known = sum(n_known_species),
        total_occ_all = sum(total_occ),
        pct_known = n_known_species / total_known,
        ideal_occ = pct_known * total_occ_all,
        bias = total_occ - ideal_occ
      ) |>
      select(kingdom, phylum, class, order, family, n_known_species, n_in_gbif,
             occ_prior = prior, occ_last_year = last_year, total_occ,
             ideal_occ, bias) |>
      arrange(desc(abs(bias)))
    cli_alert_success("Troudet bias (family): {nrow(shiny_data$troudet_bias_family)} families")
  } else {
    cli_alert_warning("Cannot build troudet_bias_family (family_time_summary or match_summary family column missing)")
  }

  # Join occ_prior/occ_last_year into tax_by_order (used by the tax bar charts)
  if (!is.null(shiny_data$tax_by_order)) {
    to_add_order <- shiny_data$troudet_bias_order |>
      select(kingdom, phylum, class, order, occ_prior, occ_last_year, total_occ)
    shiny_data$tax_by_order <- shiny_data$tax_by_order |>
      left_join(to_add_order, by = c("kingdom", "phylum", "class", "order")) |>
      mutate(
        occ_prior = replace_na(occ_prior, 0),
        occ_last_year = replace_na(occ_last_year, 0),
        total_occ = replace_na(total_occ, 0)
      )
  }

  # Also join into tax_by_family
  if (!is.null(shiny_data$tax_by_family) && !is.null(shiny_data$troudet_bias_family)) {
    to_add_family <- shiny_data$troudet_bias_family |>
      select(kingdom, phylum, class, order, family, occ_prior, occ_last_year, total_occ)
    shiny_data$tax_by_family <- shiny_data$tax_by_family |>
      left_join(to_add_family, by = c("kingdom", "phylum", "class", "order", "family")) |>
      mutate(
        occ_prior = replace_na(occ_prior, 0),
        occ_last_year = replace_na(occ_last_year, 0),
        total_occ = replace_na(total_occ, 0)
      )
  }
}


# ==============================================================================
# 14. Publisher Data (from 06a)
# ==============================================================================

cli_h2("Loading Publisher Data")

pub_path <- here(p_derived, "publisher_summary_10km.csv")
if (file.exists(pub_path)) {
  shiny_data$publisher_summary <- as_tibble(fread(pub_path))
  cli_alert_success("Publisher summary: {nrow(shiny_data$publisher_summary)} publishers")
}

pub_cell_path <- here(p_derived, "publisher_cell_dependency_10km.csv")
if (file.exists(pub_cell_path)) {
  shiny_data$publisher_cell_dependency <- as_tibble(fread(pub_cell_path))
}


# ==============================================================================
# 15. Metadata
# ==============================================================================

cli_h2("Adding Metadata")

dataset_names <- names(shiny_data)

shiny_data$metadata <- list(
  created_at = Sys.time(),
  created_by = "scripts/11_prepare_gap_finder_data.R",
  r_version = R.version.string,
  n_datasets = length(dataset_names),
  datasets = dataset_names,

  # Country — from the active config (whichever GBIF_GAP_COUNTRY selected at build)
  country_name = cfg_get("country.name", COUNTRY_CODE),
  country_code = cfg_get("country.code", COUNTRY_CODE),

  # Taxonomy backbone info — DOI now resolved from dataset_key (01b), not config
  taxonomy_name = cfg_get("taxonomy.name", "National Taxonomy"),
  taxonomy_doi  = data_sources_meta$checklists$taxonomy$doi %||% cfg_get("taxonomy.doi", NULL),

  n_cells_10km = if (!is.null(shiny_data$grid_10km)) nrow(shiny_data$grid_10km) else NA,
  n_cells_50km = if (!is.null(shiny_data$grid_50km)) nrow(shiny_data$grid_50km) else NA,

  has_spatial = !is.null(shiny_data$spatial_gaps_10km),
  has_temporal = !is.null(shiny_data$temporal_year) || !is.null(shiny_data$time_summary_10km),
  has_taxonomic = !is.null(shiny_data$tax_by_rank) || !is.null(shiny_data$taxonomic_match_summary),
  has_threat_status = !is.null(shiny_data$tax_by_threat),
  has_orders = !is.null(shiny_data$order_temporal),
  has_priorities = !is.null(shiny_data$priority_taxa_missing) || !is.null(shiny_data$priority_taxa_all),
  has_last_year = !is.null(shiny_data$overview_last_year),
  has_troudet = !is.null(shiny_data$troudet_bias),
  has_troudet_family = !is.null(shiny_data$troudet_bias_family),
  has_invasive = !is.null(shiny_data$tax_by_invasive),
  has_dyntaxa_scope = !is.null(shiny_data$species_scope_lookup),

  # Per-scope flags
  has_all_scope         = !is.null(shiny_data$all_time_summary),
  has_dyntaxa_scope_data = !is.null(shiny_data$dyntaxa_time_summary),
  has_threatened_scope  = !is.null(shiny_data$threatened_time_summary),
  has_invasive_scope    = !is.null(shiny_data$invasive_time_summary),
  has_sensitive_scope   = !is.null(shiny_data$sensitive_time_summary),

  has_kingdom_cell_recency = !is.null(shiny_data$kingdom_cell_recency),
  last_year = shiny_data$last_year %||% NA,
  recent_label = shiny_data$recent_label %||% NA,

  # Resolved provenance for the Data & sources tab: cube DOIs, checklist DOIs,
  # contributing datasets + publisher count (from 01b)
  data_sources = data_sources_meta
)


# ==============================================================================
# Save Bundle
# ==============================================================================

cli_h2("Saving Shiny Data Bundle")

saveRDS(shiny_data, shiny_data_path, compress = "xz")
file_size_mb <- file.size(shiny_data_path) / 1024^2
cli_alert_success("Saved: {.path {shiny_data_path}} ({round(file_size_mb, 2)} MB)")


# ==============================================================================
# Final Summary
# ==============================================================================

cli_h1("Summary (Script 11)")

n_spatial <- sum(str_detect(dataset_names, "spatial|grid|cell|comparison"))
n_temporal <- sum(str_detect(dataset_names, "temporal|recency|time|last_year"))
n_taxonomic <- sum(str_detect(dataset_names, "tax|order|family|threatened|kingdom"))
n_priority <- sum(str_detect(dataset_names, "priority"))
# Categories aren't disjoint (e.g. `cell_last_year` matches spatial + temporal),
# so the buckets can sum to more than length(dataset_names). Clamp Other at 0.
n_other <- max(0, length(dataset_names) - n_spatial - n_temporal - n_taxonomic - n_priority)

summary_dt <- data.table(
  Category = c("Spatial", "Temporal", "Taxonomic", "Priority", "Other", "TOTAL"),
  Datasets = c(n_spatial, n_temporal, n_taxonomic, n_priority, n_other, length(dataset_names))
)
print(summary_dt)

cli_alert_info("")
cli_alert_success("Shiny data preparation complete!")
cli_alert_info("Bundle size: {round(file_size_mb, 2)} MB")
cli_alert_info("Recent period: {shiny_data$metadata$recent_label}")
cli_alert_info("Scopes available: {paste(c('all','dyntaxa','threatened','invasive','sensitive')[c(shiny_data$metadata$has_all_scope, shiny_data$metadata$has_dyntaxa_scope_data, shiny_data$metadata$has_threatened_scope, shiny_data$metadata$has_invasive_scope, shiny_data$metadata$has_sensitive_scope)], collapse = ', ')}")
