# scripts/04b_build_historical_cubes.R
# ============================================================================
# Build cube-schema parquet from GBIF historical snapshots
# ============================================================================
# Purpose:
#   Turn GBIF Sweden's aggregated historical exports into files that are
#   indistinguishable, schema-wise, from the live SQL occurrence cube — so the
#   existing pipeline (05 -> 06a/06b -> 07/08/09 -> 10/11) can run over a
#   snapshot without any script downstream knowing it is looking at history.
#
#   This is the historical counterpart of 04_convert_cubes_parquet.R. It does
#   NOT go through 04: the delivery carries raw coordinates rather than
#   eeacellcode, and 04 hard-stops on the missing `kingdom` column.
#
# What it does, per snapshot:
#   1. geo-sanity filter        (stands in for hasgeospatialissues = FALSE)
#   2. grid to EEA 10 km + 50 km via eea_cell_code()   [R/eea_grid.R]
#   3. restrict to the Swedish 10 km grid domain
#   4. aggregate to the cube grain
#   5. join the snapshot's OWN taxonomy lookup on species_id (1:1)
#   6. crosswalk to the live cube schema and write parquet + a manifest, into
#      the Stage 2 time-point layout: proc/timepoints/{tp}/cubes/
#
# WHY THE AGGREGATE COMES BEFORE THE JOIN (step 4 before step 5):
#   Every taxonomy column is functionally dependent on species_id, and the
#   lookup is 1:1, so joining before or after aggregating gives identical
#   results. Aggregating first means the six character columns are carried over
#   ~3 M grouped rows instead of 65.7 M raw ones — roughly 3 GB of peak memory
#   that a laptop does not have to find. Do not "simplify" this by joining
#   first.
#
# WHAT CHANGED FROM THE FIRST DELIVERY (see
# claude/finding-historic-delivery-v2-verified-2026-09-08.md):
#   - Grouped by `species_id`, not `taxon_id`. No sub-specific roll-up exists
#     and none is needed: the live cube is species-rank only.
#   - The taxonomy arrives WITH the delivery, contemporaneous with the snapshot.
#     That is strictly better than a backbone join, so script 01d is retired —
#     no version matching, no roll-up, no API fallback, no "wrong build".
#   - UTF-8, so no decode step runs (the decoder is kept as a guard).
#
# THE THREE MEASURES WE CANNOT BACK-CAST — and why NA is safe here:
#   mincoordinateuncertaintyinmeters, mintemporaluncertainty, distinctobservers
#   are absent from the historical index. Audited 2026-08-20: they appear in
#   EXACTLY two places in this repo — 04's `expected_cols` (they are NOT in
#   `required_cols`) and an explicitly informational check in 05. No analysis
#   script reads them. So filling them with NA costs nothing, and no analysis
#   dimension has to be scoped out. They are typed NA_real_, not NA, so the
#   parquet schema matches the live cube instead of becoming logical.
#
# COMPARABILITY WARNING:
#   Do NOT compare these cubes against the full-filtered live cube. The live
#   cube applies hasgeospatialissues = FALSE and occurrencestatus = 'PRESENT',
#   which cannot be replicated here, and it is COL-interpreted while these are
#   legacy-Backbone. Within the snapshot regime differencing is clean; across
#   the regime boundary see claude/brief-historical-comparable-current-cube.md.
#
# PRECISION CAVEAT (permanent — 3 dp was requested and refused on export size):
#   Delivered coordinates are 2 dp. At 57 deg N that is 1.11 km N-S / 0.61 km
#   E-W, so roughly 4% of records land in a neighbouring 10 km cell (about 0.9%
#   at 50 km). Carry this on every 10 km temporal figure.
#
# ABSENCE RECORDS — dormant, not unnecessary:
#   GBIF's export SQL carries no `occurrence_status` filter, while the live cube
#   filters `occurrencestatus = 'PRESENT'`. The two SLU National Forest
#   Inventory presence-absence datasets are excluded by the delivery query and
#   contribute ZERO rows to both current snapshots anyway (both were registered
#   2024-10-25, after both cut dates — verified, not assumed). They hold ~14.1 M
#   absence records today and WILL land in any snapshot dated after that, spread
#   over a systematic national forest grid, i.e. over exactly the remote cells
#   this analysis is about. Any later snapshot must keep the same exclusion, and
#   must only be compared against snapshots filtered the same way.
#   See claude/finding-absence-records-trap-2026-08-20.md.
#
# Inputs:
#   - data/{CC}/raw/historic/*_occurrences_aggregated_YYYYMMDD.tsv.gz
#   - data/{CC}/raw/historic/*_taxonomy_YYYYMMDD.tsv.gz
#   - data/{CC}/proc/cellcodes_10km.txt | grids_10km.gpkg      (script 02)
# Outputs:
#   - data/{CC}/proc/timepoints/{tp}/cubes/cube_{10,50}km.parquet
#   - data/{CC}/proc/timepoints/{tp}/cubes/cube_manifest.csv
#   - data/{CC}/proc/timepoints/historic_cube_manifest.csv   (all time points)
#
# Config (all optional, defaults shown):
#   parameters.historic.coord_decimals      6   integer lattice precision
#   parameters.historic.min_occ_retained    99  abort below this % retained
#
# Dependencies: scripts/00_setup.R, data.table, arrow, sf,
#               R/eea_grid.R, R/historic_io.R
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# 00_setup.R sources only R/packages.R and R/globals.R, so the historical
# helpers are loaded here. _targets.R tracks both files via the `r_historic`
# target so editing them still invalidates anything that depends on them.
source(here::here("R", "eea_grid.R"))
source(here::here("R", "historic_io.R"))

if (!requireNamespace("arrow", quietly = TRUE)) {
  cli_abort(c("Package {.pkg arrow} is required",
              "i" = "Install: {.code install.packages('arrow')}"))
}
library(arrow)

# ============================================================================
# Configuration
# ============================================================================

# Coordinates arrive pre-rounded from GBIF (ROUND(latitude, 2)), so they sit on
# an exact decimal lattice. Encoding them as integers makes the join key
# explicit and lets the doubles be dropped BEFORE the join rather than after,
# which is where peak memory sits at 65.7 M rows.
COORD_DP <- as.integer(cfg_get("parameters.historic.coord_decimals", 6L))

# The grid-domain filter should remove almost nothing: only 5 of the 462,103
# distinct coordinate pairs across both snapshots fall outside the SE 10 km
# grid. A large drop means the grid, not the data, is wrong.
MIN_OCC_RETAINED <- as.numeric(cfg_get("parameters.historic.min_occ_retained", 99))

# The live cube schema, in order. 04's required_cols is a strict subset.
CUBE_COLS <- c("specieskey", "species", "kingdom", "phylum", "class", "order",
               "family", "basisofrecord", "publishingorgkey", "datasetkey",
               "eeacellcode", "year", "month", "occurrences",
               "mincoordinateuncertaintyinmeters", "mintemporaluncertainty",
               "distinctobservers")

CUBE_REQUIRED <- c("specieskey", "eeacellcode", "year", "month", "occurrences",
                   "basisofrecord", "kingdom", "publishingorgkey", "datasetkey")

RESOLUTIONS <- list(grid10km = 10000L, grid50km = 50000L)

# Regression fixtures, measured independently (numpy + pyproj over the same
# files, 2026-09-09; see claude/finding-step0-probe-2026-09-09.md). A snapshot
# listed here MUST reproduce these exactly — if it does not, the R gridding or
# taxonomy path has drifted, and that is worth failing the build over. A
# snapshot not listed here is simply new and is reported, not judged.
HISTORIC_EXPECTED <- list(
  `2021-01-01` = list(rows_in = 51065888, occ_in = 94768320,
                      n_species = 53141L, n_cells10 = 5518L, n_cells50 = 287L),
  `2024-01-01` = list(rows_in = 65665237, occ_in = 121941413,
                      n_species = 62497L, n_cells10 = 5838L, n_cells50 = 292L)
)

cli_h1("Historical snapshot cubes — {COUNTRY_CODE}")

# ============================================================================
# Preconditions — fail early and loudly
# ============================================================================

# Prove the gridding before touching any data. Cheap, and the failure mode it
# guards against (silently shifted cells) is invisible downstream.
cli_h2("EEA grid parity check")
eea_grid_selftest()

#' Full cell-code universe for a resolution.
#' Mirrors get_all_cellcodes() in 07: sidecar txt when fresh, else the gpkg.
grid_cellcodes <- function(res_km) {
  codes_path <- here(p_data_proc, sprintf("cellcodes_%dkm.txt", res_km))
  gpkg_path  <- here(p_data_proc, sprintf("grids_%dkm.gpkg", res_km))
  if (file.exists(codes_path) &&
      (!file.exists(gpkg_path) || file.mtime(codes_path) >= file.mtime(gpkg_path))) {
    return(unique(readLines(codes_path, warn = FALSE)))
  }
  if (!file.exists(gpkg_path)) {
    cli_abort(c("Grid not found: {.path {gpkg_path}}", "i" = "Run script 02."))
  }
  g <- sf::st_read(gpkg_path, quiet = TRUE)
  unique(as.character(g[[guess_cellcode_field(names(g))]]))
}

codes10 <- grid_cellcodes(10L)
cli_alert_info("SE 10 km grid: {scales::comma(length(codes10))} cells")

snaps <- historic_snapshot_files()
if (nrow(snaps) < 2L) {
  cli_alert_warning(
    "Only {nrow(snaps)} snapshot available. The cubes will build, but gap \\
     CLOSURE needs at least two time points."
  )
}

# ============================================================================
# Build one time point per snapshot
# ============================================================================

manifest <- list()

for (i in seq_len(nrow(snaps))) {
  snap     <- snaps$snapshot[i]
  occ_path <- snaps$occ_path[i]
  tax_path <- snaps$tax_path[i]

  cli_h2("Snapshot {snap}")

  tp_cubes <- file.path(timepoint_dir(snap), "cubes")
  if (!dir.exists(tp_cubes)) dir.create(tp_cubes, recursive = TRUE, showWarnings = FALSE)

  # --- read ---------------------------------------------------------------
  dt <- read_historic_occurrences(occ_path, expect = snap)
  n_in   <- nrow(dt)
  occ_in <- sum(as.numeric(dt$occurrences), na.rm = TRUE)

  # --- 1. geo sanity ------------------------------------------------------
  dt <- geo_sanity_filter(dt)
  n_geo   <- nrow(dt)
  occ_geo <- sum(as.numeric(dt$occurrences), na.rm = TRUE)

  # --- 2. grid ------------------------------------------------------------
  # Project the DISTINCT coordinates only. 65.7 M rows carry just ~0.46 M
  # distinct 2 dp coordinate pairs, so this turns a 65.7 M-point reprojection
  # into a 0.46 M-point one plus a keyed join.
  cli_alert_info("Gridding distinct coordinates")

  S <- 10^COORD_DP
  if (max(abs(dt$latitude), na.rm = TRUE) * S > .Machine$integer.max) {
    cli_abort("coord_decimals = {COORD_DP} overflows 32-bit integers — lower it.")
  }
  off <- dt[, sum(abs(latitude * S - round(latitude * S)) > 1e-3 |
                  abs(longitude * S - round(longitude * S)) > 1e-3, na.rm = TRUE)]
  if (off > 0) {
    cli_abort(c(
      "{scales::comma(off)} coordinates carry more than {COORD_DP} decimal places.",
      "i" = "Raise {.code parameters.historic.coord_decimals}."
    ))
  }
  dt[, lat_i := as.integer(round(latitude * S))]
  dt[, lon_i := as.integer(round(longitude * S))]

  xy <- unique(dt[, .(lat_i, lon_i)])
  cli_alert_info("Distinct coordinate pairs: {scales::comma(nrow(xy))}")

  xy[, eeacell_10km := eea_cell_code(lat_i / S, lon_i / S, 10000L)]
  xy[, eeacell_50km := eea_cell_code(lat_i / S, lon_i / S, 50000L)]

  # Self-check: the 10 km grid must nest inside the 50 km grid. Cheap, and it
  # catches a wrong resolution constant instantly. NA on either side means the
  # point is outside the grid domain and is excluded from the check rather than
  # counted as a failure — step 3 removes those rows anyway.
  e10 <- as.integer(sub("^10kmE([0-9]+)N[0-9]+$", "\\1", xy$eeacell_10km))
  n10 <- as.integer(sub("^10kmE[0-9]+N([0-9]+)$", "\\1", xy$eeacell_10km))
  testable <- !is.na(xy$eeacell_10km) & !is.na(xy$eeacell_50km)
  nest_ok <- sprintf("50kmE%dN%d", (e10 %/% 5L) * 5L, (n10 %/% 5L) * 5L) ==
    xy$eeacell_50km
  n_bad_nest <- sum(!nest_ok[testable])
  if (n_bad_nest > 0L) {
    cli_abort("10 km cells do not nest in 50 km cells for \\
               {scales::comma(n_bad_nest)} coordinate{?s} — check eea_cell_code()")
  }
  cli_alert_success(
    "Nesting self-check passed ({scales::comma(sum(testable))} coordinates)"
  )

  # Drop the doubles BEFORE the join — the join doubles peak memory, so paying
  # 2 int columns instead of 2 double columns is worth ~0.5 GB at this scale.
  dt[, c("latitude", "longitude") := NULL]
  data.table::setkey(xy, lat_i, lon_i)
  data.table::setkey(dt, lat_i, lon_i)
  dt <- xy[dt]
  dt[, c("lat_i", "lon_i") := NULL]

  # --- 3. restrict to the Swedish grid domain -----------------------------
  # Anything outside the 10 km grid is outside the universe 07 measures coverage
  # against, so it cannot contribute to a gap either way. The 10 km grid also
  # DEFINES the country domain for 50 km (07's filter_coarse_grid_to_country()
  # intersects the dissolved 10 km grid), so filtering on it keeps both honest.
  dt <- dt[!is.na(eeacell_10km) & eeacell_10km %chin% codes10]
  occ_kept <- sum(as.numeric(dt$occurrences), na.rm = TRUE)

  # Two retention figures, deliberately. `pct_grid` isolates THIS step and is
  # what the floor tests: geo-sanity drops above are legitimate removals of
  # broken rows, so folding them in would let a real grid failure hide behind a
  # dirty delivery (or a clean delivery mask a small one). `pct_occ_kept` is the
  # end-to-end number and is what goes in the manifest.
  pct_grid     <- if (occ_geo > 0) 100 * occ_kept / occ_geo else NA_real_
  pct_occ_kept <- if (occ_in  > 0) 100 * occ_kept / occ_in  else NA_real_
  cli_alert_info(
    "Grid domain: kept {scales::comma(nrow(dt))} / {scales::comma(n_geo)} rows, \\
     {round(pct_grid, 4)}% of post-sanity occurrences \\
     ({round(pct_occ_kept, 4)}% of the delivery)"
  )
  if (!is.na(pct_grid) && pct_grid < MIN_OCC_RETAINED) {
    cli_abort(c(
      "Only {round(pct_grid, 2)}% of occurrences fall inside the SE 10 km grid \\
       (floor is {MIN_OCC_RETAINED}%).",
      "x" = "That is a grid or coordinate problem, not a data problem — \\
             refusing to write a cube that has silently lost its country.",
      "i" = "Check {.path cellcodes_10km.txt} and re-run {.code eea_grid_selftest()}."
    ))
  }

  # --- 4. taxonomy lookup (read now, joined after the aggregate) ----------
  tax <- read_historic_taxonomy(tax_path, expect = snap)
  n_species_occ <- data.table::uniqueN(dt$species_id)
  orphans <- setdiff(unique(dt$species_id), tax$species_id)
  if (length(orphans)) {
    cli_abort(c(
      "{scales::comma(length(orphans))} species_id value{?s} in the occurrence \\
       file have no row in the taxonomy lookup for {snap}",
      "x" = "Those rows would join to NA and be dropped, quietly shrinking the cube.",
      "i" = "First few: {paste(utils::head(orphans, 5), collapse = ', ')}"
    ))
  }
  cli_alert_success(
    "Referential integrity: {scales::comma(n_species_occ)} species in the \\
     occurrence file, 0 orphans against a {scales::comma(nrow(tax))}-row lookup"
  )

  # --- 5. crosswalk names shared by both resolutions ----------------------
  data.table::setnames(dt, "basis_of_record", "basisofrecord",    skip_absent = TRUE)
  data.table::setnames(dt, "dataset_id",      "datasetkey",       skip_absent = TRUE)
  data.table::setnames(dt, "publisher_id",    "publishingorgkey", skip_absent = TRUE)

  tax_out <- data.table::copy(tax)
  data.table::setnames(tax_out, "class_rank", "class", skip_absent = TRUE)
  data.table::setnames(tax_out, "order_rank", "order", skip_absent = TRUE)
  # `genus` has no column in the live cube schema; dropping it here keeps the
  # parquet schema byte-identical to the live one rather than merely compatible.
  if ("genus" %in% names(tax_out)) tax_out[, genus := NULL]

  # --- 6. aggregate + join + write, per resolution ------------------------
  for (grid_name in names(RESOLUTIONS)) {
    res_km   <- RESOLUTIONS[[grid_name]] %/% 1000L
    cell_col <- sprintf("eeacell_%dkm", res_km)

    group_cols <- intersect(
      c("species_id", "basisofrecord", "publishingorgkey", "datasetkey",
        cell_col, "year", "month"),
      names(dt)
    )
    cube <- dt[, .(occurrences = sum(as.numeric(occurrences), na.rm = TRUE)),
               by = group_cols]
    data.table::setnames(cube, cell_col, "eeacellcode")

    n_rows_pre <- nrow(cube)
    cube <- tax_out[cube, on = "species_id"]
    if (nrow(cube) != n_rows_pre) {
      cli_abort(
        "Taxonomy join changed the row count ({scales::comma(n_rows_pre)} -> \\
         {scales::comma(nrow(cube))}) — the lookup is not 1:1 after all."
      )
    }

    # specieskey is a character key on the live cube (COL ids are alphanumeric),
    # so the historical integer key is cast rather than left numeric — otherwise
    # every cross-table join downstream silently fails to match.
    cube[, specieskey := as.character(species_id)]
    cube[, species_id := NULL]

    # The three measures the historical index cannot carry. Typed NA_real_ so the
    # parquet schema matches the live cube rather than becoming logical.
    cube[, mincoordinateuncertaintyinmeters := NA_real_]
    cube[, mintemporaluncertainty           := NA_real_]
    cube[, distinctobservers                := NA_real_]

    missing_required <- setdiff(CUBE_REQUIRED, names(cube))
    if (length(missing_required)) {
      cli_abort("Historical cube is missing required column{?s}: \\
                 {paste(missing_required, collapse = ', ')}")
    }
    extra <- setdiff(names(cube), CUBE_COLS)
    if (length(extra)) {
      cli_abort("Historical cube has column{?s} the live schema does not: \\
                 {paste(extra, collapse = ', ')}")
    }
    data.table::setcolorder(cube, CUBE_COLS)

    out <- file.path(tp_cubes, sprintf("cube_%dkm.parquet", res_km))
    write_parquet(cube, out)

    n_cells   <- data.table::uniqueN(cube$eeacellcode)
    n_species <- data.table::uniqueN(cube$specieskey)
    cli_alert_success(
      "{basename(out)}: {scales::comma(nrow(cube))} rows, \\
       {scales::comma(n_species)} species, {scales::comma(n_cells)} cells, \\
       {scales::comma(sum(cube$occurrences))} occurrences"
    )

    manifest[[paste(snap, grid_name)]] <- data.frame(
      snapshot     = snap,
      timepoint    = snap,
      grid         = grid_name,
      rows         = nrow(cube),
      n_species    = n_species,
      n_cells      = n_cells,
      total_occ    = sum(cube$occurrences),
      rows_in         = n_in,
      occ_in          = occ_in,
      rows_geo_sanity = n_in - n_geo,
      occ_geo_sanity  = occ_in - occ_geo,
      pct_grid_domain = round(pct_grid, 4),
      pct_occ_kept    = round(100 * sum(cube$occurrences) / occ_in, 4),
      parquet_mb      = round(file.size(out) / 1024^2, 1),
      file            = out
    )
    rm(cube); invisible(gc())
  }

  # --- 7. regression fixtures --------------------------------------------
  exp_i <- HISTORIC_EXPECTED[[snap]]
  if (is.null(exp_i)) {
    cli_alert_info(
      "No regression fixture for {snap} — this is a new snapshot. Record its \\
       numbers in {.field HISTORIC_EXPECTED} once you have checked them."
    )
  } else {
    got <- list(
      rows_in   = n_in,
      occ_in    = occ_in,
      n_species = manifest[[paste(snap, "grid10km")]]$n_species,
      n_cells10 = manifest[[paste(snap, "grid10km")]]$n_cells,
      n_cells50 = manifest[[paste(snap, "grid50km")]]$n_cells
    )
    bad <- names(exp_i)[
      vapply(names(exp_i),
             function(k) !isTRUE(all.equal(as.numeric(exp_i[[k]]),
                                           as.numeric(got[[k]]))),
             logical(1))
    ]
    if (length(bad)) {
      cli_abort(c(
        "Snapshot {snap} does not reproduce its verified numbers.",
        "x" = "{paste(sprintf('%s: expected %s, got %s', bad,
                              format(unlist(exp_i[bad]), big.mark = ','),
                              format(unlist(got[bad]),   big.mark = ',')),
                      collapse = '; ')}",
        "i" = "These were measured independently over the same files — see \\
               claude/finding-step0-probe-2026-09-09.md. A mismatch is a bug in \\
               the R path, not new data."
      ))
    }
    cli_alert_success(
      "Regression fixture: {snap} reproduces all {length(exp_i)} verified numbers"
    )
  }

  # Per-time-point manifest, so a single time point is self-describing.
  tp_manifest <- data.table::rbindlist(
    manifest[grepl(paste0("^", snap, " "), names(manifest))], fill = TRUE)
  data.table::fwrite(tp_manifest, file.path(tp_cubes, "cube_manifest.csv"))

  rm(dt, tax, tax_out, xy); invisible(gc())
}

# ============================================================================
# Manifest across every time point
# ============================================================================

cli_h2("Manifest")

manifest_df  <- data.table::rbindlist(manifest, fill = TRUE)
tp_root      <- here(p_data_proc, "timepoints")
if (!dir.exists(tp_root)) dir.create(tp_root, recursive = TRUE, showWarnings = FALSE)
manifest_path <- file.path(tp_root, "historic_cube_manifest.csv")
data.table::fwrite(manifest_df, manifest_path)
print(manifest_df[, .(snapshot, grid, rows, n_species, n_cells, total_occ, pct_occ_kept)])
cli_alert_success("{.path {manifest_path}}")

cli_alert_success("Historical cubes complete!")
cli_alert_info(
  "Next: Stage 2 — {.code Rscript run_timepoint.R <date>} runs 05-11 against \\
   one of these time points."
)
