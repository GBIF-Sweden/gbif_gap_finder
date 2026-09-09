# R/eea_grid.R
# ============================================================================
# EEA reference-grid cell codes from WGS84 coordinates
# ============================================================================
# Purpose:
#   Reproduce GBIF's GBIF_EEARGCode(resolution, lat, lon, 0) in R, so historical
#   snapshots (which carry raw latitude/longitude, not eeacellcode) can be
#   gridded to exactly the same cells as the live SQL occurrence cube.
#
#   The live cube gets its cells server-side:
#     GBIF_EEARGCode(${RESOLUTION}, decimallatitude, decimallongitude, 0)
#   with randomisation radius 0, so cell assignment is deterministic and this
#   function reproduces it.
#
# THE NON-OBVIOUS PART — read before changing anything here:
#   The E/N numbers in an EEA code are in units of 10,000 m at BOTH resolutions,
#   after the lower-left corner has been snapped to the cell size:
#     10kmE452N354  ->  X in [4,520,000 , 4,530,000)
#     50kmE450N350  ->  X in [4,500,000 , 4,550,000)   <- 450 = 4,500,000/10,000
#   A naive floor(x / res) gives E90 for that 50 km cell and produces
#   plausible-looking codes that never join to anything. Snap first, then
#   divide by 10,000 — always 10,000, never the resolution.
#
# Verified (2026-08-20) against the project's own grids:
#   grids_10km.gpkg  6,308 / 6,308 cells exact
#   grids_50km.gpkg    297 /   297 cells exact
#   and floor(E10/5)*5 == E50 for all 460,791 distinct coordinates in the
#   2024-01-01 historical snapshot.
#   Re-run that proof any time with eea_grid_selftest().
#
# Sourced by: scripts/00_setup.R (via R/globals.R conventions)
# Dependencies: sf
# ============================================================================

#' EEA reference-grid cell code for WGS84 coordinates
#'
#' @param lat    Numeric vector of decimal latitudes (EPSG:4326).
#' @param lon    Numeric vector of decimal longitudes (EPSG:4326).
#' @param res_m  Grid resolution in metres (10000L or 50000L).
#' @return Character vector of cell codes, e.g. "10kmE452N354". Non-finite or
#'   out-of-range coordinates yield NA rather than a bogus code.
eea_cell_code <- function(lat, lon, res_m) {
  if (length(lat) != length(lon)) {
    cli_abort("eea_cell_code(): lat and lon must be the same length")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli_abort("Package {.pkg sf} is required to project coordinates to EPSG:3035")
  }

  res_m <- as.integer(res_m)
  if (is.na(res_m) || res_m <= 0L || res_m %% 1000L != 0L) {
    cli_abort("eea_cell_code(): res_m must be a positive whole number of km in metres")
  }

  out <- rep(NA_character_, length(lat))

  # Guard the projection: sf_project() will happily return Inf for a latitude of
  # 90 or a NaN input, and Inf silently becomes a garbage cell code downstream.
  ok <- is.finite(lat) & is.finite(lon) &
    lat > -90 & lat < 90 & lon >= -180 & lon <= 180
  if (!any(ok)) return(out)

  xy <- sf::sf_project(
    from = "EPSG:4326", to = "EPSG:3035",
    pts  = cbind(lon[ok], lat[ok]),
    keep = TRUE
  )

  finite_xy <- is.finite(xy[, 1]) & is.finite(xy[, 2])

  # Snap the point's lower-left cell corner to the resolution ...
  sx <- floor(xy[finite_xy, 1] / res_m) * res_m
  sy <- floor(xy[finite_xy, 2] / res_m) * res_m

  # ... then express that corner in units of 10,000 m. round(), not floor():
  # sx/10000 is mathematically a whole number here, but floating-point can land
  # it at 354.9999999999999, and floor() would then silently shift the cell by
  # one. This exact trap cost 185 of 297 cells on the first attempt.
  e <- as.integer(round(sx / 10000))
  n <- as.integer(round(sy / 10000))

  # EEA reference-grid codes are unsigned by construction, so a negative E or N
  # means the point is outside the grid's domain entirely and there is no such
  # cell. Without this, lat/lon 0/0 — the commonest "coordinates failed to
  # parse" value in any occurrence dataset — projects cleanly and returns
  # "10kmE308N-230": a code that looks real, joins to nothing, and quietly
  # inflates the distinct-cell count.
  in_grid <- e >= 0L & n >= 0L

  idx <- which(ok)[finite_xy][in_grid]
  out[idx] <- sprintf(
    "%dkmE%dN%d", res_m %/% 1000L, e[in_grid], n[in_grid]
  )
  out
}

#' Drop records whose coordinates cannot be trusted
#'
#' The historical snapshots cannot replicate the live cube's
#' `hasgeospatialissues = FALSE` filter, because the historical index carries no
#' such flag. This is the documented approximation of it. It is deliberately
#' conservative: it removes only records that are self-evidently broken, and
#' leaves "merely surprising" ones for the grid-membership filter in 04b.
#'
#' @param dt      data.table with latitude / longitude columns.
#' @param lat_col,lon_col Column names.
#' @param verbose Report what was dropped and why.
#' @return The filtered data.table (a new object; `dt` is not modified).
geo_sanity_filter <- function(dt, lat_col = "latitude", lon_col = "longitude",
                              verbose = TRUE) {
  dt <- data.table::as.data.table(dt)
  n0 <- nrow(dt)
  lat <- dt[[lat_col]]
  lon <- dt[[lon_col]]

  # Categories are made mutually exclusive so the tallies below sum to the drop
  # count instead of double-reporting the same row.
  bad_na    <- !is.finite(lat) | !is.finite(lon)
  # |lat| > 90 is impossible: almost always lat/lon written the wrong way round.
  bad_swap  <- !bad_na & abs(lat) > 90
  bad_range <- !bad_na & !bad_swap & abs(lon) > 180
  # Null Island: the classic "coordinates failed to parse" signature.
  bad_zero  <- !bad_na & !bad_swap & !bad_range & lat == 0 & lon == 0

  drop <- bad_na | bad_swap | bad_range | bad_zero
  out  <- dt[!drop]

  if (isTRUE(verbose)) {
    cli_alert_info(
      "geo-sanity: dropped {scales::comma(sum(drop))} / {scales::comma(n0)} rows \\
       (missing/non-finite {sum(bad_na)}, transposed {sum(bad_swap)}, \\
       out-of-range {sum(bad_range)}, 0/0 {sum(bad_zero)})"
    )
  }
  out[]
}

#' Prove eea_cell_code() against the project's own reference grids
#'
#' Reads grids_10km.gpkg / grids_50km.gpkg, takes each cell's CENTROID, converts
#' it back to WGS84, and checks that eea_cell_code() returns the cell code the
#' grid file itself stores. Centroids (not corners) are used deliberately: the
#' stored corner coordinates carry float noise (3549999.9999999986), which is a
#' property of the grid file, not of this function.
#'
#' This is the offline replacement for the live-SQL parity sample: the grid files
#' ARE the ground truth the pipeline measures coverage against, so agreeing with
#' them is what actually matters.
#'
#' @return Invisibly, a data.frame of results per resolution. Aborts on mismatch.
eea_grid_selftest <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli_abort("Package {.pkg sf} is required for eea_grid_selftest()")
  }
  grids <- list(
    list(res = 10000L, path = here(p_data_proc, "grids_10km.gpkg")),
    list(res = 50000L, path = here(p_data_proc, "grids_50km.gpkg"))
  )

  results <- list()
  for (g in grids) {
    if (!file.exists(g$path)) {
      cli_alert_warning("Grid not found, skipping: {.path {g$path}} (run script 02)")
      next
    }
    grid <- sf::st_read(g$path, quiet = TRUE)
    code_field <- guess_cellcode_field(names(grid))
    codes <- as.character(grid[[code_field]])

    cent <- sf::st_coordinates(
      sf::st_transform(sf::st_centroid(sf::st_geometry(grid)), 4326)
    )
    pred <- eea_cell_code(lat = cent[, 2], lon = cent[, 1], res_m = g$res)

    n_ok  <- sum(!is.na(pred) & pred == codes)
    n_all <- length(codes)
    results[[paste0(g$res %/% 1000L, "km")]] <-
      data.frame(res_km = g$res %/% 1000L, n_cells = n_all, n_match = n_ok)

    if (n_ok == n_all) {
      cli_alert_success("EEA {g$res %/% 1000L} km: {n_ok}/{n_all} cell codes reproduced exactly")
    } else {
      bad <- head(data.frame(stored = codes, predicted = pred)[pred != codes, ], 5)
      cli_abort(c(
        "eea_cell_code() disagrees with {basename(g$path)}: {n_ok}/{n_all} match",
        "x" = "First mismatches: {paste(bad$stored, '->', bad$predicted, collapse = '; ')}",
        "i" = "Do NOT grid historical data until this passes."
      ))
    }
  }
  invisible(do.call(rbind, results))
}
