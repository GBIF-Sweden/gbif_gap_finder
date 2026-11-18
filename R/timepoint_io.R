# R/timepoint_io.R
# ============================================================================
# Reading time points: where they live, what they are, and their data
# ============================================================================
# Purpose:
#   The file side of scripts 14 (closure) and 15 (bridge). R/closure.R holds the
#   arithmetic and touches no files; this holds the readers both scripts share,
#   so a time point is located, described and read the same way in both.
#
# Globals these functions read at call time (set by the calling script after
# scripts/00_setup.R): tp_root, LIVE_TP, live_ok, SOURCE_TYPES, plus p_cubes,
# p_gaps, p_data_proc and COUNTRY_CODE from R/globals.R.
#
# THE LIVE CUBE IS A TIME POINT ALREADY. With GAP_FINDER_TIMEPOINT unset,
#   p_timepoint == p_data_proc, so the live pipeline's own outputs -
#   proc/cubes/ and proc/gaps/taxonomic_match_table.csv - ARE that time point's
#   cube and match table. They sit at the root rather than under timepoints/,
#   and are labelled with the cube's download date.
#
# Dependencies: data.table, arrow, dplyr, yaml, cli, glue, here, R/closure.R
# ============================================================================

#' The live cube's download date, or NA - never today's date
#'
#' get_snapshot_date() falls back to Sys.Date() when the metadata carries no
#' cube date. That is right for staleness scoring and wrong here: it would
#' label the live time point with whatever day the script happened to run, and
#' the label is the time point's identity. This reads the same metadata and
#' returns NA rather than guessing.
live_timepoint_date <- function() {
  meta_path <- here(p_data_proc, "data_sources_meta.rds")
  if (!file.exists(meta_path)) return(NA_character_)
  meta  <- tryCatch(readRDS(meta_path), error = function(e) NULL)
  cubes <- meta$cubes
  if (is.null(cubes) || !length(cubes)) return(NA_character_)
  d <- tryCatch(
    do.call(c, lapply(cubes, function(x) {
      v <- x$created
      if (is.null(v) || all(is.na(v))) as.Date(NA) else as.Date(v)
    })),
    error = function(e) as.Date(NA))
  d <- d[!is.na(d)]
  if (!length(d)) NA_character_ else as.character(max(d))
}

#' Where does this time point's data live: under timepoints/, or at the root?
tp_is_live <- function(tp) live_ok && identical(tp, LIVE_TP)
tp_cubes_dir <- function(tp) if (tp_is_live(tp)) here(p_cubes) else file.path(tp_root, tp, "cubes")
tp_gaps_dir  <- function(tp) if (tp_is_live(tp)) here(p_gaps)  else file.path(tp_root, tp, "gaps")

#' Regime and key vocabulary of one time point
#'
#' The live cube is regime "cube" with COL keys by construction (the cube SQL
#' pins COL). Every other point reads reference_version.yml. A point computed
#' before those fields existed is inferred from its cube manifest: 04b's
#' manifest has a `snapshot` column (and `key_backbone` once 04b records it;
#' older 04b runs were all legacy-keyed), a frozen live cube's manifest does not.
tp_meta <- function(tp) {
  if (tp_is_live(tp)) return(list(tp = tp, regime = "cube", key_backbone = "col"))
  ref <- file.path(tp_root, tp, "reference_version.yml")
  r <- if (file.exists(ref)) tryCatch(yaml::read_yaml(ref), error = function(e) NULL)
  regime <- r$regime; kb <- r$key_backbone
  if (is.null(regime) || is.null(kb)) {
    man <- file.path(tp_root, tp, "cubes", "cube_manifest.csv")
    m <- if (file.exists(man)) data.table::fread(man, nrows = 1L) else NULL
    is_snap <- !is.null(m) && "snapshot" %in% names(m)
    if (is.null(regime)) regime <- if (is_snap) "snapshot" else "cube"
    if (is.null(kb)) {
      kb <- if (!is.null(m) && "key_backbone" %in% names(m)) m$key_backbone[1]
            else if (is_snap) "legacy" else "col"
    }
    cli_alert_warning(
      "{tp}: no regime/key_backbone in reference_version.yml - inferred \\
       {regime}/{kb} from the cube manifest. Re-run {.code Rscript run_timepoint.R {tp}} \\
       to record them.")
  }
  list(tp = tp, regime = as.character(regime), key_backbone = as.character(kb))
}

#' The pinned checklist must be the same at both ends, or the difference is
#' partly taxonomy churn rather than data. run_timepoint.R records it; this
#' turns that record into a check.
reference_release <- function(tp) {
  # The live point has no reference_version.yml - run_timepoint.R writes those,
  # and the live pipeline is driven by targets. Its equivalent is the tracked
  # provenance file that 01b rewrites on every run, which is where
  # run_timepoint.R copies from in the first place.
  f <- if (tp_is_live(tp)) here("provenance", glue("upstream_versions_{COUNTRY_CODE}.yml"))
       else file.path(tp_root, tp, "reference_version.yml")
  if (!file.exists(f)) return(NA_character_)
  r <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
  if (is.null(r$taxonomy$published)) NA_character_ else as.character(r$taxonomy$published)
}

#' Reduce one cube to the closure grain
#'
#' One row per (specieskey, eeacellcode, datasetkey) with min_year, occurrences
#' and record count. Only five columns are read: the cube is ~42 M rows and the
#' other twelve are not needed here.
read_grain <- function(tp, res_km) {
  path <- file.path(tp_cubes_dir(tp), sprintf("cube_%dkm.parquet", res_km))
  if (!file.exists(path)) {
    cli_abort(c("Missing cube: {.path {path}}",
                "i" = "Run {.code scripts/04b_build_historical_cubes.R} first."))
  }
  dt <- data.table::as.data.table(arrow::read_parquet(
    path, col_select = c("specieskey", "eeacellcode", "datasetkey",
                         "year", "occurrences")))
  # The aggregation lives in R/closure.R so it can be unit-tested.
  g <- closure_build_grain(dt, SOURCE_TYPES)
  rm(dt); invisible(gc())
  untyped <- g[!datasetkey %chin% names(SOURCE_TYPES), .(occ = sum(occ)), by = datasetkey]
  if (nrow(untyped)) {
    data.table::setorder(untyped, -occ)
    cli_alert_warning(
      "{tp} {res_km} km: {nrow(untyped)} dataset{?s} not on the source-type list \\
       count as {.val other} ({scales::comma(sum(untyped$occ))} records): \\
       {.val {utils::head(untyped$datasetkey, 5)}}")
  }
  cli_alert_info(
    "{tp} {res_km} km: {scales::comma(nrow(g))} (species x cell x dataset) rows, \\
     {scales::comma(sum(g$occ))} occurrences"
  )
  g[]
}

#' specieskey -> Dyntaxa taxonID and the reconciled taxonomy, for one time point
read_match_table <- function(tp) {
  f <- file.path(tp_gaps_dir(tp), "taxonomic_match_table.csv")
  if (!file.exists(f)) {
    cli_abort(c("Missing match table for {tp}: {.path {f}}",
                "i" = "Run {.code Rscript run_timepoint.R {tp}} first (script 09a)."))
  }
  m <- data.table::fread(f, showProgress = FALSE)
  # Fail here, by name, rather than 20 minutes later inside a data.table join.
  # `class` and `order` drive the taxonomic panel; `backbone_taxonID` is the
  # entire Dyntaxa key space.
  need <- c("specieskey", "backbone_taxonID", "class", "order")
  absent <- setdiff(need, names(m))
  if (length(absent)) {
    cli_abort(c(
      "Match table for {tp} is missing column{?s}: {paste(absent, collapse = ', ')}",
      "i" = "Header found: {paste(names(m), collapse = ', ')}",
      "x" = "Script 09a's output shape has changed; closure cannot be computed \\
             against it without checking what moved."
    ))
  }
  m[, specieskey := as.character(specieskey)]
  if ("backbone_taxonID" %in% names(m)) m[, backbone_taxonID := as.character(backbone_taxonID)]
  m
}

#' GBIF's mapping from its former backbone to the Catalogue of Life, for some keys
#'
#' The mapping (one row per key of the former backbone, about 7.7 million rows,
#' 3.9 GB) is GBIF's own: https://download.checklistbank.org/col/gbif/README.html.
#' It is read as a stream and filtered to `keys`, so the whole file never sits
#' in memory. A synonym is replaced by its accepted COL taxon.
#'
#' @param path The mapping file (tab-separated, no quoting, header row).
#' @param keys Keys of the former backbone to look up.
#' @return data.table(legacy_key, col_key, col_name, match_type); a key GBIF
#'   could not map has NA col_key.
read_col_mapping <- function(path, keys) {
  if (!file.exists(path)) {
    cli_abort(c(
      "GBIF's mapping from its former backbone to the Catalogue of Life is missing: {.path {path}}",
      "i" = "Download it from {.url https://download.checklistbank.org/col/gbif/README.html} \\
             and set {.field parameters.closure.col_mapping_file} to where it is."))
  }
  hdr <- strsplit(readLines(path, n = 1L, encoding = "UTF-8"), "\t", fixed = TRUE)[[1]]
  need <- c("gbif:ID", "matchType", "col:ID", "col:scientificName", "col:acceptedID",
            "col:acceptedScientificName")
  absent <- setdiff(need, hdr)
  if (length(absent)) {
    cli_abort("{.path {path}} lacks column{?s} {.field {absent}}")
  }
  # Every column as text, the tab as the only delimiter: names hold quotes and
  # apostrophes that a quoting parser would read as field boundaries.
  sch <- arrow::schema(lapply(stats::setNames(hdr, hdr), function(x) arrow::utf8()))
  ds <- arrow::open_dataset(
    path, format = "csv", schema = sch,
    parse_options = arrow::CsvParseOptions$create(delimiter = "\t", quoting = FALSE),
    read_options = arrow::CsvReadOptions$create(skip_rows = 1L, column_names = hdr),
    convert_options = arrow::CsvConvertOptions$create(strings_can_be_null = TRUE))
  keys <- unique(as.character(keys))
  m <- data.table::as.data.table(
    dplyr::collect(dplyr::select(dplyr::filter(ds, .data[["gbif:ID"]] %in% keys),
                                 dplyr::all_of(need))))
  data.table::data.table(
    legacy_key = m[["gbif:ID"]],
    col_key    = data.table::fcoalesce(m[["col:acceptedID"]], m[["col:ID"]]),
    col_name   = data.table::fcoalesce(m[["col:acceptedScientificName"]], m[["col:scientificName"]]),
    match_type = m[["matchType"]])
}

#' Full cell universe for a resolution - from the grid, never from the data
grid_cellcodes <- function(res_km) {
  f <- here(p_data_proc, sprintf("cellcodes_%dkm.txt", res_km))
  if (!file.exists(f)) cli_abort(c("Missing {.path {f}}", "i" = "Run script 02."))
  unique(readLines(f, warn = FALSE))
}
