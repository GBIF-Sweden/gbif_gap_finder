# R/historic_io.R
# ============================================================================
# Reading GBIF Sweden's historical snapshot deliveries
# ============================================================================
# Purpose:
#   One place that knows how a historical delivery is physically laid out, so
#   scripts never have to.
#
# Delivery format:
#   - UTF-8, no BOM, no NUL bytes, LF line endings. decode_historic_delivery()
#     below also accepts UTF-16LE (BOM, CRLF) and passes a UTF-8 file straight
#     through.
#   - TWO files per snapshot, paired by date:
#       <prefix>_occurrences_aggregated_YYYYMMDD.tsv.gz   10 columns
#       <prefix>_taxonomy_YYYYMMDD.tsv.gz                  9 columns
#   - Grouped by `species_id` (the legacy GBIF nub species key), NOT `taxon_id`.
#     There is no taxon_id column and no sub-specific resolution to roll up.
#   - The taxonomy file is a clean 1:1 lookup on species_id and a deliberate
#     SUPERSET of the species in its occurrence file (no coordinate filter, no
#     dataset exclusion). Superset is the safe direction for a lookup; the join
#     therefore cannot fan out, and that is asserted below rather than assumed.
#   - Coordinates are 2 dp, and export size rules out 3 dp, so
#     this is permanent: ~4% of records land in a neighbouring 10 km cell
#     (~0.9% at 50 km). Carry it on every 10 km temporal figure.
#
# What this file deliberately does NOT do:
#   Look species up in the GBIF Backbone. The delivery carries its own
#   contemporaneous taxonomy, which is strictly better — no backbone version
#   matching, no roll-up, no API fallback, no "wrong build" failure mode.
#
# Dependencies: data.table, here, cli (all attached by R/packages.R).
#   NOT R.utils — see the gz section below. Deliberately kept optional so the
#   historical path adds nothing to renv.lock.
# ============================================================================

# ============================================================================
# Locations
# ============================================================================

#' Directory holding the raw historical deliveries
historic_raw_dir <- function() here(p_data_raw, "historic")

#' Directory for decoded / intermediate historical artefacts
historic_proc_dir <- function() {
  d <- here(p_data_proc, "historic")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Root directory for one time point of the series
#'
#' A time point is a DIRECTORY, not a column: everything time-varying in the
#' pipeline hangs off one path root, so a whole run can be redirected under
#' `proc/timepoints/{tp}/` without editing scripts 05-11 at all. 04b writes
#' straight into this layout, so a time-point run finds its cubes in place.
#'
#' `p_timepoint` in R/globals.R, driven by the GAP_FINDER_TIMEPOINT environment
#' variable, is that root for the run in progress. This helper is the canonical
#' way to NAME a time-point directory; it does not compete with that variable.
#'
#' @param tp     Time-point label, e.g. "2021-01-01".
#' @param create Create the directory if missing (default TRUE).
#' @return Absolute path.
timepoint_dir <- function(tp, create = TRUE) {
  if (!is.character(tp) || length(tp) != 1L || !nzchar(tp)) {
    cli_abort("timepoint_dir(): {.arg tp} must be a single non-empty string")
  }
  d <- here(p_data_proc, "timepoints", tp)
  if (isTRUE(create) && !dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  d
}

# ============================================================================
# The delivery's shape
# ============================================================================

#' Columns of the occurrence file, in delivered order
HISTORIC_OCC_COLS <- c("species_id", "basis_of_record", "dataset_id",
                       "publisher_id", "latitude", "longitude", "year",
                       "month", "snapshot", "occurrences")

#' Columns of the taxonomy file, in delivered order
HISTORIC_TAX_COLS <- c("snapshot", "species_id", "species", "kingdom",
                       "phylum", "class_rank", "order_rank", "family", "genus")

# The prefix is whatever GBIF chose ("sweden_"); only the suffix is contractual,
# so it is not hard-coded here and a re-branded delivery still matches.
HISTORIC_OCC_PATTERN <- "_occurrences_aggregated_([0-9]{8})\\.tsv(\\.gz)?$"
HISTORIC_TAX_PATTERN <- "_taxonomy_([0-9]{8})\\.tsv(\\.gz)?$"

#' YYYYMMDD -> the ISO label used everywhere else (and in the `snapshot` column)
historic_snapshot_label <- function(ymd) {
  sprintf("%s-%s-%s", substr(ymd, 1, 4), substr(ymd, 5, 6), substr(ymd, 7, 8))
}

#' Pair up the occurrence and taxonomy files of every snapshot in a directory
#'
#' Aborts on an unpaired file rather than skipping it. A missing taxonomy file
#' would otherwise surface much later as an empty join and a cube with no
#' species names at all, which 09a would then report as a taxonomic collapse.
#'
#' @param dir Directory to scan (default `historic_raw_dir()`).
#' @return data.table(snapshot, occ_path, tax_path), one row per snapshot,
#'   ordered by snapshot.
historic_snapshot_files <- function(dir = historic_raw_dir()) {
  if (!dir.exists(dir)) {
    cli_abort(c(
      "Historical delivery directory not found: {.path {dir}}",
      "i" = "Put GBIF Sweden's snapshot export(s) there."
    ))
  }
  files <- list.files(dir, full.names = TRUE)
  files <- files[!dir.exists(files)]

  occ <- files[grepl(HISTORIC_OCC_PATTERN, basename(files))]
  tax <- files[grepl(HISTORIC_TAX_PATTERN, basename(files))]

  stray <- setdiff(files, c(occ, tax))
  if (length(stray)) {
    cli_alert_warning(
      "Ignoring {length(stray)} unrecognised file{?s} in {.path {dir}}: \\
       {paste(basename(head(stray, 5)), collapse = ', ')}"
    )
  }
  if (length(occ) == 0L) {
    cli_abort(c(
      "No occurrence file matching {.val *_occurrences_aggregated_YYYYMMDD.tsv[.gz]} in {.path {dir}}",
      "i" = "Found: {paste(basename(files), collapse = ', ')}"
    ))
  }

  occ_dt <- data.table::data.table(
    snapshot = historic_snapshot_label(
      sub(paste0("^.*", HISTORIC_OCC_PATTERN), "\\1", basename(occ))),
    occ_path = occ
  )
  tax_dt <- data.table::data.table(
    snapshot = historic_snapshot_label(
      sub(paste0("^.*", HISTORIC_TAX_PATTERN), "\\1", basename(tax))),
    tax_path = tax
  )

  dup <- c(occ_dt$snapshot[duplicated(occ_dt$snapshot)],
           tax_dt$snapshot[duplicated(tax_dt$snapshot)])
  if (length(dup)) {
    cli_abort(c(
      "More than one file for snapshot{?s} {paste(unique(dup), collapse = ', ')}",
      "i" = "Keep exactly one occurrence file and one taxonomy file per date \\
             (archive the rest elsewhere)."
    ))
  }

  out <- merge(occ_dt, tax_dt, by = "snapshot", all = TRUE)
  bad <- out[is.na(occ_path) | is.na(tax_path)]
  if (nrow(bad)) {
    cli_abort(c(
      "Unpaired historical delivery file{?s} for snapshot{?s} \\
       {paste(bad$snapshot, collapse = ', ')}",
      "x" = "Every snapshot needs BOTH an occurrence file and a taxonomy file.",
      "i" = "Missing: {paste(ifelse(is.na(bad$occ_path), 'occurrences', 'taxonomy'), collapse = ', ')}"
    ))
  }

  data.table::setorder(out, snapshot)
  cli_alert_success(
    "Historical snapshots found: {paste(out$snapshot, collapse = ', ')}"
  )
  out[]
}

# ============================================================================
# Decoding (defensive — the current delivery needs none)
# ============================================================================

#' Decode a UTF-16 historical delivery to a plain ASCII/UTF-8 TSV
#'
#' A UTF-8 delivery is returned untouched. The case this exists for is UTF-16LE
#' with a BOM and CRLF line endings — a PowerShell `>` redirect artefact — which
#' `fread()` mis-parses and which `iconv -f UTF-16` turns into an EMPTY stream.
#' If an export ever arrives that way, this catches it at the door instead of
#' producing a cube built from mangled rows.
#'
#' Streams the file in chunks, deleting NUL and CR bytes. That is a valid
#' UTF-16LE -> ASCII decode ONLY while the payload is pure ASCII, so the
#' function verifies exactly that and aborts rather than silently mangling
#' anything else. If a byte > 0x7F ever appears, the delivery has real non-ASCII
#' content and must be converted with a real decoder:
#'   iconv -f UTF-16LE -t UTF-8 in.tsv > out.tsv          # note: -f UTF-16 FAILS
#'
#' @param path        Path to the delivered file (.gz or plain).
#' @param out_path    Destination; defaults to historic_proc_dir()/<name>.tsv
#' @param chunk_bytes Read granularity (default 64 MiB).
#' @param force       Re-decode even if an up-to-date output exists.
#' @return Path to a readable TSV: `path` itself when no decode was needed.
decode_historic_delivery <- function(path, out_path = NULL,
                                     chunk_bytes = 64L * 1024L^2,
                                     force = FALSE) {
  if (!file.exists(path)) cli_abort("Not found: {.path {path}}")

  gz <- grepl("\\.gz$", path)

  # --- Sniff the encoding from the first bytes -----------------------------
  con <- if (gz) gzfile(path, "rb") else file(path, "rb")
  head_raw <- readBin(con, "raw", n = 4L)
  close(con)

  is_utf16le <- length(head_raw) >= 2 &&
    head_raw[1] == as.raw(0xFF) && head_raw[2] == as.raw(0xFE)
  is_utf16be <- length(head_raw) >= 2 &&
    head_raw[1] == as.raw(0xFE) && head_raw[2] == as.raw(0xFF)

  if (is_utf16be) {
    cli_abort(c(
      "Delivery is UTF-16 BIG endian - this decoder handles little-endian only.",
      "i" = "Convert with: iconv -f UTF-16BE -t UTF-8 ..."
    ))
  }
  if (!is_utf16le) return(path)   # the normal path for the current delivery

  stem <- sub("\\.gz$", "", basename(path))
  stem <- sub("\\.(tsv|txt|csv)$", "", stem)
  if (is.null(out_path)) {
    out_path <- file.path(historic_proc_dir(), paste0(stem, ".tsv"))
  }
  if (!force && file.exists(out_path) && file.mtime(out_path) >= file.mtime(path)) {
    cli_alert_info(
      "Decoded delivery up to date: {.path {basename(out_path)}} \\
       ({round(file.size(out_path) / 1024^3, 2)} GB) - skipping"
    )
    return(out_path)
  }

  cli_alert_warning("Delivery is UTF-16LE - decoding to {.path {basename(out_path)}}")

  con_in  <- if (gz) gzfile(path, "rb") else file(path, "rb")
  con_out <- file(out_path, "wb")
  on.exit({ try(close(con_in), silent = TRUE); try(close(con_out), silent = TRUE) },
          add = TRUE)

  NUL <- as.raw(0x00); CR <- as.raw(0x0D); DEL <- as.raw(0x7F)
  first <- TRUE
  n_out <- 0

  repeat {
    buf <- readBin(con_in, "raw", n = chunk_bytes)
    if (length(buf) == 0L) break

    # Deleting NULs is chunk-boundary safe: we are removing bytes, not decoding
    # multi-byte sequences, so a code unit split across chunks is harmless.
    buf <- buf[buf != NUL & buf != CR]

    if (first && length(buf) > 0L) {
      # BOM survives the NUL strip as a lone 0xFF (0xFE is a separate byte).
      while (length(buf) > 0L && (buf[1] == as.raw(0xFF) || buf[1] == as.raw(0xFE))) {
        buf <- buf[-1]
      }
      first <- FALSE
    }

    if (length(buf) && any(buf > DEL)) {
      close(con_in); close(con_out); unlink(out_path)
      cli_abort(c(
        "Delivery contains non-ASCII bytes - NUL-stripping would corrupt it.",
        "i" = "Convert properly: {.code iconv -f UTF-16LE -t UTF-8 in.tsv > out.tsv}",
        "!" = "Note {.code -f UTF-16} (without LE) returns an empty stream on these files."
      ))
    }

    writeBin(buf, con_out)
    n_out <- n_out + length(buf)
  }

  close(con_in); close(con_out); on.exit(NULL)
  cli_alert_success(
    "Decoded: {.path {basename(out_path)}} ({round(n_out / 1024^3, 2)} GB)"
  )
  out_path
}

# ============================================================================
# Reading gzipped deliveries without R.utils
# ============================================================================
# `data.table::fread()` cannot open a .gz at all unless the optional R.utils
# package is installed — and even with it, fread's gz path DECOMPRESSES THE
# WHOLE FILE to a temp file before returning anything. That is not a detail:
# `fread(gz, nrows = 0L)` is O(file size), not O(1). On the 393 MB 2024
# delivery a header peek is therefore ~17 s and several GB of temp writes — and
# 04b peeks three times per snapshot (header check, snapshot check, real read).
#
# So: peek through a connection (O(1), no package), and do the one real read
# through the system decompressor (no package, no second copy on disk beyond
# fread's own temp file).
#
# There is deliberately NO R.utils fallback. A `requireNamespace("R.utils")`
# here is a real dependency as far as renv is concerned — it turns up in
# renv::status() as used-but-not-recorded — and it would only ever be reached on
# a machine with no gzip, zcat or gzcat, which is neither macOS nor rocker/r-ver.
# Paying a lockfile entry for an unreachable branch is the wrong trade.

#' First `n` lines of a possibly-gzipped text file, without decompressing it all
historic_readlines <- function(path, n) {
  con <- if (grepl("\\.gz$", path)) gzfile(path, "rt") else file(path, "rt")
  on.exit(close(con), add = TRUE)
  readLines(con, n = n, warn = FALSE)
}

#' Header field names of a possibly-gzipped TSV
historic_header <- function(path) {
  first <- historic_readlines(path, 1L)
  if (!length(first) || !nzchar(first[1])) {
    cli_abort("File is empty or has no header row: {.path {basename(path)}}")
  }
  strsplit(first[1], "\t", fixed = TRUE)[[1]]
}

#' fread() a possibly-gzipped file without requiring R.utils
#'
#' Note fread still stages the decompressed stream in `tempdir()`, so the
#' machine needs room for the uncompressed delivery (~4-6 GB per snapshot).
#' Pass `tmpdir =` through `...` to put that somewhere with space.
historic_fread <- function(path, ...) {
  if (!grepl("\\.gz$", path)) return(data.table::fread(path, ...))
  prog <- Sys.which(c("gzip", "zcat", "gzcat"))
  prog <- prog[nzchar(prog)]
  if (!length(prog)) {
    cli_abort(c(
      "Cannot read {.path {basename(path)}}: it is gzipped and there is no \\
       gzip, zcat or gzcat on PATH.",
      "i" = "Every platform this project runs on ships one (macOS, rocker/r-ver). \\
             Elsewhere, decompress the delivery first: {.code gunzip -k <file>.gz}"
    ))
  }
  flag <- if (names(prog)[1] == "gzip") "-dc" else ""
  data.table::fread(cmd = paste(shQuote(unname(prog[1])), flag, shQuote(path)), ...)
}

# ============================================================================
# Readers
# ============================================================================

#' Assert a file's header carries every expected column
#'
#' Shape is checked BEFORE the multi-gigabyte read, so a delivery with a changed
#' column set costs milliseconds rather than twenty minutes and a wrong cube.
historic_check_header <- function(path, expected, what) {
  hdr <- historic_header(path)
  missing <- setdiff(expected, hdr)
  if (length(missing)) {
    cli_abort(c(
      "Historical {what} file is missing column{?s}: {paste(missing, collapse = ', ')}",
      "i" = "Header found: {paste(hdr, collapse = ', ')}",
      "x" = "Refusing to build a cube from a delivery with a different shape."
    ))
  }
  extra <- setdiff(hdr, expected)
  if (length(extra)) {
    cli_alert_info("Extra column{?s} in {what}: {paste(extra, collapse = ', ')}")
  }
  invisible(hdr)
}

#' Read one snapshot's occurrence table
#'
#' `snapshot` is validated from a 1,000-row peek and then NOT read in full: it is
#' constant within a file and 8 bytes x 65.7 M rows of a constant is half a
#' gigabyte of nothing.
#'
#' @param path     Path to the occurrence file (.tsv or .tsv.gz).
#' @param expect   Expected snapshot label, e.g. "2021-01-01"; NULL to skip.
#' @param select   Columns to read (default: all but `snapshot`).
#' @return data.table
read_historic_occurrences <- function(path, expect = NULL, select = NULL) {
  if (!file.exists(path)) cli_abort("Not found: {.path {path}}")
  path <- decode_historic_delivery(path)
  historic_check_header(path, HISTORIC_OCC_COLS, "occurrence")

  if (!is.null(expect)) {
    # Connection read, not fread: see the note above historic_readlines().
    hdr   <- historic_header(path)
    icol  <- match("snapshot", hdr)
    lines <- historic_readlines(path, 1001L)[-1]
    got   <- unique(vapply(strsplit(lines, "\t", fixed = TRUE),
                           function(x) if (length(x) >= icol) x[icol] else NA_character_,
                           character(1)))
    if (!identical(got, expect)) {
      cli_abort(c(
        "Snapshot column disagrees with the filename in {.path {basename(path)}}",
        "x" = "Filename says {.val {expect}}; first rows say \\
               {.val {paste(got, collapse = ', ')}}.",
        "i" = "The filename date is what names the time-point directory, so these \\
               must agree or the series is silently mislabelled."
      ))
    }
  }

  if (is.null(select)) select <- setdiff(HISTORIC_OCC_COLS, "snapshot")
  classes <- list(
    integer   = intersect(c("species_id", "year", "month", "occurrences"), select),
    numeric   = intersect(c("latitude", "longitude"), select),
    character = intersect(c("basis_of_record", "dataset_id", "publisher_id",
                            "snapshot"), select)
  )
  classes <- classes[lengths(classes) > 0L]

  dt <- historic_fread(path, sep = "\t", select = select,
                       colClasses = classes, showProgress = TRUE)

  # species_id is the join key for the whole taxonomy step. An NA here means
  # either a blank field or an integer overflow, and both would silently drop
  # rows at the join instead of failing.
  if ("species_id" %in% names(dt) && anyNA(dt$species_id)) {
    cli_abort(c(
      "{scales::comma(sum(is.na(dt$species_id)))} row{?s} have a missing or \\
       unparseable {.field species_id} in {.path {basename(path)}}",
      "i" = "These would vanish at the taxonomy join rather than error."
    ))
  }
  cli_alert_success(
    "Occurrences: {scales::comma(nrow(dt))} rows, \\
     {scales::comma(sum(as.numeric(dt$occurrences), na.rm = TRUE))} occurrences"
  )
  dt[]
}

#' Read one snapshot's taxonomy lookup
#'
#' Asserts the 1:1 property the join depends on. The taxonomy file is a superset
#' of the occurrence file's species, which is the safe direction; a DUPLICATE
#' species_id is the dangerous one, because `lookup[occurrences]` would fan the
#' occurrence table out and inflate every count downstream without any error.
#'
#' @param path   Path to the taxonomy file (.tsv or .tsv.gz).
#' @param expect Expected snapshot label; NULL to skip.
#' @return data.table keyed on species_id.
read_historic_taxonomy <- function(path, expect = NULL) {
  if (!file.exists(path)) cli_abort("Not found: {.path {path}}")
  path <- decode_historic_delivery(path)
  historic_check_header(path, HISTORIC_TAX_COLS, "taxonomy")

  tax <- historic_fread(
    path, sep = "\t", select = HISTORIC_TAX_COLS,
    colClasses = list(integer = "species_id",
                      character = setdiff(HISTORIC_TAX_COLS, "species_id")),
    showProgress = FALSE
  )

  if (anyNA(tax$species_id)) {
    cli_abort("Taxonomy lookup has {sum(is.na(tax$species_id))} row{?s} with no \\
               {.field species_id}: {.path {basename(path)}}")
  }
  n_dup <- nrow(tax) - data.table::uniqueN(tax$species_id)
  if (n_dup > 0L) {
    cli_abort(c(
      "Taxonomy lookup is not 1:1: {scales::comma(n_dup)} duplicate \\
       {.field species_id} value{?s} in {.path {basename(path)}}",
      "x" = "Joining this would fan the occurrence table out and inflate every \\
             count downstream, silently.",
      "i" = "Ask GBIF for a de-duplicated lookup, or resolve the duplicates here \\
             deliberately."
    ))
  }
  if (!is.null(expect)) {
    got <- unique(as.character(tax$snapshot))
    if (!identical(got, expect)) {
      cli_abort(c(
        "Snapshot column disagrees with the filename in {.path {basename(path)}}",
        "x" = "Filename says {.val {expect}}; file says \\
               {.val {paste(got, collapse = ', ')}}."
      ))
    }
  }

  tax[, snapshot := NULL]
  data.table::setkey(tax, species_id)
  cli_alert_success(
    "Taxonomy: {scales::comma(nrow(tax))} species (1:1 on species_id)"
  )
  tax[]
}
