#!/usr/bin/env Rscript
# Weekly status of the GBIF Gap Finder: the outside services that the pipeline and
# the app depend on, and the page of the live app.
#
#   Rscript .github/scripts/weekly_status.R        # from the repository root
#
# One line per check:
#   Live site           LIVE_URL answers with the Gap Finder; its version against
#                       the newest release tag; the date of its data
#   NRM coordinates     georeferenced records of the NRM bird and mammal collections
#                       on GBIF; the data refresh waits while one of them has none
#   Upstream releases   the five checklists of provenance/upstream_versions_<CC>.yml
#                       against GBIF; a newer release goes into the next refresh
#   and the network checks of scripts/00_preflight.R: GBIF API, Catalogue of Life,
#   Tier 4, Tier 5, checklist archives, basemap, action runtimes.
#
# Each line is OK, WARN (worth knowing) or FAIL (broken). FAIL is kept for what
# breaks the live app or the next refresh: the preflight's own failures (the GBIF
# API, Tier 4, Tier 5) and a live app that does not answer or is not the Gap
# Finder. When a check fails, every check runs again after STATUS_RETRY_WAIT
# seconds and only the second result counts, so a passing blip does not turn the
# run red. The exit status is 1 if a FAIL remains.
#
# On GitHub (weekly-status.yml) each line becomes an annotation, titled by the
# check: a notice (OK), a warning (WARN) or an error (FAIL). The monthly health
# check reads these annotations. A table goes on the run's summary page, and
# `live=<answers|down|unreachable|unknown>` goes to GITHUB_OUTPUT, so the workflow
# opens the live app's tabs only when its page answers.
#
# Environment:
#   LIVE_URL           the live app; default https://gbif.se/gap-finder/
#   NEWEST_TAG         the newest release tag, and NEWEST_TAG_DATE its date
#                      (YYYY-MM-DD); read from git when unset
#   STATUS_RETRY_WAIT  seconds before the second try; default 300
#   GBIF_GAP_COUNTRY   country config, as in the pipeline; default SE
#
# Needs httr, jsonlite and yaml, and what scripts/00_setup.R loads: the packages
# of renv.lock. weekly-status.yml runs it in the dependency image.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(yaml)
})

if (!file.exists(file.path("scripts", "00_preflight.R"))) {
  stop("Run this from the repository root.", call. = FALSE)
}

GBIF_V1     <- "https://api.gbif.org/v1"
LIVE_URL    <- Sys.getenv("LIVE_URL", "https://gbif.se/gap-finder/")
RETRY_WAIT  <- as.numeric(Sys.getenv("STATUS_RETRY_WAIT", "300"))
COUNTRY     <- Sys.getenv("GBIF_GAP_COUNTRY", "SE")
ON_GITHUB   <- identical(Sys.getenv("GITHUB_ACTIONS"), "true")
UA          <- httr::user_agent(
  "gbif-gap-finder weekly status (https://github.com/GBIF-Sweden/gbif_gap_finder)")
# Older than this, the data of the live app counts as stale (as in the monthly
# health check).
DATA_MAX_AGE_DAYS <- 120

# Datasets whose georeferenced records the data refresh needs. While one of them
# has none on GBIF, a new cube would leave that collection out, so the refresh
# waits.
WATCHED_DATASETS <- c(
  "NRM Ornithology" = "87ed95eb-4ead-465e-9c3a-7d9e9be4035d",
  "NRM Mammals"     = "f7b77040-0c64-11dd-84d1-b8a03c50a862"
)

# Sections of the provenance file, by the name used in the report.
UPSTREAM <- c(col_backbone = "Catalogue of Life", taxonomy = "Dyntaxa",
              redlist = "red list", invasives = "GRIIS",
              sensitive = "sensitive list")

# Annotation titles. The monthly health check finds the lines by these titles.
TITLES <- c(live_site = "Live site", nrm = "NRM coordinates",
            upstream = "Upstream releases", gbif_api = "GBIF API",
            col_checklist = "Catalogue of Life", tier4_roundtrip = "Tier 4",
            species_match_v2 = "Tier 5", checklist_archives = "Checklist archives",
            basemap = "Basemap", ci_actions = "Action runtimes",
            preflight = "Preflight")

# Covered by other lines when they are fine: the API by Tier 4 and Tier 5, the
# Catalogue of Life by Upstream releases. They are annotated only when not OK.
QUIET_WHEN_OK <- c("gbif_api", "col_checklist")

# --- helpers ------------------------------------------------------------------

# As in R/globals.R, which the preflight sources after this.
`%||%` <- function(a, b) if (!is.null(a)) a else b

result <- function(check, status, detail) {
  data.frame(check = check, status = status, detail = detail,
             stringsAsFactors = FALSE)
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

as_day <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1])) return(as.Date(NA))
  suppressWarnings(as.Date(substr(as.character(x[1]), 1, 10)))
}

# The first line of an error message, without curl's long tail.
short <- function(msg) substr(strsplit(msg, "\n", fixed = TRUE)[[1]][1], 1, 160)

# GET a GBIF API URL. list(ok = TRUE, body) or list(ok = FALSE, why).
get_json <- function(url) {
  r <- tryCatch(httr::GET(url, UA, httr::timeout(60)), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, why = short(conditionMessage(r))))
  if (httr::status_code(r) != 200L) {
    return(list(ok = FALSE, why = sprintf("HTTP %d", httr::status_code(r))))
  }
  body <- tryCatch(
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = TRUE),
    error = function(e) NULL)
  if (is.null(body)) return(list(ok = FALSE, why = "the answer is not JSON"))
  list(ok = TRUE, body = body)
}

first_match <- function(text, pattern) {
  m <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(m) < 2L) NA_character_ else trimws(m[2])
}

# A check that stops with an error reports that as WARN: a broken check must not
# look like a broken service, and a WARN still shows in the monthly report.
safely <- function(check, fn) {
  tryCatch(fn(), error = function(e) {
    list(row = result(check, "WARN", paste("the check could not run:", short(conditionMessage(e)))),
         live = "unknown")
  })
}

# --- newest release ---------------------------------------------------------------

newest_release <- function() {
  tag  <- Sys.getenv("NEWEST_TAG", "")
  date <- Sys.getenv("NEWEST_TAG_DATE", "")
  if (!nzchar(tag) && nzchar(Sys.which("git"))) {
    tags <- suppressWarnings(tryCatch(
      system2("git", c("tag", "--list", "v*", "--sort=-v:refname"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()))
    if (length(tags) && nzchar(tags[1])) {
      tag <- tags[1]
      date <- suppressWarnings(tryCatch(
        system2("git", c("log", "-1", "--format=%cs", tag), stdout = TRUE, stderr = FALSE)[1],
        error = function(e) ""))
    }
  }
  list(tag = tag, date = as_day(date))
}

version_of <- function(x) {
  v <- sub("^v", "", x)
  if (grepl("^[0-9]+(\\.[0-9]+)*$", v)) package_version(v) else NULL
}

# --- checks -------------------------------------------------------------------------

check_live <- function(release) {
  t0 <- Sys.time()
  r <- tryCatch(httr::GET(LIVE_URL, UA, httr::timeout(90)), error = function(e) e)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  if (inherits(r, "error")) {
    # No answer at all. If the host answers, the app is down; if the host does
    # not answer either, this machine cannot reach it, or the whole site is down.
    host <- sub("^(https?://[^/]+).*$", "\\1/", LIVE_URL)
    h <- tryCatch(httr::HEAD(host, UA, httr::timeout(30)), error = function(e) e)
    if (inherits(h, "error")) {
      return(list(row = result("live_site", "WARN", sprintf(
        "%s does not answer from here (%s), and neither does %s: either this machine cannot reach the site, or the whole site is down",
        LIVE_URL, short(conditionMessage(r)), host)), live = "unreachable"))
    }
    return(list(row = result("live_site", "FAIL", sprintf(
      "%s does not answer (%s), while %s does (HTTP %d)",
      LIVE_URL, short(conditionMessage(r)), host, httr::status_code(h))), live = "down"))
  }

  code <- httr::status_code(r)
  if (code != 200L) {
    return(list(row = result("live_site", "FAIL", sprintf(
      "%s answers HTTP %d", LIVE_URL, code)), live = "down"))
  }

  html    <- httr::content(r, "text", encoding = "UTF-8")
  title   <- first_match(html, "<title>([^<]*)</title>")
  version <- first_match(html, "App version\\s*<strong>([^<]+)</strong>")
  data_on <- as_day(first_match(html, "Data as of\\s*<strong>([0-9]{4}-[0-9]{2}-[0-9]{2})</strong>"))
  if (is.na(title) || !startsWith(title, "GBIF Gap Finder") || is.na(version)) {
    return(list(row = result("live_site", "FAIL", sprintf(
      "%s answers, but not with the Gap Finder (title: %s)", LIVE_URL,
      if (is.na(title)) "none" else sprintf("\"%s\"", title))), live = "down"))
  }

  status <- "OK"
  notes <- character()
  live_v <- version_of(version)
  new_v  <- version_of(release$tag)
  shown  <- version
  if (is.null(live_v)) {
    status <- "WARN"
    notes <- c(notes, sprintf("version \"%s\" is not a release", version))
  } else if (!is.null(new_v) && live_v < new_v) {
    status <- "WARN"
    notes <- c(notes, sprintf("the newest release, %s%s, is not deployed yet", release$tag,
                              if (is.na(release$date)) "" else sprintf(" (tagged %s)", format(release$date))))
  } else if (!is.null(new_v) && live_v == new_v) {
    shown <- paste(version, "(the newest release)")
  }
  age <- if (is.na(data_on)) NA_integer_ else as.integer(Sys.Date() - data_on)
  detail <- sprintf("%s answers in %.1f s: %s, data as of %s%s", LIVE_URL, secs, shown,
                    if (is.na(data_on)) "unknown" else format(data_on),
                    if (is.na(age)) "" else sprintf(" (%d days)", age))
  if (!is.na(age) && age > DATA_MAX_AGE_DAYS) {
    status <- "WARN"
    notes <- c(notes, sprintf("the data are more than %d days old", DATA_MAX_AGE_DAYS))
  }
  if (length(notes)) detail <- paste0(detail, "; ", paste(notes, collapse = "; "))
  list(row = result("live_site", status, detail), live = "answers")
}

check_nrm <- function() {
  counts <- character(); none <- character(); unread <- character()
  for (name in names(WATCHED_DATASETS)) {
    key <- WATCHED_DATASETS[[name]]
    all <- get_json(sprintf("%s/occurrence/search?datasetKey=%s&limit=0", GBIF_V1, key))
    geo <- get_json(sprintf("%s/occurrence/search?datasetKey=%s&hasCoordinate=true&limit=0",
                            GBIF_V1, key))
    if (!all$ok || !geo$ok || is.null(all$body$count) || is.null(geo$body$count)) {
      unread <- c(unread, sprintf("%s (%s)", name,
                                  if (!all$ok) all$why else if (!geo$ok) geo$why else "no count"))
      next
    }
    counts <- c(counts, sprintf("%s %s of %s", name, fmt_n(geo$body$count), fmt_n(all$body$count)))
    if (geo$body$count == 0) none <- c(none, name)
  }
  parts <- c(
    if (length(counts)) paste0("Georeferenced records on GBIF: ", paste(counts, collapse = "; ")),
    if (length(none)) paste0("The data refresh waits until ", paste(none, collapse = " and "),
                             if (length(none) == 1L) " has" else " have", " coordinates again"),
    if (length(unread)) paste0("Not read: ", paste(unread, collapse = "; "))
  )
  list(row = result("nrm", if (length(none) || length(unread)) "WARN" else "OK",
                    paste(parts, collapse = ". ")))
}

check_upstream <- function() {
  path <- file.path("provenance", sprintf("upstream_versions_%s.yml", COUNTRY))
  if (!file.exists(path)) return(list(row = result("upstream", "WARN", paste(path, "not found"))))
  prov <- yaml::read_yaml(path)
  newer <- character(); same <- character(); unread <- character()
  for (section in names(UPSTREAM)) {
    p <- prov[[section]]
    if (is.null(p) || is.null(p$dataset_key)) next
    name <- UPSTREAM[[section]]
    d <- get_json(sprintf("%s/dataset/%s", GBIF_V1, p$dataset_key))
    if (!d$ok) {
      unread <- c(unread, sprintf("%s (%s)", name, d$why))
      next
    }
    on_gbif <- as_day(d$body$pubDate)
    ours    <- as_day(p$published)
    if (!is.na(on_gbif) && !is.na(ours) && on_gbif > ours) {
      newer <- c(newer, sprintf("%s published %s (provenance %s)", name, format(on_gbif), format(ours)))
    } else {
      same <- c(same, name)
    }
  }
  parts <- c(
    if (length(newer)) paste0("Newer on GBIF, for the next refresh: ", paste(newer, collapse = "; ")),
    if (length(same)) paste0("As in provenance/: ", paste(same, collapse = ", ")),
    if (length(unread)) paste0("Not read: ", paste(unread, collapse = "; "))
  )
  list(row = result("upstream", if (length(unread)) "WARN" else "OK", paste(parts, collapse = ". ")))
}

# The network checks of the preflight, as the pipeline runs them. It stops with an
# error when one of them fails; its report is complete by then.
check_preflight <- function() {
  env <- new.env(parent = globalenv())
  env$preflight_network_only <- TRUE
  report <- tryCatch({
    source(file.path("scripts", "00_preflight.R"), local = env)
    env$pf_report
  }, error = function(e) {
    if (is.data.frame(env$pf_report)) env$pf_report
    else result("preflight", "FAIL", paste("the preflight did not run:", short(conditionMessage(e))))
  })
  list(row = report[, c("check", "status", "detail")])
}

run_all <- function(release) {
  live <- safely("live_site", function() check_live(release))
  rows <- rbind(
    live$row,
    safely("nrm", check_nrm)$row,
    safely("upstream", check_upstream)$row,
    safely("preflight", check_preflight)$row
  )
  list(rows = rows, live = live$live %||% "unknown")
}

# --- run ------------------------------------------------------------------------------

release <- newest_release()
res <- run_all(release)
retried <- character()
if (any(res$rows$status == "FAIL") && RETRY_WAIT > 0) {
  retried <- res$rows$check[res$rows$status == "FAIL"]
  message(sprintf("Failed: %s. All checks run again in %d s.",
                  paste(retried, collapse = ", "), as.integer(RETRY_WAIT)))
  Sys.sleep(RETRY_WAIT)
  res <- run_all(release)
}
rows <- res$rows
rows$title <- ifelse(rows$check %in% names(TITLES), TITLES[rows$check], rows$check)

n_fail <- sum(rows$status == "FAIL")
n_warn <- sum(rows$status == "WARN")
plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")
verdict <- if (n_fail) {
  sprintf("%d failed, %s", n_fail, plural(n_warn, "warning"))
} else if (n_warn) {
  sprintf("no failure, %s", plural(n_warn, "warning"))
} else {
  "all OK"
}
retry_note <- ""
if (length(retried)) {
  retry_note <- sprintf(
    "Checked twice: %s failed at first, so every check ran again after %d s. These are the second results.",
    paste(ifelse(retried %in% names(TITLES), TITLES[retried], retried), collapse = ", "),
    as.integer(RETRY_WAIT))
}

for (i in seq_len(nrow(rows))) {
  cat(sprintf("%-4s  %-18s %s\n", rows$status[i], rows$title[i], rows$detail[i]))
}

if (ON_GITHUB) {
  # Annotations: %, CR and LF are escaped as the workflow-command syntax asks.
  esc <- function(x) gsub("\n", "%0A", gsub("\r", "%0D", gsub("%", "%25", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  level <- c(OK = "notice", WARN = "warning", FAIL = "error")
  show <- rows[!(rows$status == "OK" & rows$check %in% QUIET_WHEN_OK), ]
  # GitHub keeps at most 10 annotations of each kind per step; the rest of a kind
  # are joined into its tenth.
  for (lv in names(level)) {
    s <- show[show$status == lv, ]
    if (nrow(s) > 10L) {
      rest <- s[10:nrow(s), ]
      s <- s[1:9, ]
      s <- rbind(s, data.frame(check = "more", status = lv,
                               detail = paste(sprintf("%s: %s", rest$title, rest$detail), collapse = " | "),
                               title = "More checks", stringsAsFactors = FALSE))
    }
    for (i in seq_len(nrow(s))) {
      cat(sprintf("::%s title=%s::%s\n", level[[lv]], s$title[i], esc(s$detail[i])))
    }
  }
}
cat(sprintf("Weekly status: %s\n", verdict))
if (nzchar(retry_note)) cat(retry_note, "\n", sep = "")

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(summary_file)) {
  cell <- function(x) gsub("|", "\\|", gsub("\n", " ", x, fixed = TRUE), fixed = TRUE)
  word <- c(OK = "OK", WARN = "Warning", FAIL = "Failed")
  lines <- c(
    sprintf("### Weekly status: %s", verdict),
    "",
    "| Check | Result | Detail |",
    "|---|---|---|",
    sprintf("| %s | %s | %s |", rows$title, word[rows$status], cell(rows$detail)),
    "",
    sprintf("Checked %s UTC with %s. Newest release: %s.",
            format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"), R.version.string,
            if (nzchar(release$tag)) release$tag else "unknown"),
    if (nzchar(retry_note)) c("", retry_note),
    ""
  )
  cat(lines, file = summary_file, sep = "\n", append = TRUE)
}

output_file <- Sys.getenv("GITHUB_OUTPUT", "")
if (nzchar(output_file)) cat(sprintf("live=%s\n", res$live), file = output_file, append = TRUE)

quit(save = "no", status = if (n_fail) 1L else 0L)
