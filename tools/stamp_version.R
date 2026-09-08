#!/usr/bin/env Rscript
# tools/stamp_version.R
# ============================================================================
# Stamp the release version into every surface that carries one
# ============================================================================
# Why this exists:
#   Six places carried a hand-typed version string. By 2026-09 four of them said
#   0.4.3 while the released tag was v0.5.1 — two releases of drift, because
#   every one had to be remembered separately.
#
# The rule: ONE source of truth (the git tag), everything else derived.
#
#   git describe --tags --always --dirty   ->  CITATION.cff        (version, date-released)
#                                          ->  ROADMAP.Rmd         (**Version:** line)
#                                          ->  docs/user_manual.md (footer line)
#
#   The app does NOT read a stamped constant: it reads GAP_FINDER_VERSION from
#   the environment, baked into the image at build time (Dockerfile ARG/ENV,
#   supplied by CI from the tag). A container has no git, and a stamped constant
#   would be one more thing to forget.
#
#   The DATA snapshot date is deliberately NOT handled here. Code version and
#   data date are different facts: see globals::get_snapshot_date() and
#   R/report_helpers.R. Conflating them is how "Data last updated" ended up
#   showing when the RDS was packaged.
#
# Usage:
#   Rscript tools/stamp_version.R            # write the derived version everywhere
#   Rscript tools/stamp_version.R --check    # verify only; exit 1 on any drift (CI)
#   Rscript tools/stamp_version.R --version 0.6.0   # override the derived version
#
# This is NOT a pipeline step and is deliberately not a targets target: it
# rewrites tracked files, which would leave the working tree dirty after every
# tar_make(). Run it at release time, or let --check catch drift in CI.
# ============================================================================

args     <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
explicit   <- if ("--version" %in% args) args[which(args == "--version") + 1L] else NULL

root <- tryCatch(
  if (requireNamespace("here", quietly = TRUE)) here::here() else getwd(),
  error = function(e) getwd()
)

# ---------------------------------------------------------------- version ----
derive_version <- function() {
  if (!is.null(explicit) && nzchar(explicit)) return(sub("^v", "", explicit))
  out <- suppressWarnings(system2(
    "git", c("-C", shQuote(root), "describe", "--tags", "--always", "--dirty"),
    stdout = TRUE, stderr = FALSE
  ))
  if (!length(out) || !nzchar(out[1])) {
    stop("Cannot derive a version: no git tag reachable and no --version given.",
         call. = FALSE)
  }
  sub("^v", "", out[1])
}

version <- derive_version()
today   <- format(Sys.Date(), "%Y-%m-%d")

if (grepl("dirty$", version)) {
  message("! Working tree is dirty — version derived as '", version, "'.")
  if (!check_only) {
    message("  Stamping a -dirty version into released files is almost never what you want.")
    message("  Commit first, or pass --version explicitly.")
  }
}

# ------------------------------------------------------------------ edits ----
# Each entry: file, a regex identifying the line, and the replacement built from
# `version`. Anchored to line starts so a passing mention in prose is never hit.
edits <- list(
  list(
    file    = file.path(root, "CITATION.cff"),
    pattern = "^version:.*$",
    replace = sprintf('version: "%s"', version)
  ),
  list(
    file    = file.path(root, "CITATION.cff"),
    pattern = "^date-released:.*$",
    replace = sprintf('date-released: "%s"', today)
  ),
  list(
    file    = file.path(root, "ROADMAP.Rmd"),
    pattern = "^- \\*\\*Version:\\*\\* v[0-9][^.]*\\.[^.]*\\.[^ .]*\\.",
    replace = sprintf("- **Version:** v%s.", version)
  ),
  list(
    file    = file.path(root, "docs", "user_manual.md"),
    pattern = "^> Version [0-9][^ ]* \\([^)]*\\)\\.",
    replace = sprintf("> Version %s (%s).", version, today)
  )
)

drift <- character()
wrote <- character()

for (e in edits) {
  if (!file.exists(e$file)) {
    message("- skipped (not found): ", basename(e$file))
    next
  }
  lines <- readLines(e$file, warn = FALSE)
  hit   <- grep(e$pattern, lines)

  if (!length(hit)) {
    drift <- c(drift, sprintf("%s: no line matching /%s/ — the file changed shape",
                              basename(e$file), e$pattern))
    next
  }
  if (length(hit) > 1L) {
    drift <- c(drift, sprintf("%s: %d lines match /%s/ — refusing to guess",
                              basename(e$file), length(hit), e$pattern))
    next
  }

  old <- lines[hit]
  new <- sub(e$pattern, e$replace, old)

  if (identical(old, new)) {
    message("  ok  ", basename(e$file), ": ", trimws(old))
    next
  }

  if (check_only) {
    drift <- c(drift, sprintf("%s\n      is: %s\n      want: %s",
                              basename(e$file), trimws(old), trimws(new)))
  } else {
    lines[hit] <- new
    writeLines(lines, e$file)
    wrote <- c(wrote, sprintf("%s: %s", basename(e$file), trimws(new)))
    message("  set ", basename(e$file), ": ", trimws(new))
  }
}

# ----------------------------------------------------------------- report ----
cat("\n")
if (check_only) {
  if (length(drift)) {
    cat(sprintf("Version drift against %s:\n\n", version))
    for (d in drift) cat("  - ", d, "\n", sep = "")
    cat("\nRun: Rscript tools/stamp_version.R\n")
    quit(status = 1L)
  }
  cat(sprintf("All version surfaces agree with %s\n", version))
} else {
  if (length(drift)) {
    cat("Could not stamp every surface:\n")
    for (d in drift) cat("  - ", d, "\n", sep = "")
    quit(status = 1L)
  }
  if (length(wrote)) {
    cat(sprintf("Stamped %s into %d file(s). Commit the result.\n",
                version, length(wrote)))
  } else {
    cat(sprintf("Everything already at %s — nothing to do.\n", version))
  }
}
