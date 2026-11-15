#!/usr/bin/env bash
# Starts an app image and checks that the app works in it:
#   1. the container stays up and its page answers (HTTP 200, the app's title);
#   2. every tab opens in a headless browser without an error and without a
#      plot, map or table that stays empty (check_app_tabs.py);
#   3. the container log has no R error.
# Prints what it finds and exits with status 1 if a check fails.
#
#   bash .github/scripts/check_app_image.sh <image>
#
# Used by build-deps-image.yml (job `app`). There the problems become error
# annotations, and the result and the end of the container log go on the run's
# summary page. It runs on a laptop with Docker too:
#   docker build -f shiny_app/gap_finder/Dockerfile.gap_finder -t gap-finder:check .
#   python3 -m pip install -r .github/scripts/requirements.txt
#   bash .github/scripts/check_app_image.sh gap-finder:check
#
# Environment:
#   CHECK_PORT            port on this machine that the app is published on; default 3838
#   CHECK_START_TIMEOUT   seconds the app may take until its page answers; default 300
#   CHECK_BROWSER_CHANNEL, CHECK_TAB_TIMEOUT   see check_app_tabs.py

# No `set -e`: a failed step is a finding to report, and the checks after it
# still say something.
set -uo pipefail

image=${1:?usage: check_app_image.sh <image>}
here=$(cd "$(dirname "$0")" && pwd)
port=${CHECK_PORT:-3838}
start_timeout=${CHECK_START_TIMEOUT:-300}
url="http://127.0.0.1:${port}/"
name="gap-finder-check-$$"
work=$(mktemp -d)
trap 'docker rm -f "$name" > /dev/null 2>&1; rm -rf "$work"' EXIT

problems=0
: > "$work/problems.md"
problem() {
  problems=$((problems + 1))
  echo "- $*" >> "$work/problems.md"
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    echo "::error title=App image check::$*"
  else
    echo "PROBLEM: $*"
  fi
}

# --- 1. Start the container and wait for the page ----------------------------
echo "Starting $image"
page_line="not reached"
tabs_line="not run"
answered=no
created=no
if ! docker run -d --name "$name" -p "127.0.0.1:${port}:3838" "$image" > "$work/run.out" 2>&1; then
  problem "the container could not be started: $(grep -v "^Run 'docker" "$work/run.out" | grep . | tail -n 1)"
else
  created=yes
  t0=$(date +%s)
  while :; do
    code=$(curl -s -o "$work/page.html" -w '%{http_code}' --noproxy '*' --max-time 10 "$url" 2> /dev/null) || code=000
    if [ "$code" = 200 ]; then
      answered=yes
      break
    fi
    if [ "$(docker inspect -f '{{.State.Running}}' "$name" 2> /dev/null)" != true ]; then
      problem "the container stopped before the page answered (exit code $(docker inspect -f '{{.State.ExitCode}}' "$name" 2> /dev/null))"
      break
    fi
    if [ $(($(date +%s) - t0)) -ge "$start_timeout" ]; then
      problem "the page did not answer within ${start_timeout} s (last HTTP status: ${code})"
      break
    fi
    sleep 3
  done
fi

# --- 2. The page is the app, and its tabs render ------------------------------
if [ "$answered" = yes ]; then
  started=$(($(date +%s) - t0))
  title=$(grep -o '<title>[^<]*</title>' "$work/page.html" | head -n 1 | sed -e 's/<[^>]*>//g')
  page_line="HTTP 200 after ${started} s, title \"${title}\""
  echo "The page answers after ${started} s; title: ${title}"
  case "$title" in
    "GBIF Gap Finder"*) ;;
    *) problem "the page is not the app: its title is \"${title}\"" ;;
  esac

  # The script prints one line per tab, and "PROBLEM: ..." for what failed.
  python3 "$here/check_app_tabs.py" "$url" > "$work/tabs.out" 2>&1
  tabs_status=$?
  grep -v '^PROBLEM: ' "$work/tabs.out"
  while IFS= read -r line; do
    problem "${line#PROBLEM: }"
  done < <(grep '^PROBLEM: ' "$work/tabs.out")
  if [ "$tabs_status" -ne 0 ] && ! grep -q '^PROBLEM: ' "$work/tabs.out"; then
    problem "the tab check did not run: $(tail -n 1 "$work/tabs.out")"
  fi
  tabs_line=$(grep -E '^[0-9]+ tabs opened' "$work/tabs.out" | tail -n 1)
  tabs_line=${tabs_line:-did not finish}

  # R can stop without writing an error, for example when a library crashes.
  if [ "$(docker inspect -f '{{.State.Running}}' "$name" 2> /dev/null)" != true ]; then
    problem "the container stopped while the tabs were opened (exit code $(docker inspect -f '{{.State.ExitCode}}' "$name" 2> /dev/null))"
  fi
fi

# --- 3. The container log ------------------------------------------------------
# R writes "Error in f(x) : ..." or "Error: ..." and, when it gives up,
# "Execution halted". Shiny writes "Warning: Error in <output>: ..." for an
# output that failed. A message that R puts on the next line is joined to its
# first line. Warnings are listed and do not fail the check.
: > "$work/container.log"
if [ "$created" = yes ]; then
  docker logs "$name" > "$work/container.log" 2>&1
fi
awk '/^(Warning: )?Error( in |:| :)|^Execution halted|caught segfault/ {
       line = $0
       if (line ~ /: *$/ && (getline rest) > 0) { sub(/ +$/, "", line); sub(/^ +/, "", rest); line = line " " rest }
       print line
     }' "$work/container.log" > "$work/errors.txt"
grep -E '^Warning( message| messages|:| in )' "$work/container.log" | grep -v '^Warning: Error in ' > "$work/warnings.txt"
n_errors=$(wc -l < "$work/errors.txt" | tr -d ' ')
n_warnings=$(wc -l < "$work/warnings.txt" | tr -d ' ')
while IFS= read -r line; do
  problem "container log: ${line:0:300}"
done < <(head -n 10 "$work/errors.txt")
while IFS= read -r line; do
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    echo "::warning title=App image check::container log: ${line:0:300}"
  else
    echo "WARNING: container log: ${line:0:300}"
  fi
done < <(head -n 5 "$work/warnings.txt")
echo "Container log: ${n_errors} errors, ${n_warnings} warnings"

# --- Result ---------------------------------------------------------------------
if [ "$problems" -eq 0 ]; then
  verdict="passed"
else
  verdict="failed"
  echo "----- end of the container log -----"
  tail -n 40 "$work/container.log" | cut -c1-300
  echo "-----"
fi
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "### App image check: ${verdict}"
    echo
    echo "| | |"
    echo "|---|---|"
    echo "| Image | \`${image}\` |"
    echo "| Page | ${page_line} |"
    echo "| Tabs | ${tabs_line} |"
    echo "| Container log | ${n_errors} errors, ${n_warnings} warnings |"
    echo
    if [ "$problems" -gt 0 ]; then
      echo "**Problems**"
      echo
      cat "$work/problems.md"
      echo
    fi
    if [ -s "$work/tabs.out" ]; then
      echo "<details><summary>Tabs</summary>"
      echo
      echo '```text'
      cut -c1-300 "$work/tabs.out"
      echo '```'
      echo
      echo "</details>"
      echo
    fi
    echo "<details><summary>End of the container log</summary>"
    echo
    echo '```text'
    tail -n 60 "$work/container.log" | cut -c1-300
    echo '```'
    echo
    echo "</details>"
  } >> "$GITHUB_STEP_SUMMARY"
fi

if [ "$problems" -eq 0 ]; then
  message="${image} starts and serves: ${page_line}; ${tabs_line}; no R error in the log"
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    echo "::notice title=App image check::${message}"
  else
    echo "OK: ${message}"
  fi
  exit 0
fi
echo "App image check failed: ${problems} problem(s)"
exit 1
