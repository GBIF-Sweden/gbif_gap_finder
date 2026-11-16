#!/usr/bin/env bash
# Opens every tab of the live app in a headless browser, with the same check that
# the app check runs on the image before a release (check_app_tabs.py): no tab
# shows an error, and no plot, map or table stays empty.
#
#   bash .github/scripts/check_live_tabs.sh https://gbif.se/gap-finder/
#
# Used by weekly-status.yml, after weekly_status.R has found that the page
# answers. There each problem becomes an error annotation and the tab lines go on
# the run's summary page. When the check finds a problem, it runs once more after
# CHECK_RETRY_WAIT seconds (default 300) and only the second result counts. Exit
# status 1 if a problem remains.
#
# Environment: CHECK_RETRY_WAIT; CHECK_BROWSER_CHANNEL and CHECK_TAB_TIMEOUT, see
# check_app_tabs.py.

# No `set -e`: the tab check's own exit status is what this script reports.
set -uo pipefail

url=${1:?usage: check_live_tabs.sh <url>}
here=$(cd "$(dirname "$0")" && pwd)
retry_wait=${CHECK_RETRY_WAIT:-300}
out=$(mktemp)
trap 'rm -f "$out"' EXIT

# One annotation line on GitHub, a plain line elsewhere.
say() {
  local level=$1 message=${2//%/%25}
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    echo "::${level} title=Live app tabs::${message}"
  else
    echo "$(printf '%s' "$level" | tr '[:lower:]' '[:upper:]'): $2"
  fi
}

python3 "$here/check_app_tabs.py" "$url" > "$out" 2>&1
status=$?
retried=no
if [ "$status" -ne 0 ]; then
  echo "The tab check found problems; it runs again in ${retry_wait} s. First result:"
  cat "$out"
  sleep "$retry_wait"
  python3 "$here/check_app_tabs.py" "$url" > "$out" 2>&1
  status=$?
  retried=yes
fi
grep -v '^PROBLEM: ' "$out"

# GitHub keeps at most 10 error annotations per step: 9 problems, then a count.
problems=$(grep -c '^PROBLEM: ' "$out")
n=0
while IFS= read -r line; do
  n=$((n + 1))
  if [ "$n" -le 9 ]; then
    say error "${line#PROBLEM: }"
  fi
done < <(grep '^PROBLEM: ' "$out")
if [ "$problems" -gt 9 ]; then
  say error "$((problems - 9)) more problems; all of them are on the run's summary page"
fi
if [ "$status" -ne 0 ] && [ "$problems" -eq 0 ]; then
  say error "the tab check did not run: $(tail -n 1 "$out" | cut -c1-300)"
  problems=1
fi

tabs_line=$(grep -E '^[0-9]+ tabs opened' "$out" | tail -n 1)
tabs_line=${tabs_line:-did not finish}
if [ "$problems" -eq 0 ]; then
  verdict="passed"
else
  verdict="failed"
fi
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "### Live app tabs: ${verdict}"
    echo
    echo "${url}: ${tabs_line}."
    if [ "$retried" = yes ]; then
      echo "The first try found problems, so the check ran again after ${retry_wait} s. This is the second result."
    fi
    echo
    if [ "$problems" -gt 0 ]; then
      echo "**Problems**"
      echo
      grep '^PROBLEM: ' "$out" | sed -e 's/^PROBLEM: /- /' | cut -c1-300
      echo
    fi
    echo "<details><summary>Tabs</summary>"
    echo
    echo '```text'
    cut -c1-300 "$out"
    echo '```'
    echo
    echo "</details>"
    echo
  } >> "$GITHUB_STEP_SUMMARY"
fi

if [ "$problems" -eq 0 ]; then
  say notice "${url}: ${tabs_line}"
  exit 0
fi
echo "Live app tabs: ${problems} problem(s)"
exit 1
