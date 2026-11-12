#!/usr/bin/env bash
# Throwaway probe helper: prints lines <from>..<to> of notices.tsv
# (title<TAB>message) as notice annotations and adds them to the job summary.
# GitHub keeps only about 10 annotations of a kind per step, so the workflow
# calls this in slices of 9.
n="${RUNNER_TEMP:-/tmp}/probe/notices.tsv"
[ -f "$n" ] || exit 0
esc() { local s=${1//'%'/'%25'}; s=${s//$'\r'/'%0D'}; s=${s//$'\n'/'%0A'}; printf '%s' "$s"; }
esc_title() { local s; s=$(esc "$1"); s=${s//':'/'%3A'}; s=${s//','/'%2C'}; printf '%s' "$s"; }
sed -n "${1},${2}p" "$n" | while IFS=$'\t' read -r title msg; do
  echo "::notice title=$(esc_title "$title")::$(esc "$msg")"
  printf -- '- **%s**: %s\n' "$title" "$msg" >> "${GITHUB_STEP_SUMMARY:-/dev/null}"
done
exit 0
