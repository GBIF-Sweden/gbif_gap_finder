#!/bin/bash
# Throwaway probe helper. Runs inside the image build's base image
# (rocker/r-ver, Ubuntu 26.04) with fresh apt lists, as in the Dockerfile:
# apt-get update, then download one .deb (libgeos-dev, as in PR #12's check).
# Environment:
#   SNAPMODE  yes  = APT::Snapshot in apt.conf, as Dockerfile.gap_finder does
#             uris = sources rewritten to https://snapshot.ubuntu.com/ubuntu/<SNAP>
#             no   = no snapshot
#   SNAP      snapshot id (the Dockerfile's APT_SNAPSHOT)
#   ROOT      archive root that replaces archive/security.ubuntu.com (mirrors)
#   ERRANY    1 = apt-get update --error-on=any (as in the Dockerfile), 0 = without
# Prints one line: exit codes and times, candidate libgdal-dev, requests per host
# (Get/Err/Ign counts, MB fetched), first error.
files=$(ls /etc/apt/sources.list /etc/apt/sources.list.d/* 2> /dev/null)
root=${ROOT:-}
case "${SNAPMODE:-no}" in
  yes) echo "APT::Snapshot \"$SNAP\";" > /etc/apt/apt.conf.d/50snapshot ;;
  uris) root="https://snapshot.ubuntu.com/ubuntu/$SNAP" ;;
esac
if [ -n "$root" ]; then
  for f in $files; do
    sed -i -E "s#https?://(archive|security)\.ubuntu\.com/ubuntu/?#${root}/#g" "$f"
  done
fi
o=(-o Acquire::Retries=0 -o Acquire::http::Timeout=20 -o Acquire::https::Timeout=20 -o APT::Sandbox::User=root)
e=()
[ "${ERRANY:-1}" = 1 ] && e=(--error-on=any)
s=$(date +%s%N)
apt-get "${o[@]}" update "${e[@]}" > /tmp/u.log 2>&1
u=$?
m=$(date +%s%N)
cd /tmp && apt-get "${o[@]}" download libgeos-dev > /tmp/d.log 2>&1
d=$?
t=$(date +%s%N)
hosts=$(awk '
  /^(Hit|Get|Ign|Err):[0-9]+ / {
    match($0, /[a-z]+:\/\/[^\/ ]+/); h = substr($0, RSTART, RLENGTH); sub(/^[a-z]+:\/\//, "", h)
    split($1, k, ":"); c[h " " k[1]]++
    if (k[1] == "Get" && match($0, /\[[0-9.,]+ [kMG]?B\]$/)) {
      s = substr($0, RSTART + 1, RLENGTH - 2); split(s, a, " "); v = a[1]; gsub(",", "", v)
      f = (a[2] == "kB") ? 1e3 : (a[2] == "MB") ? 1e6 : (a[2] == "GB") ? 1e9 : 1
      mb[h] += v * f / 1e6
    }
  }
  END {
    for (x in c) {
      split(x, p, " ")
      printf "%s %s x%d%s; ", p[1], p[2], c[x], (p[2] == "Get" ? sprintf(" (%.1f MB)", mb[p[1]]) : "")
    }
  }' /tmp/u.log)
err=$({ grep -h -A1 -E '^Err:' /tmp/u.log /tmp/d.log | head -2; grep -h -E '^E: ' /tmp/u.log /tmp/d.log | head -1; } | tr -s ' \n' '  ' | cut -c1-350)
gdal=$(apt-cache policy libgdal-dev 2> /dev/null | sed -n 's/^ *Candidate: //p')
echo "update=$u ($(( (m - s) / 1000000 )) ms) download=$d ($(( (t - m) / 1000000 )) ms) libgdal-dev=${gdal:-none} | ${hosts}| ${err}"
