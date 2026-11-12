#!/bin/sh
# Installs Ubuntu packages for Dockerfile.gap_finder (builder and runtime stage):
#   sh apt_install.sh <package> ...
#
# Package sources, in this order; the first that answers is used:
#   1. snapshot.ubuntu.com as of APT_SNAPSHOT: the pinned archive, so every
#      rebuild gets the same system libraries. Tried twice, 60 s apart.
#   2. The live Ubuntu archive (archive.ubuntu.com, security.ubuntu.com), when
#      the snapshot server is down. On an LTS release it differs from the
#      snapshot only by stable and security updates.
#   3. Ubuntu mirrors run by others (APT_MIRRORS), when all of Canonical is
#      down, as on 2026-11-11. A mirror serves Canonical's signed package lists
#      and apt checks them against Ubuntu's archive key, so a mirror cannot
#      change packages; it can be a few hours behind. The Canonical sources are
#      put back after the install.
# /etc/gap-finder-apt-source records the source that was used: "snapshot <date>",
# "live archive <time>" or "mirror <url> <time>".
#
# Environment:
#   APT_SNAPSHOT  required; the Dockerfile's ARG
#   APT_MIRRORS   archive roots for amd64, space-separated (default below)
#   APT_RETRIES   download retries per file (default 8; the mirror check in
#                 build-image-pr.yml uses 1)
set -eu

APT_MIRRORS_DEFAULT="https://mirrors.edge.kernel.org/ubuntu https://mirror.cs.princeton.edu/pub/mirrors/ubuntu-archive"

: "${APT_SNAPSHOT:?APT_SNAPSHOT is not set}"
mirrors=${APT_MIRRORS:-$APT_MIRRORS_DEFAULT}
retries=${APT_RETRIES:-8}
if [ "$#" -eq 0 ]; then
  echo "usage: sh apt_install.sh <package> ..." >&2
  exit 2
fi

now() { date -u +%Y-%m-%dT%H:%MZ; }
# --error-on=any: a failed fetch fails the update instead of leaving stale or
# empty package lists behind.
update() { apt-get update --error-on=any; }

echo "APT::Snapshot \"${APT_SNAPSHOT}\";" > /etc/apt/apt.conf.d/50snapshot
echo "Acquire::Retries \"${retries}\"; Acquire::Retries::Delay::Maximum \"60\";" > /etc/apt/apt.conf.d/80retries

src=""
if update; then
  src="snapshot ${APT_SNAPSHOT}"
else
  echo "apt-get update failed, retrying in 60 s"
  sleep 60
  if update; then src="snapshot ${APT_SNAPSHOT}"; fi
fi

if [ -z "$src" ]; then
  echo "WARNING: snapshot.ubuntu.com is unavailable; installing from the live Ubuntu archive" >&2
  rm -f /etc/apt/apt.conf.d/50snapshot
  if update; then src="live archive $(now)"; fi
fi

# Source files that point at Canonical's archive: ubuntu.sources (deb822) on
# 24.04 and later, sources.list on older images.
lists=""
if [ -z "$src" ]; then
  lists=$(grep -l -E '//(archive|security)\.ubuntu\.com/' /etc/apt/sources.list /etc/apt/sources.list.d/* 2>/dev/null || true)
  if [ -z "$lists" ]; then
    echo "ERROR: no Ubuntu package source answered, and no apt source file names archive.ubuntu.com to swap for a mirror (the mirrors cover amd64 only)" >&2
    exit 100
  fi
  mkdir -p /tmp/apt-sources-orig
  i=0
  for f in $lists; do i=$((i + 1)); cp "$f" "/tmp/apt-sources-orig/$i"; done
  for m in $mirrors; do
    echo "WARNING: the live Ubuntu archive is unavailable too; trying the mirror $m" >&2
    i=0
    for f in $lists; do
      i=$((i + 1))
      sed -e "s#http[s]*://archive\.ubuntu\.com/ubuntu#${m}#g" \
          -e "s#http[s]*://security\.ubuntu\.com/ubuntu#${m}#g" \
          "/tmp/apt-sources-orig/$i" > "$f"
    done
    if update; then src="mirror $m $(now)"; break; fi
  done
fi

if [ -z "$src" ]; then
  echo "ERROR: no Ubuntu package source answered (snapshot, live archive, mirrors)" >&2
  exit 100
fi

apt-get install -y --no-install-recommends "$@"

if [ -n "$lists" ]; then
  i=0
  for f in $lists; do i=$((i + 1)); cp "/tmp/apt-sources-orig/$i" "$f"; done
  rm -rf /tmp/apt-sources-orig
fi
echo "$src" > /etc/gap-finder-apt-source
rm -rf /var/lib/apt/lists/*
echo "apt source: $src"
