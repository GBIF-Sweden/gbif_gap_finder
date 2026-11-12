#!/usr/bin/env bash
# Throwaway probe (branch probe/apt-sources, never merged): where did the
# published images get their system libraries from, and which versions?
set -u
out="${RUNNER_TEMP:-/tmp}/probe"
mkdir -p "$out"
N="$out/notices.tsv"
: > "$N"
note() { printf '%s\t%s\n' "$1" "$2" >> "$N"; printf '== %s: %s\n' "$1" "$2"; }

for t in v1.1.1 v1.0.2; do
  img=ghcr.io/gbif-sweden/gap-finder:$t
  if ! docker pull -q "$img" > /dev/null 2>&1; then
    note "image $t" "anonymous pull failed"
    continue
  fi
  src=$(docker run --rm "$img" cat /etc/gap-finder-apt-source 2> /dev/null || echo "no /etc/gap-finder-apt-source")
  libs=$(docker run --rm "$img" dpkg-query -W -f='${db:Status-Abbrev}${Package}=${Version}\n' 'libgdal*' 'libgeos*' 'libproj*' 2> /dev/null |
    awk '/^ii/ { print $2 }' | tr '\n' ' ')
  rev=$(docker image inspect -f '{{index .Config.Labels "org.opencontainers.image.revision"}} {{.Created}}' "$img")
  note "image $t" "apt source: $src | $libs| revision+created $rev"
done
exit 0
