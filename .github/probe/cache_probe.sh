#!/usr/bin/env bash
# Throwaway probe (branch probe/apt-sources, never merged): does a build from an
# imported BuildKit layer cache still need Ubuntu's or Posit's servers?
# 1. Cold build of the runtime target on an empty builder; export the layer
#    cache (mode=max) to a local directory.
# 2. Block Canonical (snapshot/archive/security) and Posit (P3M) for containers.
# 3. A second, empty builder imports that cache and builds the same target.
# Reports exit codes, times, and how many RUN steps came from the cache.
set -u
out="${RUNNER_TEMP:-/tmp}/probe"
mkdir -p "$out"
N="$out/notices.tsv"
: > "$N"
note() { printf '%s\t%s\n' "$1" "$2" >> "$N"; printf '== %s: %s\n' "$1" "$2"; }

df=shiny_app/gap_finder/Dockerfile.gap_finder
BASE=$(sed -n 's/^FROM \(rocker\/r-ver[^ ]*\) AS builder$/\1/p' "$df")
cache="$out/bk"
build() { # <builder> <log> <cache args...>; prints "<exit> <seconds>"
  local b=$1 log=$2 s
  shift 2
  s=$(date +%s)
  docker buildx build --builder "$b" --progress=plain -f "$df" --target runtime "$@" \
    --output type=cacheonly . > "$log" 2>&1
  echo "$? $(( $(date +%s) - s ))"
}
runs() { # RUN steps taken from the cache, in a plain-progress log
  awk 'match($0, /^#[0-9]+ \[[^]]+\] RUN /) { r[substr($1, 2)] = 1 }
       /^#[0-9]+ CACHED$/ { c[substr($1, 2)] = 1 }
       END { n = 0; k = 0; for (i in r) { n++; if (i in c) k++ }; printf "%d of %d RUN steps cached", k, n }' "$1"
}

docker pull -q "$BASE" > /dev/null
docker buildx create --name cold --driver docker-container > /dev/null
read -r rc1 t1 < <(build cold "$out/cold.log" --cache-to "type=local,dest=$cache,mode=max")
note "cache 1: cold build (empty builder)" "exit $rc1, ${t1} s, $(runs "$out/cold.log"), exported cache $(du -sh "$cache" 2> /dev/null | cut -f1)"
if [ "$rc1" != 0 ]; then
  note "cache 1: last lines" "$(tail -5 "$out/cold.log" | tr '\n' ' ' | cut -c1-400)"
  exit 0
fi

ips=0
for h in snapshot.ubuntu.com archive.ubuntu.com security.ubuntu.com packagemanager.posit.co p3m.dev cloud.r-project.org; do
  for ip in $(getent ahostsv4 "$h" | awk '{ print $1 }' | sort -u); do
    sudo iptables -I FORWARD -d "$ip" -j REJECT && sudo iptables -I OUTPUT -d "$ip" -j REJECT && ips=$((ips + 1))
  done
done
chk=$(docker run --rm "$BASE" bash -c 'timeout 10 bash -c "</dev/tcp/archive.ubuntu.com/80" 2> /dev/null && echo open || echo blocked')

docker buildx create --name warm --driver docker-container > /dev/null
read -r rc2 t2 < <(build warm "$out/warm.log" --cache-from "type=local,src=$cache")
hits=$(grep -cE 'snapshot\.ubuntu\.com|archive\.ubuntu\.com|security\.ubuntu\.com|packagemanager\.posit\.co' "$out/warm.log")
note "cache 2: empty builder + imported cache, Canonical and Posit blocked" "exit $rc2, ${t2} s, $(runs "$out/warm.log"); $ips IPs blocked, archive.ubuntu.com from a container: $chk; log lines naming Ubuntu/Posit hosts: $hits"
if [ "$rc2" != 0 ]; then
  note "cache 2: last lines" "$(tail -5 "$out/warm.log" | tr '\n' ' ' | cut -c1-400)"
fi
exit 0
