#!/usr/bin/env bash
# Throwaway probe (branch probe/apt-sources, never merged): which Ubuntu package
# sources a GitHub-hosted runner can use, and what apt contacts in snapshot mode.
# Writes notices.tsv (title<TAB>message); annotate.sh turns it into annotations.
set -u
out="${RUNNER_TEMP:-/tmp}/probe"
mkdir -p "$out"
N="$out/notices.tsv"
: > "$N"
note() { printf '%s\t%s\n' "$1" "$2" >> "$N"; printf '== %s: %s\n' "$1" "$2"; }

df=shiny_app/gap_finder/Dockerfile.gap_finder
BASE=$(sed -n 's/^FROM \(rocker\/r-ver[^ ]*\) AS builder$/\1/p' "$df")
SNAP=$(sed -n 's/^ARG APT_SNAPSHOT=//p' "$df")
docker pull -q "$BASE" > /dev/null

KO=https://mirrors.edge.kernel.org/ubuntu
PR=https://mirror.cs.princeton.edu/pub/mirrors/ubuntu-archive
UMU=https://ftp.acc.umu.se/ubuntu
MIT=https://mirrors.mit.edu/ubuntu
AZ=http://azure.archive.ubuntu.com/ubuntu
owner() { curl -sL --max-time 10 "https://rdap.arin.net/registry/ip/$1" | jq -r '.name // "?"' 2> /dev/null; }

# 1. The runner (1 notice)
ip=$(curl -s --max-time 10 https://api.ipify.org || true)
region=$(curl -s --max-time 3 -H Metadata:true \
  "http://169.254.169.254/metadata/instance/compute/location?api-version=2021-02-01&format=text" 2> /dev/null || true)
note "runner ${LABEL:-?}" "os $(. /etc/os-release; echo "$VERSION_ID") | ip ${ip:-?} ($(owner "${ip:-0}")) | region ${region:-?} | base ${BASE##*@} | APT_SNAPSHOT $SNAP"

# 2. apt-get update + one .deb per source, in the build's base image (7 notices)
run() { timeout 600 docker run --rm "$@" -v "$PWD/.github/probe:/probe:ro" "$BASE" bash /probe/in_container.sh 2>&1 | tail -1; }
note "apt snapshot (as built)" "$(run -e SNAPMODE=yes -e SNAP="$SNAP" -e ERRANY=1)"
note "apt live archive+security" "$(run -e SNAPMODE=no -e ERRANY=1)"
note "apt azure.archive" "$(run -e SNAPMODE=no -e ROOT=$AZ -e ERRANY=1)"
note "apt kernel.org" "$(run -e SNAPMODE=no -e ROOT=$KO -e ERRANY=1)"
note "apt princeton" "$(run -e SNAPMODE=no -e ROOT=$PR -e ERRANY=1)"
note "apt acc.umu.se" "$(run -e SNAPMODE=no -e ROOT=$UMU -e ERRANY=1)"
note "apt mit.edu" "$(run -e SNAPMODE=no -e ROOT=$MIT -e ERRANY=1)"

# 3. Snapshot mode: does it need the live hosts? (4 notices)
B=(--add-host archive.ubuntu.com:127.0.0.1 --add-host security.ubuntu.com:127.0.0.1)
note "Q2 snapshot, live blocked, error-on=any" "$(run "${B[@]}" -e SNAPMODE=yes -e SNAP="$SNAP" -e ERRANY=1)"
note "Q2 snapshot, live blocked, no error-on" "$(run "${B[@]}" -e SNAPMODE=yes -e SNAP="$SNAP" -e ERRANY=0)"
note "Q2 snapshot URIs in sources, live blocked" "$(run "${B[@]}" -e SNAPMODE=uris -e SNAP="$SNAP" -e ERRANY=1)"
note "Q2 snapshot, snapshot blocked" "$(run --add-host snapshot.ubuntu.com:127.0.0.1 -e SNAPMODE=yes -e SNAP="$SNAP" -e ERRANY=1)"

# 4. HTTP status, time and IP per source, curl from the runner (2 notices)
pool=$(curl -s --max-time 60 http://archive.ubuntu.com/ubuntu/dists/resolute/main/binary-amd64/Packages.gz |
  zcat 2> /dev/null | awk '/^Package: base-files$/ { p = 1 } p && /^Filename:/ { print $2; exit }')
spool=$(curl -s --max-time 60 http://security.ubuntu.com/ubuntu/dists/resolute-security/main/binary-amd64/Packages.gz |
  zcat 2> /dev/null | awk '/^Filename:/ { print $2; exit }')
c() {
  curl -sS -o /dev/null --connect-timeout 15 --max-time 90 -A "${UA:-curl/8}" \
    -w '%{http_code} %{time_total}s %{remote_ip}' "$1" 2>&1 | tr '\n' ' ' | cut -c1-140
}
row=""
for s in "snapshot https://snapshot.ubuntu.com/ubuntu/$SNAP" "archive http://archive.ubuntu.com/ubuntu" "azure $AZ"; do
  row="$row${s%% *}: InRelease $(c "${s#* }/dists/resolute/InRelease"), deb $(c "${s#* }/$pool"); "
done
row="${row}security: InRelease $(c http://security.ubuntu.com/ubuntu/dists/resolute-security/InRelease), deb $(c "http://security.ubuntu.com/ubuntu/$spool")"
note "curl Canonical (deb ${pool##*/})" "$row"
row=""
for s in "kernel.org $KO" "princeton $PR" "acc.umu.se $UMU" "mit.edu $MIT"; do
  row="$row${s%% *}: InRelease $(c "${s#* }/dists/resolute/InRelease"), deb $(c "${s#* }/$pool"); "
done
note "curl mirrors (deb ${pool##*/})" "$row"

# 5. kernel.org's timeout and Princeton's 403 (3 notices)
ko=""
for k in $(getent ahostsv4 mirrors.edge.kernel.org | awk '{ print $1 }' | sort -u); do
  h443=$(curl -sS -o /dev/null --connect-timeout 10 --max-time 20 --resolve "mirrors.edge.kernel.org:443:$k" \
    -w '%{http_code} connect %{time_connect}s' "$KO/dists/resolute/InRelease" 2>&1 | tr '\n' ' ' | cut -c1-90)
  h80=$(curl -sS -o /dev/null --connect-timeout 10 --max-time 20 --resolve "mirrors.edge.kernel.org:80:$k" \
    -w '%{http_code} connect %{time_connect}s' "http://mirrors.edge.kernel.org/ubuntu/dists/resolute/InRelease" 2>&1 | tr '\n' ' ' | cut -c1-90)
  ko="$ko$k ($(owner "$k")): https $h443 / http $h80; "
done
note "kernel.org per IP" "${ko:-no A record}"
APTUA="Debian APT-HTTP/1.3 (3.2.1ubuntu0.1)"
BRUA="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36"
pr=""
for ua in "curl/8" "$APTUA" "$BRUA"; do
  pr="${pr}[${ua%% *}] InRelease $(UA="$ua" c "$PR/dists/resolute/InRelease"), deb $(UA="$ua" c "$PR/$pool"); "
done
pip=$(getent ahostsv4 mirror.cs.princeton.edu | awk '{ print $1; exit }')
note "princeton by user agent" "${pr}ip ${pip:-?} ($(owner "${pip:-0}"))"
hdr=$(curl -sS -o /dev/null -D - --max-time 30 -A "$APTUA" "$PR/$pool" 2>&1 | grep -iE '^(HTTP|server|content-type|x-)' | tr -d '\r' | tr '\n' ' ' | cut -c1-200)
body=$(curl -s --max-time 30 -A "$APTUA" "$PR/$pool" | head -c 600 | tr -c '[:print:]' ' ' | sed 's/  */ /g' | cut -c1-300)
note "princeton .deb response" "$hdr | ${body:-empty}"
exit 0
