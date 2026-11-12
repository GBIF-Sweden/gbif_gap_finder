#!/usr/bin/env bash
# Prints the name of the dependency image for the checked-out commit:
#   ghcr.io/gbif-sweden/gap-finder:deps-<hash>
# <hash> is the first 16 hex digits of the SHA-256 of the Dockerfile followed
# by renv.lock: the two files that decide the system libraries and the R
# packages. A change in either gives a new name, so an image under a given name
# is never rebuilt with different contents.
# Used by build-deps-image.yml (builds and pushes the image) and by
# publish-shiny-images.yml (starts the release image from it). Runs on macOS
# too: `bash .github/scripts/deps_image.sh`.
set -euo pipefail
cd "$(dirname "$0")/../.."
if command -v sha256sum > /dev/null 2>&1; then
  sum() { sha256sum; }
else
  sum() { shasum -a 256; }
fi
hash=$(cat shiny_app/gap_finder/Dockerfile.gap_finder renv.lock | sum | cut -c1-16)
owner=$(printf '%s' "${GITHUB_REPOSITORY_OWNER:-GBIF-Sweden}" | tr '[:upper:]' '[:lower:]')
echo "ghcr.io/${owner}/gap-finder:deps-${hash}"
