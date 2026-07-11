#!/usr/bin/env bash
# Print the git commit hash embedded (via sbt-buildinfo) in each deployed Explore build.
#
# Usage: ./deployed-git-hash.sh [host...]
#   Defaults to the three known deployment hosts (dev, staging, production).

set -euo pipefail

hosts=("$@")
if [[ ${#hosts[@]} -eq 0 ]]; then
  hosts=(explore-dev.lucuma.xyz explore-test.gemini.edu explore.gemini.edu)
fi

for host in "${hosts[@]}"; do
  bundle=$(curl -sk "https://${host}/" | grep -o 'assets/index-[A-Za-z0-9_-]*\.js' | head -1)

  if [[ -z "$bundle" ]]; then
    echo "${host}: could not find an assets/index-*.js reference" >&2
    continue
  fi

  hash=$(curl -sk "https://${host}/${bundle}" | grep -o '`[0-9a-f]\{40\}`' | tr -d '`' | sort -u | head -1)

  if [[ -z "$hash" ]]; then
    echo "${host}: could not find an embedded git hash in ${bundle}" >&2
    continue
  fi

  subject=$(git log -1 --format=%s "$hash" 2>/dev/null || true)

  if [[ -z "$subject" ]]; then
    echo "${host}: ${hash} (commit not found locally; try git fetch)"
  else
    echo "${host}: ${hash} ${subject}"
  fi
done
