#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

export EXPLORE_ODB_URI="${EXPLORE_ODB_URI:-ws://localhost:8082/ws}"
export EXPLORE_ODB_REST_URI="${EXPLORE_ODB_REST_URI:-http://localhost:8082}"

echo "EXPLORE_ODB_URI=$EXPLORE_ODB_URI"
echo "EXPLORE_ODB_REST_URI=$EXPLORE_ODB_REST_URI"

pnpm explore dev
