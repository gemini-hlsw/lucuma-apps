#!/usr/bin/env bash
#
# Push guide-configuration states to a running observe web server, to exercise the
# GuideConfigStatus UI. This uses the same route the ODB/TCS uses to report guiding:
#
#   POST /api/observe/guide   (no auth; body is a GuideConfig JSON)
#
# Usage:
#   ./set-guide-config.sh <preset>   # apply one preset
#   ./set-guide-config.sh cycle      # walk through all presets (default)
#   ./set-guide-config.sh list       # show available presets
#   ./set-guide-config.sh off        # reset to all-off
#
# Override the target with env vars (defaults shown):
#   GUIDE_HOST=localhost GUIDE_PORT=7071 ./set-guide-config.sh on
#
# The status only renders inside a loaded observation's ConfigPanel, so load an
# observation in the UI first. Each POST updates the state live over the WebSocket.
set -euo pipefail

HOST="${GUIDE_HOST:-localhost}"
PORT="${GUIDE_PORT:-7071}"
URL="http://${HOST}:${PORT}/api/observe/guide"
CYCLE_DELAY="${GUIDE_CYCLE_DELAY:-3}"

# --- Presets (wire format verified by observe/server/.../GuideConfigDbSuite) -------------
OFF='{"tcsGuide":{"mountGuideOn":false,"m1Guide":{"on":false},"m2Guide":{"on":false},"dayTimeMode":false},"gaosGuide":null}'

ON='{"tcsGuide":{"mountGuideOn":true,"m1Guide":{"on":true,"source":"PWFS1"},"m2Guide":{"on":true,"sources":["PWFS1","PWFS2"],"comaOn":true},"dayTimeMode":false},"gaosGuide":null}'

MOUNT='{"tcsGuide":{"mountGuideOn":true,"m1Guide":{"on":false},"m2Guide":{"on":false},"dayTimeMode":false},"gaosGuide":null}'

M1='{"tcsGuide":{"mountGuideOn":false,"m1Guide":{"on":true,"source":"PWFS2"},"m2Guide":{"on":false},"dayTimeMode":false},"gaosGuide":null}'

TIPTILT='{"tcsGuide":{"mountGuideOn":false,"m1Guide":{"on":false},"m2Guide":{"on":true,"sources":["OIWFS"],"comaOn":false},"dayTimeMode":false},"gaosGuide":null}'

# M2 guiding on with coma on but no tip/tilt sources: Coma active, Tip/Tilt shows Off.
COMA='{"tcsGuide":{"mountGuideOn":false,"m1Guide":{"on":false},"m2Guide":{"on":true,"sources":[],"comaOn":true},"dayTimeMode":false},"gaosGuide":null}'

MIXED='{"tcsGuide":{"mountGuideOn":true,"m1Guide":{"on":true,"source":"OIWFS"},"m2Guide":{"on":true,"sources":["GAOS"],"comaOn":false},"dayTimeMode":false},"gaosGuide":null}'

# name=description|json
PRESETS=(
  "off=everything off (reset)|${OFF}"
  "on=mount + M1 PWFS1 + Tip/Tilt PWFS1+PWFS2 + Coma|${ON}"
  "mount=mount only|${MOUNT}"
  "m1=M1 PWFS2 only|${M1}"
  "tiptilt=Tip/Tilt OIWFS only|${TIPTILT}"
  "coma=Coma on, no tip/tilt sources|${COMA}"
  "mixed=mount + M1 OIWFS + Tip/Tilt GAOS|${MIXED}"
)

usage() {
  cat <<EOF
Usage: $0 <preset|cycle|list>

Presets:
EOF
  list_presets
  cat <<EOF

Target: ${URL}   (override with GUIDE_HOST / GUIDE_PORT)
EOF
}

list_presets() {
  for entry in "${PRESETS[@]}"; do
    name_desc="${entry%%|*}"
    name="${name_desc%%=*}"
    desc="${name_desc#*=}"
    printf '  %-8s %s\n' "$name" "$desc"
  done
}

json_for() {
  local want="$1"
  for entry in "${PRESETS[@]}"; do
    name_desc="${entry%%|*}"
    name="${name_desc%%=*}"
    if [[ "$name" == "$want" ]]; then
      echo "${entry#*|}"
      return 0
    fi
  done
  return 1
}

post() {
  local name="$1" body="$2"
  local code
  code="$(curl -s -o /dev/null -w '%{http_code}' \
    -X POST "$URL" \
    -H 'Content-Type: application/json' \
    -d "$body")"
  if [[ "$code" == "200" ]]; then
    printf '✓ %-8s -> %s\n' "$name" "$body"
  else
    printf '✗ %-8s -> HTTP %s (body: %s)\n' "$name" "$code" "$body" >&2
    return 1
  fi
}

cmd="${1:-cycle}"
case "$cmd" in
  list)
    list_presets
    ;;
  cycle)
    echo "Target: ${URL}"
    for entry in "${PRESETS[@]}"; do
      name_desc="${entry%%|*}"
      name="${name_desc%%=*}"
      post "$name" "$(json_for "$name")"
      sleep "$CYCLE_DELAY"
    done
    echo "Done."
    ;;
  -h|--help|help)
    usage
    ;;
  *)
    body="$(json_for "$cmd" || true)"
    if [[ -z "$body" ]]; then
      echo "Unknown preset: $cmd" >&2
      echo
      usage
      exit 1
    fi
    post "$cmd" "$body"
    ;;
esac
