#!/usr/bin/env bash

# Splits the client/server gap of Observe ODB traces into its request-side and
# response-side halves.
#
# For each trace it reports the client's clue span, the ODB's graphql-query span,
# and the two gaps between them:
#
#   pre  = graphql-query.start - clue.start
#          request encode + transit + everything the ODB does before GraphQL
#          execution begins (queueing, auth, routing). None of it is traced.
#   post = clue.end - graphql-query.end
#          response serialization + transit + client-side circe decoding.
#          Also untraced.
#
# A large `pre` with a small `server` means the request was waiting, not working.
# See docs/tracing-instrumentation.md.
#
# `pre`/`post` are differences between clocks on two machines, so they are only
# meaningful when client and ODB run on the same host, or when their clocks are
# known to be in sync. Expect a few ms of noise either way; small negative
# values are normal.
#
# Usage: trace-gap.sh [-s SINCE] [-n LIMIT] [-c CONTEXT] [-d DATASOURCE] [TRACE_ID...]
#
#   With trace ids, analyses exactly those.
#   With none, discovers recent observe-client and observe-server traces and
#   analyses them (server roots are the backend's ODB event mutations).
#
#   -s SINCE       lookback window for discovery (default 24h)
#   -n LIMIT       maximum traces to analyse when discovering (default 20)
#   -c CONTEXT     gcx context to use (default: current-context). Traces from a
#                  remote-ODB run live in a different Grafana account/context.
#   -d DATASOURCE  Tempo datasource UID (default grafanacloud-traces; other
#                  accounts may name theirs differently)
#
# Example: trace-gap.sh -s 3h
# Example: trace-gap.sh -c staging -d grafanacloud-traces -s 24h
# Example: trace-gap.sh 3b3bd4b0759449735764cda54b3fd389

set -euo pipefail

DATASOURCE=grafanacloud-traces
SINCE=24h
LIMIT=20
CONTEXT=""

while getopts "s:n:c:d:" opt; do
  case $opt in
    s) SINCE=$OPTARG ;;
    n) LIMIT=$OPTARG ;;
    c) CONTEXT=$OPTARG ;;
    d) DATASOURCE=$OPTARG ;;
    *) echo "Usage: $0 [-s SINCE] [-n LIMIT] [-c CONTEXT] [-d DATASOURCE] [TRACE_ID...]" >&2; exit 1 ;;
  esac
done
shift $((OPTIND - 1))

GCX_ARGS=()
[ -n "$CONTEXT" ] && GCX_ARGS+=(--context "$CONTEXT")

command -v gcx >/dev/null || { echo "gcx not found" >&2; exit 1; }
command -v jq >/dev/null || { echo "jq not found" >&2; exit 1; }

trace_ids=("$@")

if [ ${#trace_ids[@]} -eq 0 ]; then
  echo "Discovering observe traces from the last $SINCE..." >&2
  # observe-client roots are browser queries; observe-server roots are the backend's
  # ODB event mutations. Only clue-* roots; both services also emit bare POST spans.
  mapfile -t trace_ids < <(
    gcx traces query "${GCX_ARGS[@]}" -d "$DATASOURCE" '{ resource.service.name =~ "observe-(client|server)" }' \
        --since "$SINCE" -o json 2>/dev/null \
      | jq -s -r --argjson n "$LIMIT" '
          map(select(.traces)) | (.[0].traces // [])
          | map(select((.rootTraceName? // "") | startswith("clue-")))
          | sort_by(.startTimeUnixNano | tonumber) | reverse
          | .[:$n] | .[].traceID'
  )
fi

[ ${#trace_ids[@]} -eq 0 ] && { echo "No traces found." >&2; exit 0; }

for t in "${trace_ids[@]}"; do
  gcx traces get "${GCX_ARGS[@]}" -d "$DATASOURCE" "$t" --llm -o json 2>/dev/null | jq -s -r --arg t "$t" '
    (map(select(.trace)) | .[0].trace) as $tr
    | if $tr == null then empty else
    [$tr.services[] | .serviceName as $sn | .scopes[].spans[] | . + {svc: $sn}] as $spans
    | ($spans | map(select((.svc | test("^observe-(client|server)$")) and (.name | startswith("clue-")))) | .[0]) as $c
    | ($spans | map(select(.name == "graphql-query" or .name == "graphql-mutation")) | .[0]) as $g
    | if $c == null or $g == null then empty else
        {
          trace:  $t[0:8],
          query:  ($c.name | sub("^clue-request-"; "")),
          total:  $c.durationMs,
          server: $g.durationMs,
          cs:     ($c.startTimeUnixNano | tonumber),
          ce:     ($c.endTimeUnixNano   | tonumber),
          gs:     ($g.startTimeUnixNano | tonumber),
          ge:     ($g.endTimeUnixNano   | tonumber)
        }
      end
    end'
done | jq -s -r '
  if length == 0 then "No traces with both a clue span and an ODB span.\n" | halt_error(0) else . end
  # Traces issued close together are grouped into a burst and timed against a
  # shared origin, so concurrent queries line up. Queries that overlap in
  # `client` but not in `ODB` are being serialized somewhere untraced.
  | (5000 * 1000000) as $burstGap
  | sort_by(.cs)
  | reduce .[] as $r ([];
      if length == 0 or ($r.cs - .[-1][-1].cs) > $burstGap
      then . + [[$r]]
      else .[:-1] + [.[-1] + [$r]] end)
  | .[]
  | (map(.cs, .gs) | min) as $base
  | def ms($x): (($x - $base) / 1000000) | round;
    (.[] | [
       .trace,
       (.query | .[0:24]),
       (.total  | round),
       (.server | round),
       ((.gs - .cs) / 1000000 | round),
       ((.ce - .ge) / 1000000 | round),
       "client[\(ms(.cs)) → \(ms(.ce))]  ODB[\(ms(.gs)) → \(ms(.ge))]"
     ] | @tsv),
    "--"' \
  | awk -F'\t' '
      BEGIN { fmt = "%-10s %-24s %7s %7s %7s %7s   %s\n" }
      $1 == "--" { pending = 1; next }
      NF == 7 {
        if (!seen++ || pending) {
          if (seen > 1) print ""
          printf fmt, "TRACE", "QUERY", "TOTAL", "SERVER", "PRE", "POST", "TIMELINE (ms, per burst)"
          pending = 0
        }
        printf fmt, $1, $2, $3, $4, $5, $6, $7
        next
      }
      { print }'
