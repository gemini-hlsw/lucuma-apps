#!/bin/bash

# Refetches the user preferences schema.

set -euo pipefail

cd "$(dirname "$0")"

ENDPOINT="${1:-https://user-prefs-master.herokuapp.com/v1/graphql}"
TARGET=app/src/clue/resources/UserPreferencesDB.graphql

TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT

gq "$ENDPOINT" --introspect > "$TMP"

# Clue resolves root fields only under the conventional names, so drop Hasura's
# schema block and rename its roots. Without this, codegen fails with
# "Could not resolve type for validated field [...]".
perl -0pi -e '
  s/\A\s*schema\s*\{[^}]*\}\s*\n//;
  s/^type query_root \{/type Query {/m;
  s/^type mutation_root \{/type Mutation {/m;
  s/^type subscription_root \{/type Subscription {/m;
' "$TMP"

for root in Query Mutation Subscription; do
  if ! grep -q "^type $root {" "$TMP"; then
    echo "Conversion failed: no 'type $root' in the introspected schema" >&2
    exit 1
  fi
done

if grep -q "_root" "$TMP"; then
  echo "Conversion failed: leftover '_root' references" >&2
  exit 1
fi

chmod 644 "$TMP"
mv "$TMP" "$TARGET"
echo "Wrote $TARGET from $ENDPOINT"
