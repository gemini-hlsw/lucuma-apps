#!/usr/bin/env bash
#
# Convert the ITS-provided PEM certificates (packed in *.tar.gz) into Java
# keystores (JKS) as described in observe/web/server/TLS.md.
#
# Each tar.gz contains a leaf certificate (<name>.cer.pem) and its private key
# (<name>.key.pem). For each archive we:
#   1. Extract the cert + key into a temporary directory.
#   2. Build a PKCS12 store with openssl (java cannot import the key directly).
#   3. Import the PKCS12 into a fresh JKS with keytool.
#   4. List the resulting keystore to confirm it holds one entry.
#
# Output keystore names follow the pattern:
#   <3-letter-site>gpp<3-letter-system>[dev]-lv1.<2-letter-site>.gemini.edu.jks
#
#   3-letter site : mko (hi) / cpo (cl)
#   3-letter system: obs (observe) / nav (navigate)
#   dev           : present for the -test ("dev") servers
#   2-letter site : cl / hi (taken from the filename)
#
# The keystore password and certificate password are the same value. There is
# one password for the dev (-test) servers and one for the production servers;
# both are prompted for below.

set -euo pipefail

cd "$(dirname "$0")/.certs"

# --- Prompt for passwords (keystore pwd == cert pwd for each environment) ----
read -r -s -p "Password for DEV (-test) keystores: " DEV_PWD; echo
read -r -s -p "Password for PRODUCTION keystores: " PROD_PWD; echo

if [[ -z "$PROD_PWD" || -z "$DEV_PWD" ]]; then
  echo "Both passwords are required." >&2
  exit 1
fi

system_code() {
  case "$1" in
    observe)  echo "obs" ;;
    navigate) echo "nav" ;;
    *) echo "Unknown system: $1" >&2; exit 1 ;;
  esac
}

site_code() {
  case "$1" in
    hi) echo "mko" ;;
    cl) echo "cpo" ;;
    *) echo "Unknown site: $1" >&2; exit 1 ;;
  esac
}

shopt -s nullglob
for archive in *.tar.gz; do
  base="${archive%.tar.gz}"            # e.g. observe-test.cl  /  navigate.hi

  site2="${base##*.}"                  # cl / hi
  left="${base%.*}"                    # observe-test / navigate

  is_dev="no"
  system="$left"
  if [[ "$left" == *-test ]]; then
    is_dev="yes"
    system="${left%-test}"
  fi

  sys3="$(system_code "$system")"
  site3="$(site_code "$site2")"

  devsuffix=""
  pwd="$PROD_PWD"
  if [[ "$is_dev" == "yes" ]]; then
    devsuffix="dev"
    pwd="$DEV_PWD"
  fi

  jks="${site3}gpp${sys3}${devsuffix}-lv1.${site2}.gemini.edu.jks"

  echo "=== $archive -> $jks ==="

  workdir="$(mktemp -d)"
  trap 'rm -rf "$workdir"' EXIT

  tar xzf "$archive" -C "$workdir"

  cert="$(ls "$workdir"/*.cer.pem)"
  key="$(ls "$workdir"/*.key.pem)"
  alias="$(basename "$cert" .cer.pem)"   # e.g. observe.cl.gemini.edu

  p12="$workdir/store.p12"

  # Build the PKCS12 store from the leaf cert + private key.
  openssl pkcs12 -export \
    -in "$cert" -inkey "$key" \
    -out "$p12" -name "$alias" \
    -password "pass:$pwd"

  # Fresh JKS each run.
  rm -f "$jks"

  keytool -importkeystore \
    -deststorepass "$pwd" -destkeypass "$pwd" -destkeystore "$jks" \
    -srckeystore "$p12" -srcstoretype PKCS12 -srcstorepass "$pwd" \
    -alias "$alias"

  echo "--- $jks contents ---"
  keytool -list -keystore "$jks" -storepass "$pwd"

  rm -rf "$workdir"
  trap - EXIT
  echo
done

echo "Done."
