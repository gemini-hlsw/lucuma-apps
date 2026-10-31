#!/usr/bin/env bash
# Upload the Explore build in heroku/static to an S3 bucket and invalidate its CloudFront distribution.
#
# Usage: ./deploy-aws.sh <bucket> <cloudfront-distribution-id>
#
# Expects AWS credentials in the environment: the GitHub OIDC role in CI, an access key locally.
# Run `pnpm explore build` first.

set -euo pipefail

bucket=${1:?S3 bucket name}
distribution=${2:?CloudFront distribution id}
static="$(cd "$(dirname "$0")" && pwd)/heroku/static"

if [[ ! -f "${static}/index.html" ]]; then
  echo "No Explore build found in ${static}; run 'pnpm explore build' first" >&2
  exit 1
fi

# Vite hashes everything under assets/, so a given name never changes content and browsers may keep it forever.
aws s3 sync "${static}/assets" "s3://${bucket}/assets" --delete \
  --cache-control "public, max-age=31536000, immutable"

# Everything else (index.html, the service worker, environments.conf.json...) must be revalidated on every load.
aws s3 sync "${static}" "s3://${bucket}" --delete --exclude "assets/*" \
  --cache-control "no-cache"

# The aws cli does not know the webmanifest extension and would upload it as binary/octet-stream.
aws s3 cp "${static}/manifest.webmanifest" "s3://${bucket}/manifest.webmanifest" \
  --cache-control "no-cache" --content-type "application/manifest+json"

aws cloudfront create-invalidation --distribution-id "${distribution}" --paths "/*" \
  --query "Invalidation.Id" --output text
