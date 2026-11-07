#!/usr/bin/env bash
# Upload the Explore build in heroku/static to an S3 bucket and invalidate its CloudFront distribution.
#
# The build is uploaded under builds/<git-sha>/, an immutable copy that promotions and rollbacks
# copy from, and then copied server side onto latest/, the origin path the distribution serves.
#
# Usage: ./deploy-aws.sh <bucket> <cloudfront-distribution-id>
#
# Expects AWS credentials in the environment: the GitHub OIDC role in CI, an access key locally.
# Run `pnpm explore build` first.

set -euo pipefail

# The aws cli pipes command output through a pager when run from a terminal; keep it inline.
export AWS_PAGER=""

bucket=${1:?S3 bucket name}
distribution=${2:?CloudFront distribution id}
explore_dir="$(cd "$(dirname "$0")" && pwd)"
static="${explore_dir}/heroku/static"
sha=$(git -C "${explore_dir}" rev-parse HEAD)

if [[ ! -f "${static}/index.html" ]]; then
  echo "No Explore build found in ${static}; run 'pnpm explore build' first" >&2
  exit 1
fi

archive="s3://${bucket}/builds/${sha}"
latest="s3://${bucket}/latest"

# Upload the build once, to its immutable archive, with the cache headers each kind of file needs.
echo "Archiving build ${sha} to ${archive}"

# Vite hashes everything under assets/, so a given name never changes content and browsers may keep it forever.
aws s3 sync "${static}/assets" "${archive}/assets" --delete \
  --cache-control "public, max-age=31536000, immutable"

# Everything else (index.html, the service worker, environments.conf.json...) must be revalidated on every load.
aws s3 sync "${static}" "${archive}" --delete --exclude "assets/*" \
  --cache-control "no-cache"

# The aws cli does not know the webmanifest extension and would upload it as binary/octet-stream.
aws s3 cp "${static}/manifest.webmanifest" "${archive}/manifest.webmanifest" \
  --cache-control "no-cache" --content-type "application/manifest+json"

# Then copy the archive onto latest/, server side, the same way promotions do. This uses `cp`, not
# `sync`: sync skips same-sized files when the source is older (e.g. redeploying an earlier build)
# and would keep the newer index.html and sw.js. Assets go first so the new index.html never
# references bundles that don't exist yet, and stale files are only removed at the end. Tags are not
# copied: we don't use them, and copying them on large objects needs s3:GetObjectTagging, which
# developers don't have.
echo "Deploying build ${sha} to ${latest}"
aws s3 cp "${archive}/assets" "${latest}/assets" --recursive --only-show-errors \
  --copy-props metadata-directive
aws s3 cp "${archive}" "${latest}" --recursive --only-show-errors --exclude "assets/*" \
  --copy-props metadata-directive
aws s3 sync "${archive}" "${latest}" --delete --only-show-errors --copy-props metadata-directive

aws cloudfront create-invalidation --distribution-id "${distribution}" --paths "/*" \
  --query "Invalidation.Id" --output text
