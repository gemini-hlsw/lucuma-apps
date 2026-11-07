# Shared helpers for the Explore site on AWS (S3 + CloudFront), sourced by explore/deploy-aws.sh,
# promote.sh and rollback-servers.sh.
#
# CI archives every dev build under builds/<git-sha>/ in the dev bucket. Each environment serves
# latest/ in its own bucket, so deploying, promoting and rolling back all copy an archive onto it.

# The aws cli pipes command output through a pager when run from a terminal; keep it inline.
export AWS_PAGER=""

# S3 bucket holding the Explore site of an environment (origin of its CloudFront distribution).
map_aws_explore_bucket() {
  case $1 in
    "dev") echo "gemini-edu-explore-dev" ;;
    "staging") echo "gemini-edu-explore-test" ;;
    "production") echo "gemini-edu-explore-prd" ;;
    *) echo "" ;;
  esac
}

# CloudFront distribution serving the Explore site of an environment.
map_aws_explore_distribution() {
  case $1 in
    "dev") echo "E3DZG7GNUB60T6" ;;
    "staging") echo "E2E5A5UYD8VLF7" ;;
    "production") echo "E1VDJ0RPRW1PVL" ;;
    *) echo "" ;;
  esac
}

aws_explore_archive() {
  echo "s3://$(map_aws_explore_bucket dev)/builds/$1"
}

# Copy a build onto a target prefix, server side.
#
# Uses `cp`, not `sync`: sync skips same-sized files when the source is older, which is always the
# case when going back to an earlier build, and would keep the newer index.html and sw.js. Assets
# go first so the new index.html never references bundles that don't exist yet, and stale files
# are only removed once the new build is complete. Tags are not copied: we don't use them, and
# copying them on large objects needs s3:GetObjectTagging, which developers don't have.
copy_explore_build() {
  local source=$1
  local target=$2
  aws s3 cp "$source/assets" "$target/assets" --recursive --only-show-errors \
    --copy-props metadata-directive &&
    aws s3 cp "$source" "$target" --recursive --only-show-errors --exclude "assets/*" \
      --copy-props metadata-directive &&
    aws s3 sync "$source" "$target" --delete --only-show-errors --copy-props metadata-directive
}

# Invalidate a whole distribution, printing the invalidation id.
invalidate_explore_distribution() {
  aws cloudfront create-invalidation --distribution-id "$1" --paths "/*" \
    --query "Invalidation.Id" --output text
}

# Fails unless the aws cli is installed and the build of a commit is archived.
check_explore_archive() {
  local sha=$1
  if ! command -v aws >/dev/null; then
    echo "  ! aws CLI not found; install it and set AWS_PROFILE to a profile with access to the Explore buckets"
    return 1
  fi
  if ! aws s3 ls "$(aws_explore_archive "$sha")/index.html" >/dev/null; then
    echo "  ! Build $sha is not archived at $(aws_explore_archive "$sha"); deploy it to dev again to archive it"
    return 1
  fi
}

# Serve the archived build of a commit in an environment: copy it onto latest/ in the
# environment's bucket, the origin path its distribution serves, and invalidate the distribution.
deploy_explore_build() {
  local sha=$1
  local env=$2
  local bucket distribution
  bucket=$(map_aws_explore_bucket "$env")
  distribution=$(map_aws_explore_distribution "$env")

  if [ -z "$bucket" ] || [ -z "$distribution" ]; then
    echo "  ! No AWS bucket or distribution configured for Explore in $env"
    return 1
  fi
  check_explore_archive "$sha" || return 1

  local archive target
  archive=$(aws_explore_archive "$sha")
  target="s3://$bucket/latest"
  echo "  Copying $archive to $target..."
  if copy_explore_build "$archive" "$target"; then
    echo "  ✓ Explore build $sha copied to $target"
  else
    echo "  ! Explore AWS copy FAILED"
    return 1
  fi

  local invalidation
  if invalidation=$(invalidate_explore_distribution "$distribution"); then
    echo "  ✓ CloudFront distribution $distribution invalidated ($invalidation)"
  else
    echo "  ! CloudFront invalidation of $distribution FAILED"
    return 1
  fi
}
