#!/usr/bin/env bash
# Roll back SSO, ITC, ODB and RESOURCE in a given environment to a given commit SHA.
# If SHA is not provided, list SHAs appearing in the last 10 deployment records for the environment.
# Usage: rollback-servers.sh <env> [<commit-sha>] [--only <systems>] [--debug]
#
# --only takes a comma separated list of systems, e.g. `--only ODB,ITC`. Explore is only rolled
# back when named there, and on its own: it lives in lucuma-apps, so its commit SHA can't be
# shared with the servers. It needs AWS credentials (AWS_PROFILE) and only changes the AWS site,
# not Firebase.
#   rollback-servers.sh production 1e29b27d1983f700ad0a1bf4613e1cdda129e054 --only Explore

# Vibe coded 🎉 with GPT-5 mini

set -e

source "$(dirname "${BASH_SOURCE[0]}")/explore/aws-explore.sh"

USAGE="Usage: rollback-servers.sh <env> [<commit-sha>] [--only <systems>] [--debug]"
ENV=""
SHA=""
ONLY=""
DEBUG=false
DEBUG_CURL=()

while [ $# -gt 0 ]; do
  case $1 in
    --debug) DEBUG=true; DEBUG_CURL=("-v") ;;
    --only)
      ONLY=${2:-}
      if [ -z "$ONLY" ]; then echo "$USAGE"; exit 1; fi
      shift
      ;;
    --*) echo "Unknown option $1"; echo "$USAGE"; exit 1 ;;
    *)
      if [ -z "$ENV" ]; then ENV=$1
      elif [ -z "$SHA" ]; then SHA=$1
      else echo "$USAGE"; exit 1
      fi
      ;;
  esac
  shift
done

if [ -z "$ENV" ]; then
  echo "$USAGE"
  exit 1
fi

VALID_ENVS=("dev" "staging" "production")
if ! [[ " ${VALID_ENVS[*]} " =~ " ${ENV} " ]]; then
  echo "Error: Invalid environment. Must be one of: ${VALID_ENVS[*]}"
  exit 1
fi

# GitHub auth: prefer an explicit GPP_GITHUB_TOKEN, otherwise borrow the credential `gh` already
# holds. Nothing to export by hand and nothing to expire in a drawer.
if [ -z "${GPP_GITHUB_TOKEN:-}" ]; then
  GPP_GITHUB_TOKEN=$(gh auth token 2>/dev/null || true)
fi

if [ -z "$GPP_GITHUB_TOKEN" ]; then
  echo "Error: GPP_GITHUB_TOKEN is not set and \`gh auth token\` gave nothing. Run \`gh auth login\`."
  exit 1
fi

confirm() {
  local reply
  while true; do
    read -r -p "WARNING: This script WILL NOT undo a DB migration. Make sure you are rolling back to a commit that supports the currently deployed DB migration. Proceed [y/N]: " reply
    case "${reply,,}" in
      y|yes) return 0 ;;
      n|no)  return 1 ;;
      "")    return 1 ;;  # default NO
      *) echo "Please answer y or n." ;;
    esac
  done
}

map_github_deploy_env() {
  case $1 in
    "dev") echo "development" ;;
    "staging") echo "staging" ;;
    "production") echo "production" ;;
    *) echo "$1" ;;
  esac
}

declare -A repo
declare -A image_name
declare -A process_types

repo["SSO"]="gemini-hlsw/lucuma-odb"
image_name["SSO"]="lucuma-sso"
process_types["SSO"]="web"

repo["ITC"]="gemini-hlsw/lucuma-odb"
image_name["ITC"]="itc"
process_types["ITC"]="web"

repo["ODB"]="gemini-hlsw/lucuma-odb"
image_name["ODB"]="lucuma-postgres-odb"
process_types["ODB"]="web calibration obscalc pdfsummary"

repo["RESOURCE"]="gemini-hlsw/lucuma-odb"
image_name["RESOURCE"]="lucuma-resource"
process_types["RESOURCE"]="web"

repo["Explore"]="gemini-hlsw/lucuma-apps"

docker_systems=("SSO" "ITC" "ODB" "RESOURCE")
all_systems=("${docker_systems[@]}" "Explore")
deploy_env=$(map_github_deploy_env "$ENV")

# Systems to roll back: the servers by default, or the --only list matched case-insensitively.
systems=()
if [ -z "$ONLY" ]; then
  systems=("${docker_systems[@]}")
else
  IFS=',' read -r -a requested <<< "$ONLY"
  for name in "${requested[@]}"; do
    match=""
    for system in "${all_systems[@]}"; do
      if [ "${system,,}" = "${name,,}" ]; then match=$system; fi
    done
    if [ -z "$match" ]; then
      echo "Error: Unknown system '$name'. Must be one of: ${all_systems[*]}"
      exit 1
    fi
    systems+=("$match")
  done
fi

rollback_explore=false
rollback_servers=false
for system in "${systems[@]}"; do
  if [ "$system" = "Explore" ]; then rollback_explore=true; else rollback_servers=true; fi
done

if [ "$rollback_explore" = true ] && [ "$rollback_servers" = true ]; then
  echo "Error: Explore is versioned in ${repo["Explore"]}, not with the servers. Roll it back on its own with --only Explore."
  exit 1
fi

# GitHub curl base options (re-usable)
gh_curl_opts=("-s" "--fail-with-body" "-H" "Accept: application/vnd.github.v3+json" "-H" "Authorization: Bearer $GPP_GITHUB_TOKEN")
gh_curl_opts+=( "${DEBUG_CURL[@]}" )

# Heroku curl base options (uses netrc for auth)
heroku_curl_base=("-s" "--netrc" "-H" "Content-Type: application/json" "-H" "Accept: application/vnd.heroku+json; version=3.docker-releases")
heroku_curl_base+=( "${DEBUG_CURL[@]}" )

# If SHA not provided: list unique SHAs (preserve order) from recent deployment records per repo, and show created_at
if [ -z "$SHA" ]; then
  echo "Listing commit SHAs from recent deployments for environment '$ENV' (newest first):"
  declare -A seen_repo=()
  for system in "${systems[@]}"; do
    repo_name=${repo["$system"]}
    if [ "${seen_repo[$repo_name]+_}" ]; then
      continue
    fi
    seen_repo[$repo_name]=1

    # list which services use this repo
    services=""
    for s in "${systems[@]}"; do
      if [ "${repo[$s]}" = "$repo_name" ]; then
        services="$services $s"
      fi
    done
    services=$(echo "$services" | sed 's/^ //')

    echo
    echo "==> $repo_name (services:$services)"
    # fetch a larger page to increase chance of getting 10 unique SHAs (there are duplicates)
    # lucuma-apps records several apps per environment, so only list the Explore ones.
    task_filter=""
    if [ "$system" = "Explore" ]; then task_filter="&task=deploy:Explore"; fi
    if gh_output=$(curl "${gh_curl_opts[@]}" "https://api.github.com/repos/$repo_name/deployments?environment=${deploy_env}${task_filter}&per_page=50"); then
      if [ "$DEBUG" = true ]; then echo "  *** GITHUB RESPONSE: $gh_output"; fi

      # emit sha<TAB>created_at for each deployment, preserve order, then dedupe by sha while preserving first occurrence
      echo "$gh_output" | jq -r '.[] | "\(.sha)\t\(.created_at)"' 2>/dev/null | \
        awk -F"\t" '!seen[$1]++ { printf "%2d. %s  — %s\n", ++i, $1, $2; if (i==10) exit }' || {
          echo "  (no deployments found or parsing error)"
        }
    else
      echo "  ! Failed to query GitHub for deployments of $repo_name"
    fi
  done
  echo
  echo "To perform a rollback, re-run with the desired SHA: rollback-servers.sh <env> <commit-sha> [--only <systems>]"
  exit 1
fi

confirm() {
  local reply
  while true; do
    read -r -p "WARNING: This script WILL NOT undo a DB migration. Make sure you are rolling back to a commit that supports the currently deployed DB migration. Proceed [y/N]: " reply
    case "${reply,,}" in
      y|yes) return 0 ;;
      n|no)  return 1 ;;
      "")    return 1 ;;  # default NO
      *) echo "Please answer y or n." ;;
    esac
  done
}

confirm_explore() {
  local reply
  read -r -p "This replaces the Explore site users load in $ENV on AWS (not Firebase). Proceed [y/N]: " reply
  [[ "${reply,,}" =~ ^y(es)?$ ]]
}

# Resolve the Explore build before asking, so a wrong SHA fails before any confirmation.
if [ "$rollback_explore" = true ]; then
  echo "Resolving Explore build for $SHA"
  if ! commit_out=$(curl "${gh_curl_opts[@]}" "https://api.github.com/repos/${repo["Explore"]}/commits/${SHA}"); then
    echo "  ! Commit $SHA not found in ${repo["Explore"]}"
    exit 1
  fi
  SHA=$(echo "$commit_out" | jq -r '.sha')
  echo "  Commit: $SHA"
  check_explore_archive "$SHA" || exit 1
  echo "  ✓ Build archived at $(aws_explore_archive "$SHA")"
fi

if [ "$rollback_servers" = true ] && ! confirm ; then
  exit 1
fi
if [ "$rollback_explore" = true ] && ! confirm_explore ; then
  exit 1
fi

send_slack_notification() {
  local service=$1
  local env=$2
  local sha=$3
  local channel=${GPP_SLACK_CHANNEL:-"#gpp"}

  if [ -z "$GPP_SLACK_WEBHOOK_URL" ]; then
    echo "  ! GPP_SLACK_WEBHOOK_URL not set - skipping Slack notification"
    return 0
  fi

  if [[ ! "$GPP_SLACK_WEBHOOK_URL" =~ ^https://hooks\.slack\.com/services/ ]]; then
    echo "  ! Invalid Slack webhook URL format - skipping notification"
    return 0
  fi

  local timestamp
  timestamp=$(date -u "+%Y-%m-%d %H:%M:%S UTC")
  local text=":rewind: ${service} rolled back in ${env} to commit ${sha}\nTime: ${timestamp}"
  local payload
  payload=$(printf '{"text":"%s"}' "$text")

  if curl -s -X POST -H 'Content-type: application/json' --data "$payload" "$GPP_SLACK_WEBHOOK_URL" > /dev/null; then
    echo "  Slack notification sent to $channel"
  else
    echo "  ! Failed to send Slack notification (continuing)"
  fi
}

record_github_rollback() {
  local system=$1
  local payload_object=$2
  local repo_name=${repo["$system"]}

  echo "  Recording deployment on GitHub..."
  local payload
  payload=$(printf '{"ref":"%s","auto_merge":false,"original_environment":"%s","environment":"%s","description":"Rollback to %s","required_contexts":[],"task":"deploy:%s"%s}' \
    "$SHA" "$deploy_env" "$deploy_env" "$SHA" "$system" "$payload_object")

  local gh_post_out gh_post_ok
  if gh_post_out=$(curl "${gh_curl_opts[@]}" -X POST "https://api.github.com/repos/$repo_name/deployments" -d "$payload"); then
    gh_post_ok=true
  else
    gh_post_ok=false
  fi

  if [ "$DEBUG" = true ]; then echo "  *** GITHUB POST RESPONSE: $gh_post_out"; fi

  if [ "$gh_post_ok" = true ] && echo "$gh_post_out" | grep -q '"id"'; then
    echo "  ✓ GitHub deployment record created for $system"
    send_slack_notification "$system" "$ENV" "$SHA"
  else
    echo "  ! Failed to create GitHub deployment record: $gh_post_out"
    exit 1
  fi
}

# Explore has no Heroku release: roll back by serving the build CI archived for the commit.
if [ "$rollback_explore" = true ]; then
  echo "Rolling back Explore in $ENV to commit $SHA"
  echo
  echo "##### Applying"
  echo "==> Explore"
  deploy_explore_build "$SHA" "$ENV" || exit 1
  record_github_rollback "Explore" ""
  echo
  echo "Firebase still serves the previous Explore build in $ENV."
  exit 0
fi

echo "Rolling back ${systems[*]} in $ENV to commit $SHA"

# Two phases. Resolving every system before touching anything means a missing or incomplete
# deployment record aborts with nothing changed, rather than leaving dev on a mix of versions
# that was never tested together.

declare -A resolved_shas

echo
echo "##### Resolving deployments"

for system in "${systems[@]}"; do
  repo_name=${repo["$system"]}

  echo "==> $system"
  echo "  Fetching GitHub deployment for ref=$SHA ..."
  if ! gh_output=$(curl "${gh_curl_opts[@]}" "https://api.github.com/repos/$repo_name/deployments?ref=${SHA}&environment=${deploy_env}&task=deploy:${system}&per_page=1"); then
    echo "  ! Failed to query GitHub for deployment of $system at $SHA"
    exit 1
  fi

  if [ "$DEBUG" = true ]; then echo "  *** GITHUB RESPONSE: $gh_output"; fi

  docker_shas_json=$(echo "$gh_output" | jq -c '.[0].payload.docker_image_shas // empty' || true)
  if [ -z "$docker_shas_json" ]; then
    echo "  ! No docker_image_shas found in deployment payload for $system@$SHA"
    exit 1
  fi

  # Check every process type up front, so the apply phase cannot fail on a missing entry.
  IFS=' ' read -r -a proc_types <<< "${process_types["$system"]}"
  for proc in "${proc_types[@]}"; do
    docker_image_sha=$(echo "$docker_shas_json" | jq -r --arg p "$proc" '.[$p] // "none"')
    if [ "$docker_image_sha" = "none" ]; then
      echo "  ! No docker image SHA for process '$proc' in payload - cannot proceed."
      exit 1
    fi
    echo "  $proc -> $docker_image_sha"
  done

  resolved_shas["$system"]=$docker_shas_json
done

echo
echo "##### Applying"

for system in "${systems[@]}"; do
  echo "==> $system"

  repo_name=${repo["$system"]}
  base_name="${image_name["$system"]}-${ENV}"
  docker_shas_json=${resolved_shas["$system"]}

  IFS=' ' read -r -a proc_types <<< "${process_types["$system"]}"
  for proc in "${proc_types[@]}"; do
    docker_image_sha=$(echo "$docker_shas_json" | jq -r --arg p "$proc" '.[$p]')

    echo "  Deploying ${base_name}/${proc} -> $docker_image_sha"
    body=$(printf '{"updates":[{"type":"%s","docker_image":"%s"}]}' "$proc" "$docker_image_sha")

    if heroku_out=$(curl "${heroku_curl_base[@]}" -X PATCH "https://api.heroku.com/apps/${base_name}/formation" -d "$body"); then
      heroku_ok=true
    else
      heroku_ok=false
    fi

    if [ "$DEBUG" = true ]; then echo "  *** HEROKU RESPONSE: $heroku_out"; fi

    if [ "$heroku_ok" = false ]; then
      echo "  ! Error deploying ${base_name}/${proc} to Heroku: $heroku_out"
      echo "  ! STOPPING. Systems already rolled back may differ from those not yet processed."
      exit 1
    else
      echo "  ✓ Deployed ${base_name}/${proc}"
    fi
  done

  record_github_rollback "$system" ", \"payload\":{\"docker_image_shas\":$docker_shas_json}"

  echo
done
