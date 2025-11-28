#!/usr/bin/env bash
# Roll back SSO, ITC and ODB in a given environment to a given commit SHA.
# If SHA is not provided, list SHAs appearing in the last 10 deployment records for the environment.
# Usage: rollback-servers.sh <env> [<commit-sha>] [--debug]

# Vibe coded 🎉 with GPT-5 mini

set -e

ENV=$1
SHA=$2
DEBUG=false
DEBUG_CURL=()

for arg in "$@"; do
  case $arg in
    --debug) DEBUG=true; DEBUG_CURL=("-v") ;;
  esac
done

if [ -z "$ENV" ]; then
  echo "Usage: rollback-servers.sh <env> [<commit-sha>] [--debug]"
  exit 1
fi

VALID_ENVS=("dev" "staging" "production")
if ! [[ " ${VALID_ENVS[*]} " =~ " ${ENV} " ]]; then
  echo "Error: Invalid environment. Must be one of: ${VALID_ENVS[*]}"
  exit 1
fi

if [ -z "$GPP_GITHUB_TOKEN" ]; then
  echo "Error: GPP_GITHUB_TOKEN must be set"
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
process_types["ODB"]="web calibration obscalc"

docker_systems=("SSO" "ITC" "ODB")
deploy_env=$(map_github_deploy_env "$ENV")

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
  for system in "${docker_systems[@]}"; do
    repo_name=${repo["$system"]}
    if [ "${seen_repo[$repo_name]+_}" ]; then
      continue
    fi
    seen_repo[$repo_name]=1

    # list which services use this repo
    services=""
    for s in "${docker_systems[@]}"; do
      if [ "${repo[$s]}" = "$repo_name" ]; then
        services="$services $s"
      fi
    done
    services=$(echo "$services" | sed 's/^ //')

    echo
    echo "==> $repo_name (services:$services)"
    # fetch a larger page to increase chance of getting 10 unique SHAs (there are duplicates)
    if gh_output=$(curl "${gh_curl_opts[@]}" "https://api.github.com/repos/$repo_name/deployments?environment=${deploy_env}&per_page=50"); then
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
  echo "To perform a rollback, re-run with the desired SHA: rollback-servers.sh <env> <commit-sha>"
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

if ! confirm ; then
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

echo "Rolling back SSO/ITC/ODB in $ENV to commit $SHA"

for system in "${docker_systems[@]}"; do
  echo "==> Processing $system"

  repo_name=${repo["$system"]}
  base_name="${image_name["$system"]}-${ENV}"

  echo "  Fetching GitHub deployment for ref=$SHA ..."
  if gh_output=$(curl "${gh_curl_opts[@]}" "https://api.github.com/repos/$repo_name/deployments?ref=${SHA}&environment=${deploy_env}&task=deploy:${system}&per_page=1"); then
    gh_ok=true
  else
    gh_ok=false
  fi

  if [ "$DEBUG" = true ]; then echo "  *** GITHUB RESPONSE: $gh_output"; fi

  if [ "$gh_ok" = false ]; then
    echo "  ! Failed to query GitHub for deployment of $system at $SHA"
    exit 1
  fi

  # Extract docker_image_shas payload as compact JSON
  docker_shas_json=$(echo "$gh_output" | jq -c '.[0].payload.docker_image_shas // empty' || true)
  if [ -z "$docker_shas_json" ]; then
    echo "  ! No docker_image_shas found in deployment payload for $system@$SHA"
    exit 1
  fi

  echo "  Found docker image SHAs payload: $docker_shas_json"

  IFS=' ' read -r -a proc_types <<< "${process_types["$system"]}"
  for proc in "${proc_types[@]}"; do
    docker_image_sha=$(echo "$docker_shas_json" | jq -r --arg p "$proc" '.[$p] // "none"')
    if [ "$docker_image_sha" = "none" ]; then
      echo "  ! No docker image SHA for process '$proc' in payload - cannot proceed."
      exit 1
    fi

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
      exit 1
    else
      echo "  ✓ Deployed ${base_name}/${proc}"
    fi
  done

  echo "  Recording deployment on GitHub..."
  payload=$(printf '{"ref":"%s","auto_merge":false,"original_environment":"%s","environment":"%s","description":"Rollback to %s","required_contexts":[],"task":"deploy:%s","payload":{"docker_image_shas":%s}}' \
    "$SHA" "$deploy_env" "$deploy_env" "$SHA" "$system" "$docker_shas_json")

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

  echo
done

echo "Rollback complete."