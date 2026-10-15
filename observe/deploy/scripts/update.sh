#!/bin/bash

SCRIPTS_DIR=~/observe

TAG=""
FORCE=false
for arg in "$@"; do
    case "$arg" in
        --force) FORCE=true ;;
        *)
            if [ -n "$TAG" ]; then
                echo "ERROR: Unexpected argument [$arg]"
                exit 1
            fi
            TAG="$arg"
            ;;
    esac
done

if [ -z "$TAG" ]; then
    echo "Usage: observe update <tag> [--force]"
    echo "  <tag> is the noirlab/gpp-obs docker tag, such as 20260916-4b3faa6d"
    exit 1
fi

. $SCRIPTS_DIR/config.sh

if [ "$TAG" = "latest" ] && [ "$FORCE" != true ]; then
    # This server cannot reach github.com, so the promoted tag must be looked up elsewhere
    case "${SITE%-*}" in
        staging|production) ENVIRONMENT="${SITE%-*}" ;;
        *) ENVIRONMENT="<staging|production>" ;;
    esac
    echo "You shouldn't deploy 'latest' on staging and production servers. This may break compatibility with other systems."
    echo
    echo "If you have a REALLY REALLY good reason to deploy 'latest', rerun with --force."
    echo
    echo "Otherwise, use the version provided in Slack in the latest promotion."
    echo "If you can't find it there, run this on a machine with access to github.com:"
    echo
    echo "    curl -s 'https://api.github.com/repos/gemini-hlsw/lucuma-apps/deployments?environment=$ENVIRONMENT&task=deploy:Observe&per_page=1' | grep -m1 docker_tag"
    exit 1
fi

IMAGE=noirlab/gpp-obs:$TAG

echo "Logging into DockerHub..."
echo $DOCKERHUB_TOKEN | docker login -u $DOCKERHUB_USER --password-stdin

echo "Pulling image [$IMAGE]..."
if ! docker pull $IMAGE; then
    echo "ERROR: Could not pull image [$IMAGE]. Observe was not stopped."
    exit 1
fi

echo "$TAG" > $SCRIPTS_DIR/deployed-version

$SCRIPTS_DIR/stop.sh
$SCRIPTS_DIR/start.sh
