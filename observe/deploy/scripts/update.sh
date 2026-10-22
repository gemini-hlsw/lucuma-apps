#!/bin/bash

SCRIPTS_DIR=~/observe

usage() {
    echo "Usage: observe update [--force-version <tag>]"
    echo "  Without arguments, deploys the noirlab/gpp-obs tag last promoted to this server's"
    echo "  environment (staging or production, taken from SITE in config.sh)."
    echo "  --force-version <tag> deploys the given tag instead, after asking for confirmation."
}

FORCED_TAG=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        --force-version)
            if [ -z "$2" ]; then
                usage
                exit 1
            fi
            FORCED_TAG="$2"
            shift 2
            ;;
        *)
            echo "ERROR: Unexpected argument [$1]"
            echo
            usage
            exit 1
            ;;
    esac
done

. $SCRIPTS_DIR/config.sh

ENVIRONMENT="${SITE%-*}"
case "$ENVIRONMENT" in
    staging|production) ;;
    *)
        echo "ERROR: Cannot determine the environment of this server from SITE [$SITE] in $SCRIPTS_DIR/config.sh."
        echo "  SITE must be one of staging-gn, staging-gs, production-gn or production-gs."
        exit 1
        ;;
esac

# The tag promote.sh recorded for this environment in the GitHub deployment records.
# No jq on the servers, so the fields are pulled out of the pretty-printed JSON.
PROMOTION_URL="https://api.github.com/repos/gemini-hlsw/lucuma-apps/deployments?environment=$ENVIRONMENT&task=deploy:Observe&per_page=1"
PROMOTED_TAG=""
if PROMOTION=$(curl -sSf -H 'Accept: application/vnd.github+json' "$PROMOTION_URL"); then
    PROMOTED_TAG=$(echo "$PROMOTION" | grep -o '"docker_tag": *"[^"]*"' | head -1 | sed 's/.*: *"//; s/"$//')
    PROMOTED_AT=$(echo "$PROMOTION" | grep -o '"created_at": *"[^"]*"' | head -1 | sed 's/.*: *"//; s/"$//')
fi

if [ -n "$FORCED_TAG" ]; then
    TAG="$FORCED_TAG"
    if [ "$TAG" != "$PROMOTED_TAG" ]; then
        echo "You shouldn't deploy a version other than the one last promoted on staging and production servers. This may break compatibility with other systems."
        echo
        if [ -n "$PROMOTED_TAG" ]; then
            echo "The version last promoted to $ENVIRONMENT ($PROMOTED_AT) is $PROMOTED_TAG; 'observe update' deploys it."
        else
            echo "Could not determine the version last promoted to $ENVIRONMENT."
        fi
        echo
        read -r -p "If you have a REALLY REALLY good reason to deploy $TAG, type 'yes' to continue: " ANSWER
        if [ "$ANSWER" != "yes" ]; then
            echo "Aborted. Nothing was changed."
            exit 1
        fi
    fi
else
    if [ -z "$PROMOTED_TAG" ]; then
        echo "ERROR: Could not determine the version last promoted to $ENVIRONMENT from"
        echo "  $PROMOTION_URL"
        echo "Nothing was changed."
        exit 1
    fi
    TAG="$PROMOTED_TAG"
    echo "Version last promoted to $ENVIRONMENT ($PROMOTED_AT): $TAG"
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
