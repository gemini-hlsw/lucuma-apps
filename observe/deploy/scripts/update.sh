#!/bin/bash

SCRIPTS_DIR=~/observe

TAG="$1"
if [ -z "$TAG" ]; then
    echo "Usage: observe update <tag>"
    echo "  <tag> is the noirlab/gpp-obs docker tag, such as 20260916-4b3faa6d"
    exit 1
fi

. $SCRIPTS_DIR/config.sh

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
