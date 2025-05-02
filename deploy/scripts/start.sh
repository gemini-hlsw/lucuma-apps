#!/bin/bash

SCRIPTS_DIR=~/observe

. $SCRIPTS_DIR/config.sh

IMAGE=noirlab/gpp-obs:$VERSION

echo "Starting Observe Server from image [$IMAGE]..."
docker run --name observe-server -d --network host --env SITE=$SITE --mount type=bind,src=/home/software/observe/conf,dst=/opt/docker/conf/local $IMAGE