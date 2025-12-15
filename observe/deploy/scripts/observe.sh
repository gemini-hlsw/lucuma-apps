#!/bin/bash

SCRIPTS_DIR=~/observe

# Check if an argument was received
if [ "$#" -ne 1 ]; then
    echo "ERROR: You must provide one of the following arguments: start, stop, update or help."
    exit 1
fi

bold=$(tput bold)
normal=$(tput sgr0)

# Get the argument
option="$1"

case "$option" in
    start)
        echo "Starting Observe"
        $SCRIPTS_DIR/start.sh
        ;;
    stop)
        echo "Stoping Observe"
        $SCRIPTS_DIR/stop.sh
        ;;
    restart)
        echo "Restarting Observe"
        $SCRIPTS_DIR/restart.sh
        ;;
    update)
        echo "Updating Observe"
        $SCRIPTS_DIR/update.sh
        ;;
    help)
        echo "To run Observe you should provide a valid agument"
        echo "Possible argument options are 'start', 'stop', 'update' and 'help'"
        echo -e "  ${bold}start${normal}: Will start Observe containers"
        echo -e "  ${bold}stop${normal}: Will stop Observe containers"
        echo -e "  ${bold}restart${normal}: Will restart Observe containers"
        echo -e "  ${bold}update${normal}: Will stop and remove Observe containers, pull the latest version for each docker image and recreate the containers"
        echo -e "  ${bold}help${normal}: Will show this message"
        ;;
    *)
        echo "Error: Invalid argument, use 'help' command for instructions."
        exit 1
        ;;
esac