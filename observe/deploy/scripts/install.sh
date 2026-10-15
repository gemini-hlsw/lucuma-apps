#!/bin/bash

bold=$(tput bold)
normal=$(tput sgr0)

# A stale copy of this script would miss files added since it was downloaded, so refuse to
# run unless it matches the one on main. $(...) strips trailing newlines on both sides.
INSTALL_URL=https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/install.sh
if LATEST_INSTALL=$(curl -sSf "$INSTALL_URL"); then
    if [ "$LATEST_INSTALL" != "$(cat "${BASH_SOURCE[0]}")" ]; then
        echo "This install.sh is out of date. Download the current one and run it instead:"
        echo
        echo "    curl $INSTALL_URL >install.sh"
        echo "    chmod +x install.sh"
        echo "    ./install.sh"
        exit 1
    fi
else
    echo "WARNING: Could not check whether install.sh is up to date. Continuing with this copy."
fi

mkdir ~/observe
cd ~/observe
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/config.sh.template >config.sh.template
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/start.sh >start.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/stop.sh >stop.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/restart.sh >restart.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/update.sh >update.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe/deploy/scripts/observe.sh >observe.sh
chmod +x *.sh
mkdir ~/observe/conf
mkdir ~/observe/log

# Creating symbolic link to run observe as a command
echo "Creating symbolic link to run observe as a command"
mkdir -p ~/bin
ln -s ~/observe/observe.sh ~/bin/observe
echo "Observe command installed"
echo "For more information run"
echo "  >> ${bold}observe help${normal}"