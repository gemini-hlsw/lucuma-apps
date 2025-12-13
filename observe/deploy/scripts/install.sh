#!/bin/bash

bold=$(tput bold)
normal=$(tput sgr0)

mkdir ~/observe
cd ~/observe
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe_deploy/scripts/config.sh.template >config.sh.template
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe_deploy/scripts/start.sh >start.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe_deploy/scripts/stop.sh >stop.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe_deploy/scripts/restart.sh >restart.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe_deploy/scripts/update.sh >update.sh
curl https://raw.githubusercontent.com/gemini-hlsw/lucuma-apps/refs/heads/main/observe_deploy/scripts/observe.sh >observe.sh
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