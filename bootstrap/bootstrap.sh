#!/usr/bin/env bash

# instractions that only needs to run once to setup the system

# find script location, so that we can create paths like this:
# cat "$script_dir/my_file"
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)

sudo apt-get upadte

# configure crontab
crontab $script_dir../crontab/crontab

# GNU Stow is required to create links to repo files from
# your home directory
sudo apt-get install stow make
# run stow
cd $script_dir/../ && make stow

# configure gnome settings
cd $script_dir && source ./gnome.sh

