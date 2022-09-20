#!/usr/bin/env bash

# instructions that only needs to run once to setup the system

# find script location, so that we can create paths like this:
# cat "$script_dir/my_file"
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)

sudo apt-get update

# configure crontab
crontab $script_dir/../crontab/crontab

# GNU Stow is required to create links to repo files from
# your home directory
sudo apt-get install stow make
# run stow
cd $script_dir/../ && make stow

# configure gnome settings
cd $script_dir && source ./gnome.sh

# for ssh auth
sudo apt-get install keychain

# xmonad packages
sudo apt-get install xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar xcompmgr
# feh is an X11 image viewer, we use it to add a wallpaper to xmonad
sudo apt-get install feh

