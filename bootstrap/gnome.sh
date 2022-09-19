#!/usr/bin/env bash

# GNOME configuration

# scaling factor
# see https://wiki.archlinux.org/title/HiDPI
# GNOME with Wayland ignores X settings
# force scaling factor to 0 so that wayland use the right one for each windows
gsettings set org.gnome.desktop.interface scaling-factor 0
gsettings set org.gnome.desktop.background picture-uri-dark "file:///home/unmonoqueteclea/.xmonad/wallpapers/emacs.png"

