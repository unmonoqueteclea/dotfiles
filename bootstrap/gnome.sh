#!/usr/bin/env bash

# GNOME configuration

# scaling factor
# see https://wiki.archlinux.org/title/HiDPI
# GNOME ignores X settings due to its xsettings Plugin in Gnome
gsettings set org.gnome.settings-daemon.plugins.xsettings overrides "[{'Gdk/WindowScalingFactor', <2>}]"
gsettings set org.gnome.desktop.interface scaling-factor 2
gsettings set org.gnome.desktop.background picture-uri-dark "file:///home/unmonoqueteclea/.xmonad/wallpapers/emacs.png"

