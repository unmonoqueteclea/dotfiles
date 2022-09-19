# ~/.xinitrc

# This file is sourced when running startx and other programs which
# call xinit.

# IMPORTANT: By default, X files are ignored when using default's
# Ubuntu display server: wayland.

# Load resource file, and merge with the current settings:
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
