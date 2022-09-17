# ~/.xinitrc

# This file is sourced when running startx and other programs which
# call xinit

# Load resource file, and merge with the current settings:
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
