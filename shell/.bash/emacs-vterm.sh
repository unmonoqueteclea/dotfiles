#!/bin/bash

# shell-side configuration for Emcas vterm
# from https://github.com/akermu/emacs-libvterm/blob/master/etc/emacs-vterm-bash.sh

# Some of the most useful features in emacs-libvterm require shell-side
# configurations. The main goal of these additional functions is to enable the
# shell to send information to `vterm` via properly escaped sequences. A
# function that helps in this task, `vterm_printf`, is defined below.

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

# Sync directory and host in the shell with Emacs's current directory.
# You may need to manually specify the hostname instead of $(hostname) in case
# $(hostname) does not return the correct string to connect to the server.
#
# The escape sequence "51;A" has also the role of identifying the end of the
# prompt
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'
