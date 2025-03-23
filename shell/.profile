#!/bin/bash

# bash configuration file for login shells

# almost all content of this file is copied from the default
# .profile file in Ubuntu 22.04

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022


 # get the aliases and functions in non-interactive sheels
. "$HOME/.bashrc"

# CARGO is Rust package manager, this is introduced by
# uv installation (that needs Rust)
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
