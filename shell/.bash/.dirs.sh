#!/bin/bash

# useful directories paths

# this script exports variables with the location of some useful
# directories

# Google Drive folder is mounted, with crontab, on init. Use this
# command to umount it
alias drive-umount="sudo umount -f ${HOME}/Drive/"

# folder that stores version-controlled projects
export DIR_VC="${HOME}/vc"
# repository that stores dotfiles
export DIR_DOTFILES="${DIR_VC}/dotfiles"
# folder that contains bash configuration files
export DIR_BASH="${HOME}/.bash"
