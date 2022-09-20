#!/bin/bash

# useful directories paths

# this script exports variables with the location of some useful
# directories

# Google Drive folder is usually mounted with crontab on init.
export DIR_SYNC="${HOME}/Drive"
alias drive-reconnect="rclone config reconnect drive:"
alias drive-mount="rclone mount drive: ${DIR_SYNC}/  --vfs-cache-mode full --file-perms 0600 --daemon"
alias drive-umount="sudo umount -f ${DIR_SYNC}/"

# folder that stores version-controlled projects
export DIR_VC="${HOME}/vc"
# repository that stores dotfiles
export DIR_DOTFILES="${DIR_VC}/dotfiles"
# folder that contains bash configuration files
export DIR_BASH="${HOME}/.bash"
