#!/bin/bash

# useful directories paths

# this script exports variables with the location of some useful
# directories

# Google Drive folder is usually mounted with crontab on init.
export DIR_SYNC="${HOME}/Drive"
alias drive-reconnect="rclone config reconnect drive:"
# we use attr-timeout to cache attributes. We configured a high value, it can cause
# corruption if files changes on the remote (in my case, that is not likely to happen)
# see https://rclone.org/commands/rclone_mount/#attribute-caching
alias drive-mount="rclone mount drive: ${DIR_SYNC}/  --attr-timeout 5m --vfs-cache-mode full --vfs-cache-max-age 5h  --dir-cache-time 15m --poll-interval 5m --file-perms 0600 --vfs-write-back 10m --daemon"
alias drive-umount="sudo umount -f ${DIR_SYNC}/"

# folder that stores version-controlled projects
export DIR_VC="${HOME}/vc"
# repository that stores dotfiles
export DIR_DOTFILES="${DIR_VC}/dotfiles"
# folder that contains bash configuration files
export DIR_BASH="${HOME}/.bash"
