#!/bin/bash

# useful directories paths

# this script exports variables with the location of some useful
# directories

# google drive folder is usually mounted with crontab on init.
export DIR_SYNC="${HOME}/Drive"
# a local copy of the google drive folder, for backup and quick access
# see (backup-orgmode function below)
export DIR_SYNC_LOCAL="${HOME}/DriveLocal"
alias drive-reconnect="rclone config reconnect drive:"

# we use attr-timeout to cache attributes. We configured a high value, it can cause
# corruption if files changes on the remote (in my case, that is not likely to happen)
# see https://rclone.org/commands/rclone_mount/#attribute-caching
alias drive-mount="rclone mount drive: ${DIR_SYNC}/  --attr-timeout 1m --vfs-cache-mode full --vfs-cache-max-age 1m  --dir-cache-time 1m --poll-interval 1m --file-perms 0600 --vfs-write-back 1m --daemon"
alias drive-umount="sudo umount -f ${DIR_SYNC}/"

# folder that stores version-controlled projects
export DIR_VC="${HOME}/vc"

# folder that stores software built from source
export DIR_SOURCES="${HOME}/sources"

# repository that stores dotfiles
export DIR_DOTFILES="${DIR_VC}/dotfiles"

# folder that contains bash configuration files
export DIR_BASH="${HOME}/.bash"

function backup-orgmode() {
  # This function backs up the orgmode folder from Google Drive to a local folder.
  # It uses 'rclone sync', which makes the destination identical to the source,
  # only transferring new or changed files.

  # The goal is to have a local copy of orgmode files for quick access
  # and redundancy (e.g. for grep).

  local source="${DIR_SYNC}/orgmode"
  local destination="${DIR_SYNC_LOCAL}/orgmode"

  echo "Starting backup of orgmode folder..."
  mkdir -p "${destination}"
  rclone sync "${source}" "${destination}" --create-empty-src-dirs
  echo "Backup of orgmode folder completed successfully."
}
