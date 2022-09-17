# ⚙️ unmonoqueteclea's dotfiles

These are the dotfiles that powers my daily work.  I use `Ubuntu
22.04` (although sometimes I test them on `macOS`. My `GNU Emacs`
configuration is also included in the repository.

## quickstart
Just do:

```sh
cd ./bootstrap && ./bootstrap.sh

```

That script will run all the needed initialization tasks, including
installing `GNU Stow` to create links to repo files from your home
directory.

## synced data folder
Some of these configurations assumed a synced data folder. I use
`Google Drive`, with `rclone` helping me to synchronize the files. 
