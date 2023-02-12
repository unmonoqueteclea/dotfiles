# ⚙ unmonoqueteclea's dotfiles

These are the dotfiles that power my daily work. My `GNU Emacs`
configuration is also included in the repository.

I use `Ubuntu 22.04` with `GNOME 42` and `gdm3` as login screen
display manager. `Wayland` is used as the default display server.
`XWayland` provides an X server running on top of wayland for X11
applications that does not support `Wayland` yet.


## quickstart
Just do:

```sh
cd ./bootstrap && ./bootstrap.sh

```

That script will run all the needed initialization tasks, including
installing `GNU Stow` to create links to repo files from your home
directory.

## other useful tweaks
### remap Caps Lock to Ctrl
Add the following to `etc/default/keyboard`

```sh
XKBOPTIONS="caps:ctrl_modifier"
```

## synced data folder
Some of these configurations assumed a synced data folder. I use
`Google Drive`, with `rclone` helping me to synchronize the files.
