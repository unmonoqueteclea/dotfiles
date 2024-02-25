 #!/usr/bin/env bash

# auth and credential related utilities
export SSH_KEYS_DIR="${DIR_SYNC}/access/ssh"

# when using GNOME, it ensures that the ssh-agent
# (that will remember ssh credentials) is always running. However, in Xmonad
# this doesn't happen, so we have to initialize it manually.
# Here, we are also asking for the password of my main SSH key.
# We could remove the `--eval` and wait until the password
# is first prompted
# timeout is defined in minutes (currently: 7 days)
eval "$(keychain --agents ssh --nogui --timeout 10080 --eval $SSH_KEYS_DIR/id_rsa)"
eval "$(keychain --agents ssh --nogui --timeout 10080  $SSH_KEYS_DIR/bigml/bigml_ssh)"
