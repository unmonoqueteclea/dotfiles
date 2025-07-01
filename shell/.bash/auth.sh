 #!/usr/bin/env bash

# auth and credential related utilities
export SSH_KEYS_DIR="${DIR_SYNC}/access/ssh"

# see https://www.andrewjstevens.com/posts/2021/12/use-a-keychain-to-remember-your-ssh-password/
# keychain -q --agents ssh --nogui --timeout 10080 $SSH_KEYS_DIR/id_rsa
