#!/bin/bash

# utilities for software development

# I am building emacs from source, so I need to add an alias to open
# it easily (fullscreen)
alias emacs="${DIR_SOURCES}/emacs/src/emacs --debug-init"

# pyenv configuration for generating Python environments
# pyenv lets you easily switch between multiple versions of Python.
# see https://github.com/pyenv/pyenv (this repo is cloned in $DIR_VC
# we are also using the plugin pyenv-virtualenv for creating pyenv
# virtual environments
# see https://github.com/pyenv/pyenv-virtualenv
export PYENV_ROOT="${DIR_VC}/pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# always use new docker compose version
export COMPOSE='docker compose'

# configure node version manager, for generating nodejs environments
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # load nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # load nvm bash_completion

# opencv (built from source)
export OPENCV_INCLUDE_PATH="/usr/local/include/opencv4"
export OPENCV_LIBRARY_PATH="/usr/local/lib"

# create a tunnel to a specified port
# like ngrok but without the need of any client or account
# another alternative would be https://malai.sh/
tunnel() {
    echo "Creating a tunnel for localhost port $1"
    ssh -R 80:localhost:$1 localhost.run
}

# print my public ip
alias myip='curl l2.io/ip && echo ""'

# internet speed test
alias speedtest='curl -s https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | python -'

# information about the computer
alias osinfo='curl -sL https://raw.githubusercontent.com/dylanaraps/neofetch/master/neofetch | bash'

# CRON puts logs in /var/log/syslog
alias cronlogs='tail -f /var/log/syslog | grep CRON'

# export environment variables from .env file
alias env-export='export $(grep -v '^#' .env | xargs)'


# CARGO is Rust package manager, this is introduced by
# uv installation (that needs Rust)
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi
