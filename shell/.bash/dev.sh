#!/bin/bash

# utilities for software development

# we are building emacs from source, so we need an alias
# to open it easily
alias emacs="${DIR_VC}/emacs/src/emacs -fs --debug-init"

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

# automatically login docker to BigML registry
bigml_docker_login() {
    echo $DOCKER_BIGML_PASSWORD | docker login -u $DOCKER_BIGML_USERNAME --password-stdin 
}

# configure node version manager
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
