# load tokens file
if [[ -f ~/.secrets ]]; then
    source ~/.secrets
fi

# default editor
export EDITOR="vim"

# make sure we use UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

# Go stuff
export GOPATH=~/go

# Other Exports
export PATH=~/.local/bin:/usr/local/bin:~/bin:/usr/local/sbin:$PATH
export HOMEBREW_GITHUB_API_TOKEN
export WORKON_HOME

# Docker convenience commands
# Return the last-run container ID.
alias dlc='docker ps -l -q'
# Remove all stopped containers
alias drm="docker ps -a | grep Exited | cut -d ' ' -f 1 | xargs docker rm"
# Remove all untagged images
alias drmi='docker rmi $(docker images | grep "^<none>" | awk "{print $3}")'

# Rust
if [[ -d $HOME/.cargo ]]; then
    source $HOME/.cargo/env
fi

# Load local env file
if [ -f ~/.zshenv-local ]; then
  source ~/.zshenv-local
fi
