# load tokens file
if [[ -f ~/.secrets ]]; then
    source ~/.secrets
fi

# default editor
export EDITOR="vim"

# Go stuff
export GOPATH=~/Go

# Other Exports
export PATH=/usr/local/bin:~/bin:/usr/local/sbin:$PATH
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
export PATH=~/.cargo/bin:$PATH
if [[ -d $HOME/.cargo ]]; then
    source $HOME/.cargo/env
fi

# Load local env file
if [ -f ~/.zshenv-local ]; then
  source ~/.zshenv-local
fi
