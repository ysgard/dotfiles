# load tokens file
if [[ -f ~/.dotfiles/tokens ]]; then
    source ~/.dotfiles/tokens
fi

# default editor
export EDITOR="vim"
# P4CONFIG
export P4CONFIG=.p4
export P4EDITOR="vim"

# Go stuff
export GOPATH=~/Go

# Haskell stuff - Cabal and Stack
HASKELL=.cabal/bin:.cabal-sandbox/bin:~/.local/bin

# Gnu Global
export GTAGSCONF=~/.globalrc
export GTAGSLABEL="ctags"

# Other Exports
export PATH=$HASKELL:$SCRIPTENV:/usr/local/bin:~/bin:/usr/local/sbin:$PATH
export HOMEBREW_GITHUB_API_TOKEN
export WORKON_HOME

# Load custom local script, if there is one:
if [ -f ~/.localenv ]; then
    source ~/.localenv
fi

# Docker convenience commands
# Return the last-run container ID.
alias dlc='docker ps -l -q'
# Remove all stopped containers
alias drm="docker ps -a | grep Exited | cut -d ' ' -f 1 | xargs docker rm"
# Remove all untagged images
alias drmi='docker rmi $(docker images | grep "^<none>" | awk "{print $3}")'

# Homebrew libraries
export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"

# Rust
export PATH=~/.cargo/bin:$PATH
export RUST_SRC_PATH=~/src/rust/src

# Anaconda
export PATH=/home/ysgard/anaconda3/bin:$PATH
