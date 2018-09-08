set -g fish_greeting ''

# Terminal options
set -x TERM xterm-256color
set -x LANG en_US.UTF-8
set -x LC_CTYPE en_US.UTF-8

# Node
set -x NODE_PATH ~/.node_modules

# Ruby
set -x PATH $HOME/.rbenv/bin $HOME/.rbenv/shims $PATH

# Rust
set -x PATH ~/.cargo/bin $PATH

# Setup editors and pagers
set -x LESS '--ignore-case --raw-control-chars'
set -x PAGER 'less'
set -x EDITOR (which vim)

# C/C++
set -x LANG en_us.UTF-8
set -x LC_CTYPE "en_US.UTF-8"
set -x LC_MESSAGES "en_US.UTF-8"
set -x LC_COLLATE C

# Go
set -x GOPATH ~/Go

# $HOME/bin 
set -x PATH $HOME/bin /usr/local/bin /usr/local/sbin $PATH

# Tweak the prompt
set -g theme_newline_cursor yes

# Source local fish file, if it exists
if test -e ~/.config/fish/config-local.fish
  . ~/.config/fish/config-local.fish
end

# If we're using Tilix, make sure to source vte.sh
if test -n "$TILIX_ID" 
  /bin/bash /etc/profile.d/vte.sh
end
