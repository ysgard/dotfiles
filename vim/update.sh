#!/usr/bin/env bash
#
# usage: ./update.sh [pattern]
# 
# Updates plugin repos, matching [pattern] if provided

repos=(
  airblade/vim-gitgutter
  ekalinin/Dockerfile.vim
  itchyny/lightline.vim
  maximbaz/lightline-ale
  junegunn/fzf.vim
  mileszs/ack.vim
  scrooloose/nerdtree
  sheerun/vim-polyglot
  tomasr/molokai
  tpope/vim-commentary
  tpope/vim-fugitive
  tpope/vim-pathogen
  tpope/vim-rhubarb
  tpope/vim-surround
  tpope/vim-unimpaired
  vim-ruby/vim-ruby
  w0rp/ale

)

set -e
dir=~/.dotfiles/vim/bundle

if [ -d $dir -a -z "$1" ]; then
  temp="$(mktemp -d -t bundleXXXXX)"
  echo "▲ Moving old bundle dir to $temp"
  mv "$dir" "$temp"
fi

mkdir -p $dir

for repo in ${repos[@]}; do
  if [ -n "$1" ]; then
    if ! (echo "$repo" | grep -i "$1" &>/dev/null) ; then
      continue
    fi
  fi
  plugin="$(basename $repo | sed -e 's/\.git$//')"
  [ "$plugin" = "vim-styled-jsx" ] && plugin="000-vim-styled-jsx" # https://goo.gl/tJVPja
  dest="$dir/$plugin"
  rm -rf $dest
  (
    git clone --depth=1 -q https://github.com/$repo $dest
    rm -rf $dest/.git
    echo "· Cloned $repo"
  ) &
done
wait
