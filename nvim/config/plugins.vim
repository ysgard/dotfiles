" plugins.vim
" Use vim-plug to manage plugins
if &compatible
  set nocompatible
endif

call plug#begin('~/.local/share/nvim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'w0rp/ale'
Plug 'iCyMind/NeoSolarized'
Plug 'morhetz/gruvbox'
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'mhartington/oceanic-next'
Plug 'altercation/vim-colors-solarized'

Plug 'majutsushi/tagbar'
Plug 'ervandew/supertab'

Plug 'lilydjwg/colorizer'
Plug 'luochen1990/rainbow'
Plug 'RRethy/vim-illuminate'
Plug 'inside/vim-search-pulse'

Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'b4b4r07/vim-hcl'
Plug 'hashivim/vim-terraform'
Plug 'rhysd/vim-clang-format'
Plug 'kana/vim-operator-user'
Plug 'antoyo/vim-licenses'

Plug 'vimoutliner/vimoutliner'

Plug 'fatih/vim-go'

Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'

Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }

Plug 'alaviss/nim.nvim'
call plug#end()

