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
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'mhartington/oceanic-next'

Plug 'lilydjwg/colorizer'
Plug 'luochen1990/rainbow'
Plug 'RRethy/vim-illuminate'
Plug 'inside/vim-search-pulse'

Plug 'rust-lang/rust.vim'
Plug 'b4b4r07/vim-hcl'
Plug 'hashivim/vim-terraform'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'fatih/vim-go'

" Load on-demand
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }

call plug#end()

