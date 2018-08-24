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

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Load on-demand
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }

call plug#end()

