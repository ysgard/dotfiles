" plugins.vim
" Use dein to manage plugins
if &compatible
  set nocompatible
endif

set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

  call dein#add('~/.cache/dein')
  
  " Plugins
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('scrooloose/nerdtree')
  call dein#add('w0rp/ale')
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('tomasr/molokai')

  call dein#add('autozimu/LanguageClient-neovim', {
                      \ 'rev': 'next',
                      \ 'build': 'bash install.sh',
                      \ })

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable

" On startup install any plugins not installed
if dein#check_install()
  call dein#install()
endif
