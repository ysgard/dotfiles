" main.vim
"
" Neovim takes the stage!  
" Author: Jan Van Uytven (Roughly based on afnanenayet/nvim-dotfiles)
" Incept Date: 2018-08-23
" 
" General nvim config goes here

" Display options
set termguicolors
syntax enable
" set background=light
colorscheme OceanicNext

" General options
set autoindent                " Carry over indenting from previous line
set autoread                  " Don't bother me when a file changes
set autowrite                 " Write on :next/:prev/^Z
set backspace=indent,eol,start
                              " Allow backspace beyond insertion point
set cindent                   " Automatic program indenting
set cinkeys-=0#               " Comments don't fiddle with indenting
set cino=                     " See :h cinoptions-values
set commentstring=\ \ #%s     " When folds are created, add them to this
set copyindent                " Make autoindent use the same chars as prev line
" Store swapfiles in ~/tmp if it exists, otherwise ~/.vim/swapfiles
set directory=~/tmp,~/.vim/swapfiles
set encoding=utf8             " UTF-8 by default
set expandtab                 " No tabs
set fileformats=unix,dos,mac  " Prefer unix
set fillchars=vert:\ ,stl:\ ,stlnc:\ ,fold:-,diff:┄
                              " Unicode chars for diffs/folds, and rely on
                              " Colors for window borders
silent! set foldmethod=marker " Use braces by default
set formatoptions=tcqn1       " t - autowrap normal text
                              " c - autowrap comments
                              " q - gq formats comments
                              " n - autowrap lists
                              " 1 - break _before_ single-letter words
                              " 2 - use indenting from 2nd line of para
set hidden                    " Don't prompt to save hidden windows until exit
set history=200               " How many lines of history to save
set hlsearch                  " Hilight searching
set ignorecase                " Case insensitive
set incsearch                 " Search as you type
set infercase                 " Completion recognizes capitalization
set laststatus=2              " Always show the status bar
set linebreak                 " Break long lines by word, not char
set list                      " Show whitespace as special chars - see listchars
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:· " Unicode chars for various things
set matchtime=2               " Tenths of second to hilight matching paren
set modelines=5               " How many lines of head & tail to look for ml's
silent! set mouse=nvc         " Use the mouse, but not in insert mode
set nobackup                  " No backups left after done editing
set nonumber                  " no line numbers by default
set visualbell t_vb=          " No flashing or beeping at all
set nowritebackup             " No backups made while editing
set printoptions=paper:letter " US paper
set ruler                     " Show row/col and percentage
set scroll=4                  " Number of lines to scroll with ^U/^D
set scrolloff=15              " Keep cursor away from this many chars top/bottom
set sessionoptions-=options   " Don't save runtimepath in Vim session (see tpope/vim-pathogen docs)
set noshiftround              " Don't round autoindent to nearest shiftwidth units
set shiftwidth=2              " Number of spaces to shift for autoindent of >,<
" set shortmess+=A              " Don't bother me when a swapfile exists
set showbreak=                " Show for lines that have been wrapped, like Emacs
set showmatch                 " Hilight matching braces/parens/etc.
set sidescrolloff=3           " Keep cursor away from this many chars left/right
set smartcase                 " Lets you search for ALL CAPS
set softtabstop=2             " Spaces 'feel' like tabs
set suffixes+=.pyc            " Ignore these files when tab-completing
set tabstop=2                 " The one true tab, according to some
set textwidth=80              " 100 is the new 80? Haha no
set thesaurus+=~/.vim/mthes10/mthesaur.txt
set title                     " set the title of the vim window
set wildmenu                  " Show possible completions on command line
set wildmode=list:longest,full  " List all options and complete
set wildignore=*.class,*.o,*~,*.pyc,.git,node_modules
                              " Ignore certain files in tab-completion

" ALE 
let g:ale_sign_warning = '▲'
let g:ale_sign_error = '✗'
highlight link ALEWarningSign String
highlight link ALEErrorSign Title

" Molokai
let g:molokai_original = 1
let g:rehash256 = 1
