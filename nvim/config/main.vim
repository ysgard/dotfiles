" main.vim
"
" Neovim takes the stage!  
" Author: Jan Van Uytven (Roughly based on afnanenayet/nvim-dotfiles)
" Incept Date: 2018-08-23
" 
" General nvim config goes here

" Display options
set termguicolors
set background=dark
colorscheme NeoSolarized

" ALE 
let g:ale_sign_warning = '▲'
let g:ale_sign_error = '✗'
highlight link ALEWarningSign String
highlight link ALEErrorSign Title


