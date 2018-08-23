" keybinds.vim
"
" If it messes with the keys, it should be here.

" Sane defaults
let mapleader = ','
:inoremap jj <Esc>

" Nerdtree
nmap <Leader>x :NERDTreeToggle<CR>

" Buffer navigation
nmap <Leader>n :bnext<CR>
nmap <Leader>p :bprev<CR>

" Tab navigation
nmap <Leader>N :tabnext<CR>
nmap <Leader>P :tabprev<CR>

" Space key toggles folds
nnoremap <space> za
vnoremap <space> zf

" Quick-fix spelling
nmap <Leader>z z=1<CR><CR>



