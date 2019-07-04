" keybinds.vim
"
" If it messes with the keys, it should be here.

" Sane defaults
let mapleader = ','
:inoremap jj <Esc>

nmap <Leader>x :NERDTreeToggle<CR>

" Buffer navigation
nmap <Leader>n :bnext<CR>
nmap <Leader>p :bprev<CR>

" Tab navigation
nmap <Leader>N :tabnext<CR>
nmap <Leader>P :tabprev<CR>
nnoremap <A-Left> :tabprev<CR>
nnoremap <A-Right> :tabnext<CR>

" Space key toggles folds
nnoremap <space> za
vnoremap <space> zf

" Quick-fix spelling
nmap <Leader>z z=1<CR><CR>

" Call fzf
nmap <Leader>f :FZF<CR>

" Toggle line numbers
nmap <Leader>l :set invnumber<CR>

" Window management
nmap <Leader>o <C-w>w 
nmap <Leader>v <C-w>v
nmap <Leader>s <C-w>s

" LanguageClient
noremap <silent> H :call LanguageClient_textDocument_hover()<CR>
noremap <silent> Z :call LanguageClient_textDocument_definition()<CR>
noremap <silent> R :call LanguageClient_textDocument_rename()<CR>
noremap <silent> S :call LanguageClient_textDocument_documentSymbol<CR>
