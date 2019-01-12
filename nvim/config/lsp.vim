" A basic setup for LanguageClient-Neovim

" << LSP >> {{{

set hidden

" Set to 1 if you want to start it automatically
let g:LanguageClient_autoStart = 0
nnoremap <leader>lcs :LanguageClientStart<CR>


let g:LanguageClient_serverCommands = {
  \ 'python': ['pyls'],
  \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
  \ 'javascript': ['javascript-typescript-stdio'],
  \ 'go': ['go-langserver']
  \ }

noremap <silent> H :call LanguageClient_textDocument_hover()<CR>
noremap <silent> Z :call LanguageClient_textDocument_definition()<CR>
noremap <silent> R :call LanguageClient_textDocument_rename()<CR>
noremap <silent> S :call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <F5> :call LanguageClient_contextMenu()<CR>

"}}}
