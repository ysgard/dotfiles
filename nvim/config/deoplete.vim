" deoplete.vim
"
" Settings for the deoplete autocomplete engine

" Global options
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('enable_smart_case', 1)

set hidden

let g:LanguageClient_serverCommands = {
  \ 'rust': ['~/.cargo/bin/rustup', 'run', 'nightly', 'rls'],
  \ 'ruby': ['~/.gem/ruby/2.2.0/bin'],
  \ }

nnoremap <F5> :call LanguageClient_contextMenu()<CR>

