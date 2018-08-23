" deoplete.vim
"
" Settings for the deoplete autocomplete engine

" Global options
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

" Start by turning autocomplete off
let b:deoplete_disable_auto_complete=1
let g:deoplete_disable_auto_complete=1

if !exists('g:deoplete#omni:input_patterns')
  let g:deoplete#omni#input_patterns = {}
endif


" Disable autocompletion in strings/comments
call deoplete#custom#source('_',
            \ 'disabled_syntaxes', ['Comment', 'String'])

autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" Empty sources by default, then add the ones we want
let g:deoplete#sources = {}
let g:deoplete#sources.c = ['LanguageClient']
let g:deoplete#sources.rust = ['LanguageClient']
let g:deoplete#sources.cpp = ['LanguageClient']
let g:deoplete#sources.vim = ['vim']

" ignore these sources
let g:deoplete#ignore_sources = {}
let g:deoplete#ignore_sources._ = ['buffer', 'around']
