" filetypes.vim
" 
" Override default handling of specific file types

" Reset all autocommands
augroup vimrc
autocmd!



au BufNewFile,BufRead *.h,*.c   set expandtab tabstop=4 shiftwidth=4 softtabstop=4 filetype=c.doxygen
au BufNewFile,BufRead *.cson    set ft=coffee
au BufNewFile,BufRead *.glsl    set glsl
au BufNewFile,BufRead *.html    setlocal nocindent smartindent
au BufNewFile,BufRead *.ini     setf conf
au BufNewFile,BufRead *.input   setf gnuplot
au BufNewFile,BufRead *.json    set ft=json tw=0
au BufNewFile,BufRead *.less    setlocal ft=less nocindent smartindent
au BufNewFile,BufRead *.md,*.markdown     setlocal ft=markdown nolist spell nocindent nosmartindent noautoindent tw=78 lbr fo=aw2tq colorcolumn=80 textwidth=80 foldlevel=999
au BufNewFile,BufRead *.ni      setlocal ft=inform nolist ts=2 sw=2 noet
au BufNewFile,BufRead *.plan    setlocal nospell smartindent cindent autoindent nowrap
au BufNewFile,BufRead *.plist   setf xml
au BufNewFile,BufRead *.rb      setlocal noai
au BufNewFile,BufRead *.rs      set expandtab tabstop=4 shiftwidth=4 softtabstop=4 filetype=rust
au BufNewFile,BufRead *.rxml    setf ruby
au BufNewFile,BufRead *.sass    setf sass
au BufNewFile,BufRead *.ttml    setf xml
au BufNewFile,BufRead *.tf      setlocal ft=hcl ts=2 sw=2
au BufWritePost *.tf,*.tfvars :silent :execute '!terraform fmt <afile>' | :edit
au BufNewFile,BufRead *.txt     setlocal nocindent nosmartindent spell noautoindent tw=78 lbr nolist fo=aw2tq colorcolumn=80 textwidth=80
au BufNewFile,BufRead *.vert,*.frag set ft=glsl
au BufNewFile,BufRead *.zsh     setf zsh
au BufNewFile,BufRead *templates/*.html setf htmldjango
au BufNewFile,BufRead .git/config setlocal ft=gitconfig nolist ts=4 sw=4 noet
au BufNewFile,BufRead .gitconfig* setlocal ft=gitconfig nolist ts=4 sw=4 noet
au BufNewFile,BufRead .vimlocal,.gvimlocal setf vim
au BufNewFile,BufRead .zshlocal setf zsh
au BufNewFile,BufRead /tmp/crontab* setf crontab
au BufNewFile,BufRead Thorfile,*.thor setf ruby

au FileType gitcommit setlocal nolist ts=4 sts=4 sw=4 noet 
au FileType json setlocal conceallevel=0 foldmethod=syntax foldlevel=999
au FileType make setlocal nolist ts=4 sts=4 sw=4 noet
au FileType markdown syn sync fromstart

" rust
au FileType rust set makeprg=cargo\ build\ -j\ 4
au FileType rust nmap <leader>t :!cargo test<cr>
au FileType rust nmap <leader>r :!RUST_BACKTRACE=1 cargo run<cr>
au FileType rust nmap <leader>c :term cargo build -j 4<cr>
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)

augroup END
