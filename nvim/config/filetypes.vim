" filetypes.vim
" 
" Override default handling of specific file types

" Reset all autocommands
augroup vimrc
autocmd!

au BufNewFile,BufRead *.c       set expandtab tabstop=4 shiftwidth=4 softtabstop=4
au BufNewFile,BufRead *.cson    set ft=coffee
au BufNewFile,BufRead *.glsl    set glsl
au BufNewFile,BufRead *.html    setlocal nocindent smartindent
au BufNewFile,BufRead *.ini     setf conf
au BufNewFile,BufRead *.input   setf gnuplot
au BufNewFile,BufRead *.json    set ft=json tw=0
au BufNewFile,BufRead *.less    setlocal ft=less nocindent smartindent
au BufNewFile,BufRead *.md      setlocal ft=markdown nolist spell
au BufNewFile,BufRead *.md,*.markdown setlocal foldlevel=999 tw=0 nocin
au BufNewFile,BufRead *.ni      setlocal ft=inform nolist ts=2 sw=2 noet
au BufNewFile,BufRead *.plan    setlocal nospell smartindent cindent autoindent nowrap
au BufNewFile,BufRead *.plist   setf xml
au BufNewFile,BufRead *.rb      setlocal noai
au BufNewFile,BufRead *.rs      setlocal hidden
au BufNewFile,BufRead *.rxml    setf ruby
au BufNewFile,BufRead *.sass    setf sass
au BufNewFile,BufRead *.ttml    setf xml
au BufNewFile,BufRead *.txt     setlocal nocindent nosmartindent spell noautoindent tw=78 lbr nolist fo=aw2tq
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

augroup END

