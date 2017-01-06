set autoindent
set backspace=2
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set fileformats=unix,dos
set ignorecase
set nohlsearch
set ls=2
set ruler
set shiftwidth=4
set smartcase
set nosmartindent
set softtabstop=4
" set statusline=%t[%{strlen(&fenc)?&fenc:'none'},%{&ff}]%h%m%r%y%=%c,%l/%L\ %P
set viminfo=""
let fortran_free_source=1
unlet! fortran_fixed_source
syntax on

"" Custom options for different file types

au FileType css :setlocal sts=2 sw=2
au FileType html :setlocal sts=2 sw=2
au FileType javascript :setlocal sts=2 sw=2
au FileType make :setlocal noexpandtab
au FileType ocaml :setlocal sts=2 sw=2
au FileType ruby :setlocal sts=2 sw=2

au BufNewFile,BufRead *.rs set filetype= " rust

"autocmd BufWritePre * :%s/\s\+$//e
"set lines=24
"set columns=80
"set nu
"set printoptions+=number:y
set clipboard=unnamed

filetype plugin indent on
set omnifunc=syntaxcomplete#Complete

if has('gui_running')
    colors solarized
    set background=dark
    set columns=80
    set lines=26
    set gfn=Anonymous\ Pro\ for\ Powerline:h13
    set anti
endif

let g:airline_powerline_fonts = 1

"OCaml
set rtp+=~/.opam/4.04.0/share/merlin/vim
au FileType ocaml nnoremap <buffer> <localleader>= :%!ocp-indent<Return>
