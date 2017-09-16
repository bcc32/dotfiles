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
set textwidth=79
set statusline=%t[%{strlen(&fenc)?&fenc:'none'},%{&ff}]%h%m%r%y%=%c,%l/%L\ %P
set viminfo=""
syntax on

" Custom options for different file types

au FileType css :setlocal sts=2 sw=2
au FileType html :setlocal sts=2 sw=2
au FileType javascript :setlocal sts=2 sw=2
au FileType make :setlocal noexpandtab
au FileType ocaml :setlocal sts=2 sw=2
au FileType ruby :setlocal sts=2 sw=2

autocmd BufWritePre * :%s/\s\+$//e
set number
set relativenumber
set clipboard=unnamed

filetype plugin indent on
set omnifunc=syntaxcomplete#Complete

set gfn=Anonymous\ Pro:h13
