set autoindent
set backspace=2
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set fileformats=unix,dos
set hidden  " keep undo history for backgrounded buffers
set ignorecase
set nohlsearch
set ls=2
set ruler
set shiftwidth=4
set smartcase
set nosmartindent
set softtabstop=4
set textwidth=79
"set statusline=%t[%{strlen(&fenc)?&fenc:'none'},%{&ff}]%h%m%r%y%=%c,%l/%L\ %P
set viminfo=""
syntax on

" Custom options for different file types

au FileType go         :setlocal ts=4
au FileType javascript :setlocal sts=2 sw=2
au FileType make       :setlocal noexpandtab
au FileType ocaml      :setlocal sts=2 sw=2
au FileType perl       :setlocal sts=2 sw=2
au FileType ruby       :setlocal sts=2 sw=2

autocmd BufWritePre * :%s/\s\+$//e
set cursorcolumn
set cursorline
set number
set relativenumber
set clipboard=unnamed

filetype plugin indent on
set omnifunc=syntaxcomplete#Complete

" for when you forget to start vim using sudo...
cmap w!! w !sudo tee > /dev/null %

set pastetoggle=<F3>

" vim-plug
call plug#begin('~/.vim/plugged')

Plug 'altercation/vim-colors-solarized'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'jnurmine/Zenburn'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

colors solarized
set background=dark

" CtrlP
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='solarized'
