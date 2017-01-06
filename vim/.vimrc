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
" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
" ## added by OPAM user-setup for vim / ocp-indent ## 33c34b8a4dd66d1116237b988be02d58 ## you can edit, but keep this line
if count(s:opam_available_tools,"ocp-indent") == 0
  source "/Users/rouka/.opam/4.04.0/share/vim/syntax/ocp-indent.vim"
endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line
