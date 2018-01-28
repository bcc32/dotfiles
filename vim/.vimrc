"""" Editing

""" Basics
set backspace=indent,eol,start
set clipboard=unnamed
set hidden                              " keep undo history for hidden buffers
set omnifunc=syntaxcomplete#Complete
set textwidth=79
set viminfo=""                          " TODO re-enable

""" Search
set ignorecase
set smartcase

""" User Interface
set colorcolumn=+1                      " highlight textwidth limit
set cursorcolumn
set cursorline
set hlsearch
set laststatus=2                        " always display status line
set number
set relativenumber
set ruler                               " TODO remove, superseded by airline

"""" Formatting

""" Indentation Defaults
filetype plugin indent on
set autoindent
set expandtab
set shiftwidth=4                        " 4 space indent
set softtabstop=-1                      " use value of shiftwidth

""" Custom Indentation Options
au FileType go         :setl ts=4
au FileType javascript :setl sw=2
au FileType make       :setl noet
au FileType ocaml      :setl sw=2
au FileType perl       :setl sw=2
au FileType ruby       :setl sw=2

""" strip trailing spaces on write
autocmd BufWritePre * :%s/\s\+$//e

""" Encoding and File Format
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,latin1
set fileformats=unix,dos

"""" Plugins

""" vim-plug declarations
call plug#begin('~/.vim/plugged')

Plug 'altercation/vim-colors-solarized'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'jnurmine/Zenburn'
Plug 'mbbill/undotree'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

""" Solarized color theme
colors solarized
set background=dark

""" CtrlP
"" don't list files in .gitignore
let g:ctrlp_user_command =
            \['.git', 'cd %s && git ls-files -co --exclude-standard']

""" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts            = 1
let g:airline_theme                      = 'solarized'

"""" Key Bindings and Commands

"" for when you forget to start vim using sudo...
cmap w!! w !sudo tee % >/dev/null

set pastetoggle=<F3>

nnoremap <F5> :UndotreeToggle<CR>

"" goto buffer
nnoremap gb :ls<CR>:b<Space>

""" Switch between favorite themes
nnoremap <F6> :colo solarized<CR>:AirlineTheme solarized<CR>:set bg=dark<CR>
nnoremap <F7> :colo solarized<CR>:AirlineTheme solarized<CR>:set bg=light<CR>
nnoremap <F8> :colo zenburn<CR>:AirlineTheme zenburn<CR>:set bg=dark<CR>

"""" Last but not least...

if !exists("g:syntax_on")
    syntax enable
endif
