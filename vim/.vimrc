"" Clear autocmds in case .vimrc is re-run
augroup vimrc_autocmds
    autocmd!
augroup END

"""" Editing

""" Basics
set clipboard=unnamed
set directory=~/.vim/swap//             " centralized swapfile directory
set hidden                              " keep undo history for hidden buffers
set nomodeline
set omnifunc=syntaxcomplete#Complete
set textwidth=79

""" Persistent Undo
set undodir=~/.vim/undo//
set undofile

""" Search
set ignorecase
set smartcase

""" User Interface
set cursorcolumn
set cursorline
set incsearch
set hlsearch
set list                                " show certain whitespace chars
set number
set relativenumber
set showcmd

"" highlight long lines
augroup vimrc_autocmds
    autocmd BufEnter * highlight OverLength
                \ term=undercurl cterm=undercurl gui=undercurl
    autocmd BufEnter * match OverLength /\%>79v.\+/
augroup END

"""" Formatting

""" Indentation Defaults
set expandtab
set shiftwidth=4                        " 4 space indent
set softtabstop=-1                      " use value of shiftwidth

""" Custom Indentation Options
augroup vimrc_autocmds
    autocmd FileType go         :setl noet ts=4
    autocmd FileType javascript :setl sw=2
    autocmd FileType make       :setl noet
    autocmd FileType ocaml      :setl sw=2
    autocmd FileType perl       :setl sw=2
    autocmd FileType ruby       :setl sw=2
augroup END

""" strip trailing spaces on write
augroup vimrc_autocmds
    autocmd BufWritePre * :%s/\s\+$//e
augroup END

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
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'ledger/vim-ledger'
Plug 'let-def/ocp-indent-vim'
Plug 'mbbill/undotree'
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

""" vim-signify
let g:signify_vcs_list = [ 'git', 'hg' ]

""" Solarized color theme
colorscheme solarized
set background=dark

"" don't list VCS ignored files
let g:ctrlp_user_command = {
            \ 'types': {
            \     1: ['.git', 'cd %s && git ls-files -co --exclude-standard'],
            \     2: ['.hg',  'hg --cwd %s files -I .'],
            \ },
            \ 'fallback': 'find %s -type f',
            \ }

""" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts            = 1
let g:airline_theme                      = 'solarized'

"""" Key Bindings and Commands

"" SPC begins commands
noremap <Space> :

""" Function Keys
set pastetoggle=<F3>
nnoremap <F4> :set list!<CR>
nnoremap <F5> :UndotreeToggle<CR>

"" switch between favorite themes
nnoremap <F9> :call ToggleTheme("solarized")<CR>
nnoremap <F10> :call ToggleTheme("zenburn")<CR>

""" CtrlP

"" goto buffer
nnoremap gb :CtrlPBuffer<CR>
"" list MRU files
nnoremap gc :CtrlPMRUFiles<CR>

""" Custom Commands

"" for when you forget to start vim using sudo...
cmap w!! w !sudo tee % >/dev/null

function! ToggleBackground()
    if exists("g:colors_name") && g:colors_name ==# "solarized"
                \&& &background ==# "dark"
        set background=light
    else
        set background=dark
    endif
endfunction

function! ToggleTheme(new_colors)
    if exists("g:colors_name") && g:colors_name ==# a:new_colors
        call ToggleBackground()
    else
        set background=dark
        execute "colorscheme"  a:new_colors
        execute "AirlineTheme" a:new_colors
    endif
endfunction

runtime merlin.vim
