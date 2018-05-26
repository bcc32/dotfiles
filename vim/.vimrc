""" Merlin setup
"" This needs to be updated when the switch changes
set runtimepath+=~/.opam/4.06.0/share/merlin/vim

"" Clear autocmds in case .vimrc is re-run
augroup vimrc_autocmds
    autocmd!
augroup END

"""" Editing {{{

""" Basics {{{
set clipboard=unnamed
set directory=~/.vim/swap//             " centralized swapfile directory
set hidden                              " keep undo history for hidden buffers
set nomodeline
set omnifunc=syntaxcomplete#Complete
set textwidth=79
""" }}}

""" Persistent Undo {{{
set undodir=~/.vim/undo//
set undofile
""" }}}

""" Search {{{
set ignorecase
set smartcase

"" Use ripgrep when possible
if executable('rg')
    set grepprg=rg\ --vimgrep
endif
""" }}}

""" User Interface {{{
set cursorcolumn
set cursorline
set incsearch
set hlsearch
set list                                " show certain whitespace chars
set number
set relativenumber
set showcmd
""" }}}

"" highlight long lines {{{
augroup vimrc_autocmds
    autocmd BufEnter * highlight OverLength
                \ term=undercurl cterm=undercurl gui=undercurl
    autocmd BufEnter * match OverLength /\%>79v.\+/
augroup END
"" }}}

"""" }}}

"""" Formatting {{{

""" Indentation Defaults {{{
set expandtab
set shiftround                          " < and > round to nearest shiftwidth
set shiftwidth=4                        " 4 space indent
set softtabstop=-1                      " use value of shiftwidth
""" }}}

""" Indentation Options for Various Filetypes {{{
augroup vimrc_autocmds
    autocmd FileType go         :setlocal noexpandtab tabstop=4
    autocmd FileType javascript :setlocal shiftwidth=2
    autocmd FileType make       :setlocal noexpandtab
    autocmd FileType ocaml      :setlocal shiftwidth=2
    autocmd FileType perl       :setlocal shiftwidth=2
    autocmd FileType ruby       :setlocal shiftwidth=2
    autocmd FileType vim        :setlocal foldmethod=marker
augroup END
""" }}}

""" strip trailing spaces on write {{{
augroup vimrc_autocmds
    autocmd BufWritePre * :%s/\s\+$//e
augroup END
""" }}}

""" Encoding and File Format {{{
set cryptmethod=blowfish2
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,latin1
set fileformats=unix,dos
""" }}}

"""" }}}

"""" Plugins {{{

""" vim-plug declarations {{{
call plug#begin('~/.vim/plugged')

"" Reset defaults
Plug 'tpope/vim-sensible'

"" Editing
Plug 'mbbill/undotree'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

"" Appearance
Plug 'altercation/vim-colors-solarized'
Plug 'jnurmine/Zenburn'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"" File finding
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

"" VCS
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'

"" File types
Plug 'ledger/vim-ledger'
Plug 'let-def/ocp-indent-vim'

call plug#end()
""" }}}

""" vim-signify {{{
let g:signify_vcs_list = [ 'git', 'hg' ]
""" }}}

""" Solarized color theme {{{
colorscheme solarized
set background=dark
""" }}}

""" Airline {{{
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts            = 1
let g:airline_theme                      = 'solarized'
""" }}}

"""" }}}

"""" Key Bindings and Commands {{{

""" Basic Mappings {{{

"" Set shorter key sequence timeout
set timeoutlen=250

"" SPC begins commands
noremap <Space> :

"" fd is ESC
inoremap fd <Esc>

"" make Y consistent with C, S, D, etc.
nnoremap Y y$

"" quickly edit vimrc
nnoremap <Leader>re :vsplit $MYVIMRC<CR>
nnoremap <Leader>rs :source $MYVIMRC<CR>

""" }}}

""" Toggles {{{
set pastetoggle=<F3>
nnoremap <F4> :set list!<CR>
nnoremap <F5> :UndotreeToggle<CR>

"" switch between favorite themes
nnoremap <F9> :call ToggleTheme("solarized")<CR>
nnoremap <F10> :call ToggleTheme("zenburn")<CR>
""" }}}

""" FZF Shortcuts {{{

nnoremap <C-P> :Files<CR>
"" goto buffer
nnoremap gb :Buffers<CR>
"" list recent files
nnoremap gh :History<CR>

command! -bang -nargs=* Rg call fzf#vim#grep('rg --column --line-number --no-heading --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)

nnoremap gs :Rg<CR>

""" }}}

""" Custom Commands {{{

"" for when you forget to start vim using sudo...
cmap w!! w !sudo tee % >/dev/null

""" }}}

""" Function Definitions {{{

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

""" }}}

"""" }}}
