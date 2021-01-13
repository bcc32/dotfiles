""" Merlin setup
let s:opam_share=trim(system('opam var share'))
execute 'set runtimepath+='.s:opam_share.'/merlin/vim'

"" Clear autocmds in case .vimrc is re-run
augroup vimrc_autocmds
    autocmd!
augroup END

"""" Editing {{{

""" Basics {{{
set clipboard=unnamed
set cpoptions+=J                        " Sentences end with two spaces
set directory=~/.vim/swap//             " centralized swapfile directory
set hidden                              " keep undo history for hidden buffers
set nomodeline
set omnifunc=syntaxcomplete#Complete
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
set lazyredraw                          " do not redraw screen during macro
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
    autocmd FileType ledger     :setlocal foldmethod=syntax
    autocmd FileType make       :setlocal noexpandtab
    autocmd FileType ocaml      :setlocal shiftwidth=2
    autocmd FileType perl       :setlocal shiftwidth=2
    autocmd FileType ruby       :setlocal shiftwidth=2
    autocmd FileType vim        :setlocal foldmethod=marker
augroup END
""" }}}

""" automatic reformatting {{{
augroup vimrc_autocmds
    "" strip trailing spaces on write
    "" FIXME add an option to configure this
    autocmd BufWritePre  *    :%s/\s\+$//e
    "" run goimports
    autocmd BufWritePost *.go :silent! !goimports -w % 2>/dev/null
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
Plug 'airblade/vim-rooter'
Plug 'mbbill/undotree'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

"" Appearance
Plug 'altercation/vim-colors-solarized'
Plug 'chriskempson/base16-vim'
Plug 'jnurmine/Zenburn'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"" File finding
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

"" VCS
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'

"" File types
Plug 'ledger/vim-ledger', { 'for': 'ledger' }
Plug 'let-def/ocp-indent-vim', { 'for': 'ocaml' }

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

""" Goyo and Limelight {{{
"" Automatically (de-)activate limelight with Goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

let g:limelight_conceal_ctermfg = 'bg'
let g:limelight_conceal_guifg = 'bg'
""" }}}

""" vim-ledger {{{
let g:ledger_date_format = '%Y-%m-%d'
let g:ledger_main = '~/journal/journal.ldg'
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

"" repeat last macro instead of starting ex mode
nnoremap Q @@

"" quickly edit vimrc
nnoremap <Leader>re :vsplit $MYVIMRC<CR>
nnoremap <Leader>rs :source $MYVIMRC<CR>

"" yank entire buffer to clipboard
nnoremap <Leader>cc :%yank+<CR>

inoremap <C-g><C-t> [<C-r>=strftime("%Y-%m-%d %a %H:%M")<cr>]

""" }}}

""" Windows {{{

"" move between windows
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

""" }}}

""" Toggles {{{
set pastetoggle=<F3>
nnoremap <F4> :set list!<CR>
nnoremap <F5> :UndotreeToggle<CR>

"" switch between favorite themes
nnoremap <F9> :call ToggleTheme("solarized", "solarized")<CR>
nnoremap <F10> :call ToggleTheme("zenburn", "zenburn")<CR>
nnoremap <F11> :call ToggleTheme("base16-default-dark", "base16_default")<CR>

"" Activate/deactivate Goyo
nnoremap <Leader>g :Goyo<CR>
""" }}}

""" FZF Shortcuts {{{

nnoremap <C-P> :Files<CR>
"" goto buffer
nnoremap gb :Buffers<CR>
"" list recent files
nnoremap gh :History<CR>

"" search using rg
nnoremap gs :Rg<CR>
nnoremap gS :Rg!<CR>

""" }}}

""" Custom Commands {{{

"" for when you forget to start vim using sudo...
cmap ww write !sudo tee % >/dev/null

""" }}}

""" Function Definitions {{{

function! ToggleBackground()
    if exists("g:colors_name") && g:colors_name ==# "solarized"
                \&& &background ==# "dark"
        set background=light
    else
        set background=dark
    endif
    call UpdateLimelightColors()
endfunction

function! ToggleTheme(new_colors, new_airline_theme)
    if exists("g:colors_name") && g:colors_name ==# a:new_colors
        call ToggleBackground()
    else
        set background=dark
        execute "colorscheme"  a:new_colors
        execute "AirlineTheme" a:new_airline_theme
    endif
    call UpdateLimelightColors()
endfunction

""" https://vi.stackexchange.com/a/12305
function! GetHighlight(group)
    let output = execute('hi ' . a:group)
    let list = split(output, '\s\+')
    let dict = {}
    for item in list
        if match(item, '=') > 0
            let splited = split(item, '=')
            let dict[splited[0]] = splited[1]
        endif
    endfor
    return dict
endfunction

""" Update the conceal colors for limelight
function! UpdateLimelightColors()
    let l:highlight = GetHighlight('Normal')
    if has_key(l:highlight, 'ctermbg')
        let g:limelight_conceal_ctermfg = l:highlight.ctermbg
    endif
    if has_key(l:highlight, 'guibg')
        let g:limelight_conceal_guifg = l:highlight.guibg
    endif
endfunction

call UpdateLimelightColors()

""" }}}

"""" }}}
