" Enable ligatures on macOS
if has('mac')
    set macligatures
endif

if has('gui_gtk2') || has('gui_gtk3')
    set guifont=Iosevka\ 15
else
    set guifont=Iosevka:h15
endif

" Consistent line numbering
set columns=96
set numberwidth=8
