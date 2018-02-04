let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
if !v:shell_error
    execute "set rtp+=" . g:opamshare . "/merlin/vim"
    execute "helptags " . g:opamshare . "/merlin/vim/doc"
endif
