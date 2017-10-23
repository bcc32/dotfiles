have() {
    which ${1} >/dev/null 2>&1
}

typeset -U path
path=(~/bin /usr/local/bin $GOPATH/bin $path[@])

if have opam; then
    # OPAM configuration
    . ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
    eval `opam config env`
fi

# CPAN configuration
eval `perl -Mlocal::lib`
