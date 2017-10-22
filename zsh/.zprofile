typeset -U path
path=(~/bin /usr/local/bin $GOPATH/bin $path[@])

# OPAM configuration
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval `opam config env`

# CPAN configuration
eval `perl -Mlocal::lib`