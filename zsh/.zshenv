export EDITOR=vi
export GOPATH=~/.go
export PATH=~/bin:/usr/games/bin:/usr/local/bin:/usr/local/sbin:$GOPATH/bin:~/.cargo/bin:$PATH

# OPAM configuration
. /Users/rouka/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval `opam config env`
