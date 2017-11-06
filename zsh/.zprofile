have() {
    which ${1} >/dev/null 2>&1
}

export EDITOR=vi
export GOPATH=~/go
export GPG_TTY=$(tty)
export COPYFILE_DISABLE=1 # prevent tar from adding ._* files

if have ledger; then
    LEDGER_PATH="~/Google Drive/Finances/Journal"
    export LEDGER_FILE=$LEDGER_PATH/journal.ldg
    export LEDGER_PRICE_DB=$LEDGER_PATH/pricedb.ldg
    export LEDGER_EXPLICIT=1
    export LEDGER_PEDANTIC=1
    export LEDGER_DATE_FORMAT=%Y-%m-%d
fi

export YT_API_KEY='AIzaSyAPGisBETz4EIj9Byxl70QVQQHKwIF-X3A'

typeset -U path
path=(~/bin /usr/local/bin $GOPATH/bin $path[@])

if have opam; then
    # OPAM configuration
    . ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
    eval `opam config env`
fi

# CPAN configuration
eval `perl -Mlocal::lib`

# rbenv
eval "$(rbenv init -)"
