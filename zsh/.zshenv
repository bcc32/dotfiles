export EDITOR=vi
export GOPATH=~/go
export GPG_TTY=$(tty)
export PATH=~/bin:/usr/local/bin:$GOPATH/bin:$PATH

LEDGER_PATH="~/Google Drive/Finances/Journal"
export LEDGER_FILE=$LEDGER_PATH/journal.ldg
export LEDGER_PRICE_DB=$LEDGER_PATH/pricedb.ldg
export LEDGER_EXPLICIT=1
export LEDGER_PEDANTIC=1

# OPAM configuration
. /Users/rouka/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval `opam config env`
