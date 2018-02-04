#!/bin/sh

set -eu

FLAGS=-Rv
DIRS=*/

if which stow >/dev/null 2>&1; then
    stow $FLAGS $DIRS
else
    ./stow.pl $FLAGS $DIRS
fi
