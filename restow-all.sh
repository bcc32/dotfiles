#!/bin/sh

set -eu

FLAGS=-Rv
DIRS=*/

if which stow >/dev/null 2>&1; then
    stow $FLAGS $DIRS
else
    perl -I./.vendor/lib ./.vendor/bin/stow $FLAGS $DIRS
fi
