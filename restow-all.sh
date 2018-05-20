#!/bin/sh

set -eu

# Ensure certain directories already exist so that stow won't try to create
# symlinks too high in the file hierarchy, leading to unmanaged files appearing
# in the repo.
mkdir -p ~/.config ~/.ssh ~/.stack

FLAGS=${FLAGS--R}
DIRS=*/

if which stow >/dev/null 2>&1; then
    stow $FLAGS $DIRS
else
    perl -I./.vendor/lib ./.vendor/bin/stow $FLAGS $DIRS
fi
