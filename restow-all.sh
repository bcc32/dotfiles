#!/bin/sh

set -euo pipefail

# Ensure certain directories already exist so that stow won't try to create
# symlinks too high in the file hierarchy, leading to unmanaged files appearing
# in the repo.
mkdir -p ~/.config ~/.gnupg ~/.ssh ~/.stack

# shellcheck disable=SC2035
# SC2035: Use ./*glob* or -- *glob* so names with dashes won't become options.
./stow.sh -R "$@" */
