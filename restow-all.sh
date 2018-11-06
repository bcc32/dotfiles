#!/bin/sh

set -euo pipefail

# Ensure certain directories already exist so that stow won't try to create
# symlinks too high in the file hierarchy, leading to unmanaged files appearing
# in the repo.
mkdir -p ~/.config ~/.gnupg ~/.ssh ~/.stack

FLAGS=${FLAGS--R}

if which stow >/dev/null 2>&1; then
    # shellcheck disable=SC2086
    # SC2086: Double quote to prevent globbing and word splitting
    stow $FLAGS */
else
    # shellcheck disable=SC2086
    # SC2086: Double quote to prevent globbing and word splitting
    perl -I./.vendor/lib ./.vendor/bin/stow $FLAGS */
fi
