#!/bin/sh

set -euo pipefail

if command -v stow >/dev/null 2>&1; then
    stow "$@"
else
    perl -I ./.vendor.lib -- ./.vendor/bin/stow "$@"
fi
