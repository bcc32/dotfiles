#!/bin/sh

if command -v stow >/dev/null 2>&1; then
  exec stow "$@"
else
  exec perl -I ./.vendor/lib -- ./.vendor/bin/stow "$@"
fi
