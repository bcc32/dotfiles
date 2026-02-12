#!/usr/bin/env bash

set -o errexit -o nounset

cd "$(dirname "$0")"

# Ensure certain directories already exist so that stow won't try to create
# symlinks too high in the file hierarchy, leading to unmanaged files appearing
# in the repo.
mkdir -p ~/.config ~/.gnupg ~/.ssh ~/.stack

# Satisfy ssh
chmod go-w ssh/dot-ssh/config

packages=(*/)
filtered_packages=()

for package in "${packages[@]}"; do
  skip=0

  if [ "$package" = "X/" ]; then
    case "$(uname -s)" in
    Darwin*) skip=1 ;;
    *) ;;
    esac
  fi

  if ! ((skip)); then
    filtered_packages+=("$package")
  fi
done

./stow.sh --restow --dotfiles --target "$HOME" "$@" "${filtered_packages[@]}"
