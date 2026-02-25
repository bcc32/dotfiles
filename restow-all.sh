#!/usr/bin/env bash

set -o errexit -o nounset

cd "$(dirname "$0")"

# Ensure certain directories already exist so that stow won't try to create
# symlinks too high in the file hierarchy, leading to unmanaged files appearing
# in the repo.
mkdir -p ~/.config ~/.gnupg ~/.ssh ~/.stack

# On the other hand, ensure certain directories do not exist since they /should/ in fact be symlinked as a single unit.
assert_not_dir() {
  d=$1
  if [ -d "$d" ] && ! [ -L "$d" ]; then
    echo >&2 "$d is a directory (not a symlink); aborting"
  fi
}
assert_not_dir ~/.emacs.d
assert_not_dir ~/.zprezto

# Satisfy ssh
chmod go-w ssh/dot-ssh/config

# Satisfy gpg
chmod go-rwx ~/.gnupg

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
