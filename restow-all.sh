#!/usr/bin/env bash

set -eu

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

./stow.sh --no-folding -R "$@" "${filtered_packages[@]}"
