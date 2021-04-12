#!/usr/bin/env bash

set -euo pipefail

exit_code=0

git ls-files -z | xargs -0 file | awk -F: '/shell/ { print $1 }' |
    while read -r file; do
        shellcheck -x "$file" || exit_code=$((exit_code | $?)) && true
    done
