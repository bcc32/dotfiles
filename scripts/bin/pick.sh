#!/usr/bin/env bash

set -euo pipefail

N=${N:-20}

mkdir temp

find . -maxdepth 1 -type f -print0 | shuf -zn "$N" | xargs -0 mv -t ./temp
