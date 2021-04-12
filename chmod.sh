#!/bin/sh

set -eu

cd "$(git rev-parse --show-toplevel)"

chmod go-w ssh/.ssh/config
