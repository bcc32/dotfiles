#!/bin/sh

cd "$(git rev-parse --show-toplevel)"

chmod 644 ssh/.ssh/config
