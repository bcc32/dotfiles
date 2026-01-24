#!/usr/bin/env python
from subprocess import check_output

def get_pass():
    return check_output("gpg -dq ~/src/dotfiles-private/offlineimap/password.gpg", shell=True).rstrip(b"\n")
