#!/bin/bash

# This fragment elevates privilege
# and executes whatever code that follows.
#
# This only works with 'bash -c' and 'sudo'.
#
# This has been tested on Ubuntu 12.04,
# but should also work on every system that has
# recent enough 'bash' and 'sudo'.
#
# 2013-07-27 23:00 +0700

is_root () { [ "$UID" -eq 0 ]; }

if ! is_root; then
    # If sudo does not change $UID to 0,
    # this becomes a fork bomb.
    sudo -- bash -c "$BASH_EXECUTION_STRING"
    exit $?
fi

# Beyond here we should be root.

if ! is_root; then
    echo 'Impossible!'
    exit 1
fi
