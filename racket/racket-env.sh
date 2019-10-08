#!/bin/bash

##  Usage: racket-env.sh Program Arg ...
##
##  Example:
##
##      racket-env.sh racket
##
##  This script sets PLTCOLLECTS and calls the program with the arguments.

set -o errexit
set -o pipefail
set -o nounset

##  This script can only be run from the directory that contains this script file.
##
##  This script assumes that the "racket" executable is in PATH.

##  --------------------    Set PLTCOLLECTS.

my_dir=$(dirname "$0")
my_abs_dir=$(cd "$my_dir" && pwd)
export PLTCOLLECTS=$my_abs_dir:

program=$1
shift

##  --------------------    Run the program with the environment.

"$program" "$@"
