#!/bin/bash

##  Usage: racket-env.sh Program Arg ...
##
##  Example:
##
##      racket-env.sh racket
##
##  This script sets PLTCOLLECTS and calls the program with the arguments.
##
##  This script assumes that the "racket" executable is in PATH.

main_rc() {
    PS1="(racket)$PS1"
}

main_env() {
    set -o errexit
    set -o pipefail
    set -o nounset

    ##  --------------------    Set PLTCOLLECTS.

    my_dir=$(dirname "$0")
    my_abs_dir=$(cd "$my_dir" && pwd)
    export PLTCOLLECTS=$my_abs_dir:
    export RACKET_ENV_SH_RUN=1

    if [[ $# -eq 0 ]]; then
        bash --rcfile "$0"
    else
        program=$1
        shift
        "$program" "$@"
    fi
}

racoket() {
    if [[ $# -le 0 ]]; then return 1; fi
    raco make -j 4 --vv "$1" && racket -t "$1" -m
}

if [[ ${RACKET_ENV_SH_RUN:-0} = 1 ]];
    then main_rc
    else main_env "$@"
fi
