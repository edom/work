#!/bin/bash

# ./prolog.sh FILE ...

# Start Prolog interpreter.

set -o errexit
set -o nounset
set -o pipefail

main() {
    local script_dir="$(dirname "$0")"
    export my_prolog_home="$(cd "$script_dir" && pwd)"
    swipl ${swipl_opts:-} -l "$my_prolog_home/boot/load0.pro" -- "$@"
}

main "$@"
