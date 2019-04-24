#!/bin/bash

# ./prolog.sh FILE ...

# Start Prolog interpreter.

set -o errexit
set -o nounset
set -o pipefail

main() {
    swipl ${swipl_opts:-} -l boot/load0.pro -- "$@"
}

main "$@"
