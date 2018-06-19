#!/bin/bash

if [[ "$#" == 0 ]]; then
    echo 'Usage: sh/mvn_install.sh dir1 dir2 dir3 ...'
    echo 'This script runs "mvn clean source:jar install" on the given directories.'
    exit
fi

for dir in "$@"; do
    pushd "$@"
    mvn clean source:jar install
    popd
done
