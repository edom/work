#!/bin/bash

# -------------------- infrastructure

set -o errexit
set -o nounset
set -o pipefail

config_vars=()

config() {
    local name=$1
    local default=$2
    declare -g $name=${!name:-$default}
    config_vars+=($name)
}

configure() {
    local func=
    local value=

    for var in "${config_vars[@]}"; do
        value=${!var}
        echo "$var=$value"
        if [[ "$value" = 1 ]]; then
            func=if_$var
            if [[ "$(type -t $func)" = "function" ]]; then
                $func
            else
                echo "Error: Undefined function: $func"
                return 1
            fi
        fi
    done
}

generate_config_h() {
    echo -n > "$config_file"

    for var in "${config_vars[@]}"; do
        echo "#define $var ${!var}" >> "$config_file"
    done
}

visibly () {
    echo "$@"
    "$@"
}

# -------------------- settings

# 2019-06-04: Clang compiles faster than GCC.
CXX=${CXX:-clang++}
CXXFLAGS=(
    -std=c++11
    -fPIC
    -g
    -pedantic-errors
    -Wall
    -Wextra
    -Wno-unused-function
    # -ftime-report
)
build_dir=build

# -------------------- objects

config_file=$build_dir/config.h

app_name=interpreter
app_out=$build_dir/$app_name
app_srcs=(
    src/main.cpp
)
app_libs=()

lib_soname=libinterpreter.so.0
lib_name=libinterpreter.so.0.0.0
lib_out=$build_dir/$lib_name
lib_sofile=$build_dir/$lib_soname
lib_libs=()
lib_srcs=(
    src/library.cpp
)

# -------------------- configuration

config      WANT_GMP            1
config      WANT_READLINE       1

if_WANT_GMP()               { lib_libs+=(-lgmp); }
if_WANT_READLINE()          { app_libs+=(-lreadline -ltermcap); }

# -------------------- build the objects

mkdir -p $build_dir

echo -n -e "\n-------------------- Building $config_file\n\n"
configure
generate_config_h

if [[ src/pch.h -nt src/pch.h.gch ]]; then
    echo -n -e "\n-------------------- Building precompiled header\n\n"
    visibly "$CXX" "${CXXFLAGS[@]}" -x c++-header src/pch.h
fi

# Should we use -Wl,-soname,libinterpreter.so.0?
# http://man7.org/conf/lca2006/shared_libraries/slide4b.html
# http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html
echo -n -e "\n-------------------- Building $lib_out\n\n"
visibly "$CXX" "${CXXFLAGS[@]}" -Wl,-soname,libinterpreter.so.0 -shared -o "$lib_out" "${lib_srcs[@]}" "${lib_libs[@]}"

# The app can only be run from the directory containing build.sh.
echo -n -e "\n-------------------- Building $app_out\n\n"
visibly "$CXX" "${CXXFLAGS[@]}" -o "$app_out" "${app_srcs[@]}" "$lib_out" "${lib_libs[@]}" "${app_libs[@]}"

echo -n -e "\n-------------------- Building $lib_sofile\n\n"
ln -s -f "$lib_name" "$lib_sofile"
