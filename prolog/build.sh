#!/bin/bash
mkdir -p ../build
cd ../build
g++ -Wall -std=c++11 -pedantic-errors -g -o prolog ../prolog/src/prolog.cpp
