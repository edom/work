#!/bin/bash

##  This script can only be run from the directory that contains this script file.
##
##  This script assumes that the "racket" executable is in PATH.

PLTCOLLECTS=$(pwd): racket "$@"
