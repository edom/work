#!/bin/bash

# Start Prolog interpreter.

# Example: ./prolog.sh -s FILE

# Open documentation at http://localhost:4002/pldoc/

swipl --pldoc=4002 -f ../etc/swiplrc.pro -p "library0=$HOME/work/software" "$@"
