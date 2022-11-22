#!/usr/bin/env bash

# The first argument is the name of the `.cairo` program, and the second
# argument is the per-module timeout in milliseconds.
horus-compile $1 > out.json && time horus-check -v -s z3 -s mathsat --timeout $2 out.json && rm out.json
