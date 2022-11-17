#!/usr/bin/env bash
DISPLAY_LINES=5

# The first argument is the name of the `.cairo` program, and the second
# argument is the per-module timeout in milliseconds.
horus-compile $1 > out.json && horus-check -s z3 -s mathsat --timeout $2 out.json | head -n $DISPLAY_LINES && rm out.json
echo "Warning: only the first $DISPLAY_LINES lines printed."
