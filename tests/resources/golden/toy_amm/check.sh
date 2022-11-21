#!/usr/bin/env bash
DISPLAY_LINES=100

# The first argument is the name of the `.cairo` program, and the second
# argument is the per-module timeout in milliseconds.
horus-compile $1 > out.json && time horus-check -s z3 --timeout $2 out.json | head -n $DISPLAY_LINES && rm out.json
echo "Warning: only the first $DISPLAY_LINES lines printed."
echo "You can adjust the 'DISPLAY_LINES' constant in the 'check.sh' script to show more output."
