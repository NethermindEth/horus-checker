#!/bin/bash
echo '~~~~~~~~~~~~~~~{SOURCE}~~~~~~~~~~~~~~' \
  && cat $1 && echo '' \
  && echo '~~~~~~~~~~~~~~{RESULT}~~~~~~~~~~~~~~~' \
  && time horus-compile $1 --output a.json --spec_output b.json && time stack run horus-check -- -s z3 -s mathsat -t 500 a.json b.json \
  && echo '~~~~~~~~~~~~~~{REVISION}~~~~~~~~~~~~~' \
  && git log --oneline -n 1 && echo '' \
  && echo '~~~~~~~~~~~~~~{FILENAME}~~~~~~~~~~~~~' \
  && echo $1
