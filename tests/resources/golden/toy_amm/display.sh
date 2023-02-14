#!/bin/bash
echo '~~~~~~~~~~~~~~~{SOURCE}~~~~~~~~~~~~~~' \
  && cat $1 && echo '' \
  && echo '~~~~~~~~~~~~~~{RESULT}~~~~~~~~~~~~~~~' \
  && horus-compile $1 --output a.json --spec_output b.json && time horus-check -s z3 -s mathsat -t 10000 a.json b.json \
  && echo '~~~~~~~~~~~~~~{REVISION}~~~~~~~~~~~~~' \
  && git log --oneline -n 1 && echo '' \
  && echo '~~~~~~~~~~~~~~{FILENAME}~~~~~~~~~~~~~' \
  && echo $1
