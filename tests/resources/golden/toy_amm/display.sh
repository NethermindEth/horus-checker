#!/bin/bash
echo '~~~~~~~~~~~~~~~{SOURCE}~~~~~~~~~~~~~~' \
  && cat $1 && echo '' \
  && echo '~~~~~~~~~~~~~~{RESULT}~~~~~~~~~~~~~~~' \
  && horus-compile $1 --output a.json && horus-check -s mathsat -t 20000 a.json \
  && echo '~~~~~~~~~~~~~~{REVISION}~~~~~~~~~~~~~' \
  && git log --oneline -n 1 && echo '' \
  && echo '~~~~~~~~~~~~~~{FILENAME}~~~~~~~~~~~~~' \
  && echo $1
