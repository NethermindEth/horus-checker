#!/usr/bin/env sh

set -eu

version="4.10.2"

sudo apt-get remove -y libz3-dev
TEMP=$(mktemp)
curl -L -o "${TEMP}" https://github.com/Z3Prover/z3/archive/refs/tags/z3-$version.zip
unzip "${TEMP}"
cd z3-z3-$version/
python scripts/mk_make.py --prefix=/usr
cd build 
make
sudo make install
cd ../../
rm -r z3-z3-$version