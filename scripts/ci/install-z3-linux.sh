#!/usr/bin/env sh

set -eu

version="4.10.2"

sudo apt-get remove libz3-dev
TEMP=$(mktemp)
curl -L -o "${TEMP}" https://github.com/Z3Prover/z3/releases/download/z3-$version/z3-$version-x64-glibc-2.31.zip
unzip "${TEMP}"
sudo cp z3-$version-x64-glibc-2.31/bin/libz3.so /usr/lib/libz3.so
sudo cp z3-$version-x64-glibc-2.31/include/* /usr/include/
sudo cp z3-$version-x64-glibc-2.31/bin/z3 /usr/bin/z3
rm -r z3-$version-x64-glibc-2.31
