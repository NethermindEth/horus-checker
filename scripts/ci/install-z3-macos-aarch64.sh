#!/usr/bin/env sh

set -eu

version="4.10.2"
arch="arm64-osx-11.0"

brew remove libz3-dev
curl -L -O https://github.com/Z3Prover/z3/releases/download/z3-$version/z3-$version-$arch.zip
unzip z3-$version-$arch.zip
sudo cp z3-$version-$arch/bin/libz3.so /usr/lib/libz3.so
sudo cp z3-$version-$arch/include/* /usr/include/
sudo cp z3-$version-$arch/bin/z3 /usr/bin/z3
