#!/usr/bin/env sh

set -eu

version="4.10.2"

brew extract --version=$version z3 homebrew/cask
brew install z3@$version
