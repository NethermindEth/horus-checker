#!/usr/bin/env sh

set -e

stylish-haskell -ri ./src ./app
git add --all

if [ -z "$(git diff --name-only --staged)" ]; then
    exit 0
fi

git diff --staged
exit 1
