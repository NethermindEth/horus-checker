#!/usr/bin/env sh

set -eu

TEMP=$(mktemp)
curl -L -o "${TEMP}" "https://github.com/cvc5/cvc5/releases/download/cvc5-1.0.3/cvc5-Linux"
sudo mv "${TEMP}" /usr/local/bin/cvc5
sudo chmod +x /usr/local/bin/cvc5
