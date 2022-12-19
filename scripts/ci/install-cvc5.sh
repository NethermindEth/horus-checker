#!/usr/bin/env sh

set -eu

curl -L -O https://github.com/cvc5/cvc5/releases/download/cvc5-1.0.3/cvc5-Linux
sudo mv cvc5-Linux /usr/bin/cvc5
sudo chmod +x /usr/bin/cvc5
