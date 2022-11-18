#!/usr/bin/env sh

set -eu

NAME=mathsat-5.6.8-linux-x86_64-reentrant
TEMP=$(mktemp)
curl -L -o "${TEMP}" "https://mathsat.fbk.eu/download.php?file=${NAME}.tar.gz"
tar -zxvf "${TEMP}" "${NAME}/bin/mathsat"
sudo mv "${NAME}/bin/mathsat" /usr/local/bin
