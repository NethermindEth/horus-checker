#!/usr/bin/env bash

set -eu

RES_DIR="./tests/resources"
bash "${RES_DIR}/generate-bats.sh" > "${RES_DIR}/tests.bats"
jobs=$(getconf _NPROCESSORS_ONLN)
printf "Num processors: $jobs\n"
bats --jobs "${jobs}" "${RES_DIR}/tests.bats"
rm "${RES_DIR}/tests.bats"
