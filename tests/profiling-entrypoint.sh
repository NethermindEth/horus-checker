#!/usr/bin/env bash

set -u

RES_DIR="./tests/resources"
bash "${RES_DIR}/generate-profiling-bats.sh" > "${RES_DIR}/tests-profiling.bats"
jobs=$(getconf _NPROCESSORS_ONLN)
bats -T --jobs "${jobs}" "${RES_DIR}/tests-profiling.bats" > bats-result.txt
cat bats-result.txt
rm "${RES_DIR}/tests-profiling.bats"
