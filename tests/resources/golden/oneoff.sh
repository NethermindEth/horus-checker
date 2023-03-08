#!/usr/bin/env bash
base="$1"
test_file="${base}.cairo"
printf "# checking %s\n" "$test_file"
gold_file="${base}.gold"
compiled_file="${base}.json"
temp_file="${base}.temp"
spec_file="${base}_spec.json"

horus-compile "$test_file" --output "$compiled_file" --spec_output "$spec_file"
stack run horus-check "$compiled_file" "$spec_file" -- -s cvc5 -s z3 -t 100000 &> "$temp_file" || true
ansii="\x1B\[[0-9;]\{1,\}[A-Za-z]"
sed -i "/^hint:/d" $temp_file
sed -i "/^$ansii[h]int:/d" $temp_file
sed -i "/^Warning:/d" $temp_file
sed -i "/^$ansii[W]arning:/d" $temp_file
diff "$gold_file" "$temp_file" \
     --unified \
     --ignore-trailing-space \
     --ignore-blank-lines \
     --new-file # treat absent files as empty
rm "$compiled_file"
rm "$temp_file"
rm "$base.out" -rf
