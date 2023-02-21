#!/usr/bin/env bash

versions=$(pip install horus-compile== 2>&1 \
| grep -oE '(\(.*\))' \
| awk -F:\  '{print$NF}' \
| sed -E 's/( |\))//g' \
| tr ',' '\n')

bounds=$(stack run horus-check -- --version 2>&1 \
| grep 'Horus-compile (required):' \
| grep -oE '>=.*<.*' \
| sed -E 's/(>=| |<)//g' \
| tr ',' '\n')

bounds=($bounds)

lower=${bounds[0]}
upper=${bounds[1]}

version2int() {
    local -i intversion
    local -a version
    local IFS="."

    version=($1)
    if [[ -n ${version[3]} ]]; then
        intversion=$(( ${version[0]}*10**4 + ${version[1]}*10**3 + ${version[2]}*10**2 + ${version[3]} ))
    else
        intversion=$(( ${version[0]}*10**4 + ${version[1]}*10**3 + ${version[2]}*10**2))
    fi
    echo $intversion
}

intlower=$(version2int ${lower})
intupper=$(version2int ${upper})

for elem in $versions;
do 
    intelem=$(version2int ${elem}) 
    if (( $intelem >= $intlower && $intelem < $intupper )); 
    then 
        filtered+=($elem);
    fi
done

lastversion=${filtered[-1]}

pip install horus-compile==${lastversion}