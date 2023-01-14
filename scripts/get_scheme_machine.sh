#!/bin/sh

SCHEME_PROG="chez scheme chez-scheme"

for scm in $SCHEME_PROG; do
    if command -v $scm &> /dev/null
    then
        echo "(machine-type)" | ${scm} -q
        exit 0
    fi
done

exit 1
