#!/bin/sh

SCHEME_PROG="chez scheme chez-scheme"

for scm in $SCHEME_PROG; do
    if command -v $scm &> /dev/null
    then
        echo "(scheme-version)" | ${scm} -q | sed -e 's|"||g' | cut -d ' ' -f4
        exit 0
    fi
done

exit 1
