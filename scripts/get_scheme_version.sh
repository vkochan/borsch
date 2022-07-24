#!/bin/sh

SCHEME_PROG="scheme"

if ! command -v scheme &> /dev/null
then
    SCHEME_PROG="chez"	
fi

echo "(scheme-version)" | ${SCHEME_PROG} -q | sed -e 's|"||g' | cut -d ' ' -f4
