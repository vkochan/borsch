#!/bin/sh

SCHEME_PROG="scheme"

if ! command -v scheme &> /dev/null
then
    SCHEME_PROG="chez"	
fi

echo "(machine-type)" | ${SCHEME_PROG} -q
