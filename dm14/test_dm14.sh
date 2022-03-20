#!/usr/bin/env bash

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Oscar Lahaie <oscarlahaie83@gmail.com>
# Test du dm14 version du pauvre
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set -euo pipefail
IFS=$'\n\t'

echo "***********************************"
echo "*** Lecture du code source Caml ***"
echo "***********************************"
echo
ocamlc -i $1

echo
echo "***********************************"
echo "***      Testage du pauvre      ***"
echo "***********************************"
echo

expect expect_dm14.exp $1 > $1.log
cat $1.res