#!/usr/bin/env bash

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Nicolas Pécheux <info.cpge@cpge.info>
# Saturday, 26 September 2021
# http://cpge.info
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
echo "***         Évaluation          ***"
echo "***********************************"
echo

expect expect_dm11.exp $1 > $1.log
cat $1.res
