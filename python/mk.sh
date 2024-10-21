#!/bin/sh

set -x

# TODO: for now using a simple script to build the python library
# Need to decide which of lake or setuptools is better to use,
# and how we will distribute everything

# make sure libNKL.a and lean_types.py are generated
(cd ..; lake build NKL Export)

LEAN_CFLAGS="-I$(lean --print-prefix)/include"
LEAN_LDFLAGS="-L$(lean --print-libdir) -L../.lake/build/lib"
LEAN_LIBS="-lNKL -lInit_shared -lleanshared"

# we can use the following to statically link the lean code
#LEAN_LIBS="-lNKL -lLean -lStd -lInit -lleanrt -lleancpp -luv -lgmp -lc++"

PY_EXT=$(python-config --extension-suffix)
PY_CFLAGS=$(python-config --cflags)
PY_LDFLAGS=$(python-config --ldflags)
PY_LIBS="-lpython3.10"

clang lean_rffi.c -dynamiclib -o lean_rffi${PY_EXT} \
  ${LEAN_CFLAGS} ${PY_CFLAGS} \
  ${LEAN_LDFLAGS} ${LEAN_LIBS} \
  ${PY_LDFLAGS} ${PY_LIBS}

