#!/bin/sh

set -x

# TODO: for now using a simple script to build the python library
# Need to decide which of lake or setuptools is better to use,
# and how we will distribute everything

# make sure libKLR.a is generated
(cd ..; lake build KLR)

LEAN_PREFIX=$(lean --print-prefix)
LEAN_CFLAGS="-I${LEAN_PREFIX}/include"
LEAN_LDFLAGS="-L${LEAN_PREFIX}/lib -L${LEAN_PREFIX}/lib/lean -L../.lake/build/lib"
LEAN_LIBS="-lKLR -lLean -lStd -lInit -lleanrt -lleancpp -luv -lgmp -lc++"

# we can use the following to dynamically link the lean code
#LEAN_LIBS="-lKLR -lInit_shared -lleanshared"


PY_VER=$(python -V)
PY_VER=${PY_VER:7:4}
PY_EXT=$(python3-config --extension-suffix)
PY_CFLAGS=$(python3-config --cflags)
PY_LDFLAGS=$(python3-config --ldflags)
PY_LIBS="-lpython${PY_VER}"

clang nkl/lean_rffi.c -dynamiclib -o nkl/lean_rffi${PY_EXT} \
  ${LEAN_CFLAGS} ${PY_CFLAGS} \
  ${LEAN_LDFLAGS} ${LEAN_LIBS} \
  ${PY_LDFLAGS} ${PY_LIBS}

