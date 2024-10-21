#!bin/sh

DIR=$(lean --print-libdir)

export LD_LIBRARY_PATH=$DIR   # for linux
export DYLD_LIBRARY_PATH=$DIR # for OS/X
python test.py

