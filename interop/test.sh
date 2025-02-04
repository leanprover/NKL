#!/bin/sh
ROOT=$(dirname $(dirname $(readlink -f $0)))
export PYTHONPATH=$ROOT/interop
python3 $ROOT/interop/test/test_examples.py
