# Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Paul Govereau

from .lean import load, to_json
from .loader import Loader

def parse(f):
  F = Loader(f)
  return F.translate(F.ast)

def parse_and_load(f):
  load(parse(f))
