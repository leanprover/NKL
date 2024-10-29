# Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Paul Govereau

import json
from nkl.lean_types import *
from nkl.lean_rffi import *

def to_json_dict(obj):
  if isinstance(obj, list):
    return list(map(to_json_dict,obj))
  if isinstance(obj, tuple) and hasattr(obj, '_fields'):
    d = {k:to_json_dict(v) for k,v in obj._asdict().items()}
    if isinstance(obj, Var):
      d['value'] = ""
    if not d.pop('struct'):
      if len(d) == 0:
        return obj.__class__.__name__
      d = {obj.__class__.__name__: d}
    return d
  return obj

def to_json(f: Fun):
  return json.dumps(to_json_dict(f))

def load(f: Fun):
  py_to_lean(to_json(f))
