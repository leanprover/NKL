# Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Paul Govereau

from nkl.lean_types import *
from nkl.lean_rffi import *

def to_json_dict(obj):
  if isinstance(obj, tuple) and hasattr(obj, '_fields'):
    d = {k:to_json_dict(v) for k,v in obj._asdict().items()}
    if not d.pop('struct'):
      d = {obj.__class__.__name__: d}
    return d
  return obj
