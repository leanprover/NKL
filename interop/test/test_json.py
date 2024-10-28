import pytest

import json
from nkl.lean import *

# A few quick tests TODO: setup pytest

# this should fail
def test_bad_json():
  with pytest.raises(Exception):
    py_to_lean("hello")

# this should succeed
def test_json_string():
  py_to_lean('{"name":"name", "body":[], "args":[]}')

# this should succeed
def test_json_tuple():
  f = Fun("name", [], [])
  j = to_json_dict(f)
  py_to_lean(json.dumps(j))
