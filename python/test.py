import json
from lean import *

# A few quick tests TODO: setup pytest

# this should fail
py_to_lean("hello")

# this should succeed
py_to_lean('{"name":"name", "body":[], "args":[]}')

# this should succeed
f = Fun("name", [], [])
j = to_json_dict(f)
py_to_lean(json.dumps(j))
