import pytest

import json
from nkl.lean import *

# this should fail
def test_bad_json():
  with pytest.raises(Exception):
    py_to_lean("hello")

# this should succeed
def test_json_string():
  py_to_lean('{"name":"name", "body":[], "args":[]}')

# this should succeed
def test_json_named_tuple():
  f = Fun("name", [], [])
  j = to_json_dict(f)
  py_to_lean(json.dumps(j))

@pytest.mark.parametrize("term",
    [ Nil(),
      Bool(True), Bool(False),
      Int(-1), Int(0), Int(1),
      Float(1e-2), Float(0), Float(11.2),
      String(''), String('Hello'),
    ])
def test_consts(term):
  load(Fun("f", [], [Ret(Value(term))]))

@pytest.mark.parametrize("term",
    [ Var("a", 0),
      Bvar("b"),
    ])
def test_vars(term):
  load(Fun("f", [], [Assign(term, Value(Nil()))]))

var = Bvar("a")
nil = Value(Nil())

@pytest.mark.parametrize("term",
    [ [],
      [var],
      [var, nil],
      [Slice(var, var, var)],
      [Slice(nil, nil, nil), nil],
      [Value(Dots())],
    ])
def test_subscript(term):
  load(Fun("f", [], [Assign(Var("a", 0), Subscript(var, term))]))
