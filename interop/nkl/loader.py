# Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Paul Govereau

import types
import typing
import inspect
import ast
import json

from textwrap import dedent
from operator import add
from functools import reduce

from nkl.lean import *

def flat_map(f, l): return reduce(add, map(f, l), [])

class NKLError(Exception): pass
class NKLInvalidConst(NKLError): pass
class NKLUnknownModule(NKLError): pass
class NKLUnknownSig(NKLError): pass
class NKLInvalidArgument(NKLError): pass
class NKLUnsupportred(NKLError): pass

def opr(e: ast.AST):
  assert e._fields == (), f"not an op? {op}"
  return e.__class__.__name__

def const(c):
  if c is None: return Nil()
  elif isinstance(c, bool): return Bool(c)
  elif isinstance(c, int): return Int(c)
  elif isinstance(c, float): return Float(c)
  elif isinstance(c, str): return String(c)
  else: raise NKLInvalidConst(c)

def value(c):
  return Value(const(c))

def arguments(ax):
  def arg(x):
    if isinstance(x, Expr):
      return [x]
    if isinstance(x, tuple):
      return arguments(x)
    if isinstance(x, dict):
      if len(x) == 0: return []
      else: raise NKLInvalidArgument(x)
    return [value(x)]
  return flat_map(arg, ax)

def apply_signature(f, args, kwargs):
  if not isinstance(f, typing.Callable):
    raise NKLUnknownSig(name)
  sig = inspect.signature(f)
  b = sig.bind(*args, **kwargs)
  b.apply_defaults()
  return arguments(b.arguments.values())

def apply(f, args, kwargs):
  match f:
    case Var(name, value):
      return Call(f, apply_signature(value, args, kwargs))
    case Subscript(e, ix):
      match apply(e, args, kwargs):
        case Call(f, ax):
          return Gridcall(f, ix, ax)
        case _: assert 0, "internal error"
    case _:
      if len(kwargs) > 0:
        raise NKLUnknownSig(ast.unparse(f))
      return Call(f, args)

def check_module(s):
  if s not in [
      'math', 'numpy',
      'nki', 'nki.language', 'nki.isa'
      ]:
    raise NKLUnknownModule(s)

class Loader:
  def __init__(self, f: types.FunctionType):
    super().__init__()
    self.f = f
    self.ast = ast.parse(dedent(inspect.getsource(f)))

  def translate(self, tree: ast.mod) -> Fun:
    match tree:
      case ast.Module([ast.FunctionDef(name, argsx, body, d, r, t)]):
        args = [ a.arg for a in argsx.posonlyargs + argsx.args + argsx.kwonlyargs ]
        return Fun(name, args, self.stmts(body))
      case _:
        assert 0, "expecting function definition"

  # expressions appearing under a subscript
  def index(self, e: ast.expr):
    match e:
      case ast.Ellipsis():
        return Dots()
      case ast.Slice(l,u,s):
        return Slice(self.expr(l), self.expr(u), self.expr(s))
      case _:
        return Coord(self.expr(e))

  def expr(self, e: ast.expr):
    if e is None:
      return Value(Nil())

    def sorry(): assert 0, f"unsupporred expr {ast.dump(e)}"
    def oper(op,x,y): return Binop(opr(op), self.expr(x), self.expr(y))
    def land(x,y): return Binop("And", x, y)
    def compare(ops, l, rs):
      match ops, rs:
        case [op], [r]:
          return oper(op, l, r)
        case [op, *ops], [r, *rs]:
          return land(oper(op, l, r), compare(ops, r, rs))
        case _:
          assert 0, "invalid compare node"

    match e:
      # constants
      case ast.Constant(c):
        return value(c)

      # variables
      case ast.Name(name):
        if name == "_":
          return Bvar(name)

        if name in self.f.__code__.co_varnames:
          return Bvar(name)

        if name not in self.f.__code__.co_names:
          raise NameError(name)

        val = self.f.__globals__.get(name) or self.f.__builtins__.get(name)
        if isinstance(val, types.ModuleType):
          name = val.__name__
          check_module(name)
        return Var(name, val)

      case ast.Attribute(n, a):
        match self.expr(n):
          case Bvar(n):
            return Bvar(n + "." + a)
          case Var(n, val):
            if not hasattr(val, a):
              raise AttributeError(f"{n} has no attribute {a}",
                                   obj=val, name=a)
            return Var(n + "." + a, getattr(val, a))
          case _:
            raise NKLUnsupported(e)

      # subscript
      case ast.Subscript(l, ast.Tuple(ix)):
        return Subscript(self.expr(l), list(map(self.index, ix)))
      case ast.Subscript(l, ix):
        return Subscript(self.expr(l), [self.index(ix)])

      # literals
      case ast.Tuple(es):
        return Tuple(self.exprs(es))
      case ast.List(es):
        return List(self.exprs(es))

      # binary operations
      case ast.BoolOp(op, values):
        op = opr(op)
        values = map(self.expr, values)
        return reduce(lambda x, y: Binop(op, x, y), values)
      case ast.BinOp(l, op, r):
        return oper(op, l, r)
      case ast.UnaryOp(ast.USub(), val):
        return oper(ast.Sub(), ast.Constant(0), val)
      case ast.Compare(l, ops, rs):
        return compare(ops, l, rs)

      # function calls
      case ast.Call(f, args, kwargs):
        args = self.exprs(args)
        kwargs = {a.arg:self.expr(a.value) for a in kwargs}
        return apply(self.expr(f), args, kwargs)

      # conditional expressions
      case ast.IfExp(tst, tru, els):
        return Cond(self.expr(tst), self.expr(tru), self.expr(els))

      case e:
        raise NKLUnsupported(e)

  def exprs(self, es): return list(map(self.expr, es))

  # l-values
  def lval(self, e: ast.expr):
    return self.expr(e)

  def stmt(self, s: ast.stmt) -> [Stmt]:
    match s:
      case ast.Return(e):
        return [Ret(self.expr(e))]

      # assignments
      case ast.Assign(l, r):
        x = self.lval(l[0])
        return [Assign(x, self.expr(r))] + list(map(lambda y: Assign(self.lval(y), x), l[1:]))
      case ast.AugAssign(l, op, r):
        return self.stmt(ast.Assign([l], ast.BinOp(l, op, r)))
      case ast.AnnAssign(l, _, r):
        return self.stmt(ast.Assign([l], r))
      case ast.Expr(ast.Constant()):
        return []
      case ast.Expr(e):
        return self.stmt(ast.Assign([ast.Name("_")], e))

      # note: because we do not support break, we can handle orelse
      case ast.For(t, i, body, orelse):
        return [ Forloop(self.expr(t), self.expr(i), self.stmts(body)) ] + self.stmts(orelse)

      # if statements
      case ast.If(c, t, e):
        return [ Ifstm(self.expr(c), self.stmts(t), self.stmts(e)) ]

      # static assertions
      case ast.Assert(e):
        return [ Check(self.expr(e)) ]

      case s:
        raise NKLUnsupported(s)

  def stmts(self, ss: [ast.stmt]) -> [Stmt]:
    return flat_map(self.stmt, ss)
