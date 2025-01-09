/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import Lean
import NKL.KLR
import NKL.Python
import NKL.Trace.Types
import NKL.Trace.Basic

namespace NKL.Trace
open NKL.Python

def const : Const -> ErrorM Term
  | .none     => return .expr (.const $ .none)     .none
  | .bool b   => return .expr (.const $ .bool b)   .bool
  | .int i    => return .expr (.const $ .int i)    .int
  | .float f  => return .expr (.const $ .float f)  .float
  | .string s => return .expr (.const $ .string s) .string
  | .ellipsis => throw "unsupported use of ellipsis"

/-
Evaluating index expressions.

An index expression occurs only within a subscript expression. For example, in
the expression:

  t[1,1:10,None,x+9]

all of 1, 1:10, None, and x+9 are indexes. Note None may also be written as
np.newaxis. Also, a None or a slice (or ellipsis) may only occur at the
outer-most level of an index: if you write, e.g.

  t[x+None]

then the None is interpreted as an integer and not as a new axis. If you write,

  t[(1:2) + 3]
  t[... * 8]

these are syntax errors in python.
-/

mutual
-- top-level index expressions: None (a.k.a. np.newaxis) or IndexExpr
def indexExpr? : Option Expr -> Tracer (Option KLR.IndexExpr)
  | none   => return none
  | some (.exprPos (.const .none) _) => return none
  | some e => indexExpr e

-- general sub-expressions
def indexExpr : Expr -> Tracer KLR.IndexExpr
  | .exprPos e' p => withPos p (indexExpr' e')

def indexExpr' : Expr' -> Tracer KLR.IndexExpr
  | .const (.int i) => return .int i
  | .name id _      => return .var id
  | .binOp op l r   => return <- indexBinOp op (<- indexExpr l) (<- indexExpr r)
  | .unaryOp op e   => return <- indexUnOp op (<- indexExpr e)
  | _ => throw "invalid index expression"

-- top-level index: slice, ellipsis, or indexExpr?
-- TODO: get rid of ...
def index : Expr -> Tracer KLR.Index
  | .exprPos (.const .ellipsis) _ => return .ellipsis
  | .exprPos (.slice l u s) p => withPos p do
      return (.slice (<- indexExpr? l) (<- indexExpr? u) (<- indexExpr? s))
  | e => return (.coord (<- indexExpr? e))
end

-- Note, a list index can be negative, which means index from end of list.
def list_access (l : List Term) : List KLR.Index -> TraceM Term
  | [.coord (some (.int i))] => do
      let i := if i < 0 then l.length + i else i
      if i < 0 then throw "index out of bounds"
      let n := i.toNat
      if h:l.length > n then return l.get (Fin.mk n h)
      else throw "index out of bounds"
  |_ => throw "unsupported __subscript__"

def access : Term -> List KLR.Index -> TraceM Term
  | .object _, _  => throw "builtin object __subscript__ not supported"
  | .tuple l, ix
  | .list l, ix   => list_access l ix
  | .expr e _, ix => return .expr (.access e ix) (.any "?".toName)


mutual
partial def expr : Expr -> Tracer Item
  | .exprPos e' p => withPos p (expr' e')

partial def term (e : Expr) : Tracer Term := do
  match (<- expr e) with
  | .module n   => return .expr (.var n.toString) (.any "?".toName)
  | .global g   => return .expr (.var g.name.toString) (.any "?".toName)
  | .source _   => throw "invalid use of source function"
  | .term t     => return t

partial def term' (e : Expr') : Tracer Term := do
  term (.exprPos e (<- getPos))

partial def klr (e : Expr) : Tracer KLR.Expr := do
  match (<- term e) with
  | .object obj => return .var obj.name.toString
  | .tuple _    => throw "tuple cannot be converted to a KLR term"
  | .list _     => throw "list cannot be converted to a KLR term"
  | .expr e _   => return e

partial def integer (e : Expr) : Tracer Int := do
  match (<- term e) with
  | .expr (.const c) _ => return (<- c.toInt)
  | _ => throw "invalid tensor dimension"

partial def expr' : Expr' -> Tracer Item
  | .const c => return .term (<- const c)
  | .tensor s dty => do
      let shape <- s.mapM integer
      return .term (.expr (.tensor ⟨ dty, shape ⟩) (.tensor dty shape))
  | .name id _ => lookup_item id.toName
  | .attr (.exprPos e p) id _ => do withPos p ((<- expr' e).attr id)
  | .tuple l _ => return .term (.tuple (<- l.mapM term))
  | .list  l _ => return .term (.list  (<- l.mapM term))
  | .subscript t [ .exprPos (.tuple ix _) _ ] _
  | .subscript t ix _ => return .term (<- access (<- term t) (<- ix.mapM index))
  | .slice _ _ _ => throw "syntax error"
  | .boolOp op xs => return .term (<- boolOp op (<- xs.mapM term))
  | .binOp op l r => return .term (<- binOp op (<- term l) (<- term r))
  | .unaryOp op e => return .term (<- unOp op (<- term e))
  | .compare l ops cs => return .term (<- compare (<- term l) ops (<- cs.mapM term))
  | .ifExp tst tru fls => do
      let tst <- (<- term tst).isTrue
      let tru <- expr tru  -- eagerly evaluate both branches
      let fls <- expr fls  -- to report errors to user
      return if tst then tru else fls
  | .call f args kws => do
      match <- expr f with
      | .module n => throw s!"module {n} not callable"
      | .global g => return .term (<- g.call (<- args.mapM term) (<- kws.mapM (keyword term)))
      | .term t   => return .term (<- t.call (<- args.mapM klr) (<- kws.mapM (keyword klr)))
      | .source f => do
          function_call f (<- args.mapM term) (<- kws.mapM (keyword term))
          return .term (.expr (.const .none) .none)

partial def keyword (f : Expr -> Tracer a) : Keyword -> Tracer (String × a)
  | .keyword id e p => withPos p do return (id, (<- f e))

-- When looking for a variable we rely on the store attribute
-- from the Python parser to check if it is a defining use.
partial def var : Expr -> Tracer String
  | .exprPos (.name id .store) _ => return id
  | _ => throw "expecting variable"

-- When we perform an assignment, we will either add to the environment
-- the term found on the RHS, or the variable itself. The latter case
-- allows us to lookup and find the variable without substituting
-- its definition.
partial def assign (xs : List Expr) (e : Expr) : Tracer Unit := do
  let xs <- xs.mapM var
  let e <- term e
  match e with
  | .expr (.const _) _ => xs.forM fun x => extend x.toName e
  | .expr e ty => xs.forM fun x => do
      extend x.toName (.expr (.var xs[0]!) ty)
      add_stmt (KLR.Stmt.assign x e)
  | t => xs.forM fun x => extend x.toName t

partial def stmt : Stmt -> Tracer Unit
  | .stmtPos s' p => withPos p (stmt' s')

partial def stmt' : Stmt' -> Tracer Unit
  | .expr (.exprPos (.const _) _) => return ()
  | .expr e => do
      match <- term e with
      | .expr e _ => add_stmt (.expr e)
      | _ => return ()  -- effects are done, can be removed from KLR
  | .assert e => do
      let t <- term e
      if (<- t.isFalse) then throw "assertion failed"
  | .assign xs e => assign xs e
  | .augAssign x op e => do
      stmt' (.assign [x] (.exprPos (.binOp op x e) (<- getPos)))
  | .annAssign _ _ .none => return ()
  | .annAssign x _ (.some e) => stmt' (.assign [x] e)
  | _s => throw "not yet implemented" --s!"unimp {repr s}"

-- Bind positional and keyword arguments to a Python function based on its
-- signature.

partial def bind_args (f : Fun)
                      (args : List Term)
                      (kwargs : List (String × Term))
                      : Tracer (List (String × Term)) := do
  if f.args.vararg != none || f.args.kwarg != none then
    throw "var args not supported"
  if args.length < f.args.posonlyargs.length then
    throw "not enough arguments"
  let dflts := f.args.all_defaults
  let names := f.args.names
  if args.length + kwargs.length > names.length then
    throw "too many arguments supplied (varargs not supported)"
  let argmap <- f.args.names.enum.mapM fun (i,x) => do
    if h:args.length > i then
      return (x, args.get (Fin.mk i h))
    else if let some v := kwargs.lookup x then
      return (x, v)
    else if let some e := dflts.lookup x then
      return (x, <- term' e)
    else
      throw s!"argument {x} not supplied"
  return argmap

-- For a function call, first evaluate the argument in the current environment.
-- Then enter a new environment and evaluate the function statements.
partial def function_call (f : Fun)
                          (args : List Term)
                          (kwargs : List (String × Term))
                          : Tracer Unit := do
  let args <- bind_args f args kwargs
  --let args <- args.mapM fun (x,e) => return (x, e)
  withSrc f.source $ enterFun $ do
    args.forM fun (x,e) => do extend x.toName e
    f.body.forM stmt

end

/-
Evaluate each global in the current environment, skipping any globals that are
already defined. We do not redefine globals because, we may have picked up
functions with dummy implementations, e.g., nki.language.add is defined as:

  def add(x,y): pass

in some versions of the code. We do not want this to shadow a built-in
definition of add. If we have an internal definition, we will use this over
anything found during parsing.
-/

private def globals (k : Kernel) : Tracer Unit := do
  let s <- get
  for (n, f) in k.funcs do
    let n := n.toName
    if not (s.env.contains n) then
      extend_global n (.source f)
  for (n,e) in k.globals do
    let n := n.toName
    if not (s.env.contains n) then
      extend_global n (<- expr' e)

-- Call the top-level kernel function
def traceKernel (k : Kernel) : Tracer Unit := do
  globals k
  match k.funcs.lookup k.entry with
  | none => throw s!"function {k.entry} not found"
  | some f => do
      let args <- k.args.mapM term'
      let kwargs <- k.kwargs.mapM fun (x,e) => return (x, <- term' e)
      function_call f args kwargs

def runKernel (k : Kernel) : Except String (List KLR.Stmt) :=
  tracer ⟨ ∅, #[] ⟩ do
    traceKernel k
    let g <- get
    return g.body.toList
