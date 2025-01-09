/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import NKL.KLR
import NKL.Trace.Types
import NKL.Trace.Tensor

/-
# Basic tracing facilities

Basic tracing definitions only deal with Terms (not Python sources)
-/

namespace NKL.Trace
open NKL.KLR

-- Operators within index expressions

def indexBinOp : String -> KLR.IndexExpr -> KLR.IndexExpr -> ErrorM KLR.IndexExpr
  | "Add" ,      l,      r => return .add l r
  | "Sub" ,      l,      r => return .add l r.neg
  | "Mult", .int i,      e
  | "Mult",      e, .int i => return .mul i e
  | "Div" ,      e, .int i => return .floor e i
  | "Mod" ,      e, .int i => return .mod e i
  | _, _, _ => throw "invalid index expression"

def indexUnOp : String -> KLR.IndexExpr -> ErrorM KLR.IndexExpr
  | "USub", e => return .neg e
  | _, _ => throw "invalid index expresssion"

-- Truthiness of Terms following Python

def Term.isTrue : Term -> TraceM Bool
  | .object _ => return true
  | .tuple [] => return false
  | .tuple _  => return true
  | .list []  => return false
  | .list _   => return true
  | .expr (.const c) _ => return c.isTrue
  | .expr _          _ => throw "non-constant expression"

def Term.isFalse (t : Term) : TraceM Bool :=
  return not (<- t.isTrue)

-- Following Python semantics, boolean operators return
-- the first value that is convertible to True or False

def boolOp (op : String) (es : List Term) : TraceM Term := do
  bop (<- bopFn op) es
where
  bop fn : List Term -> TraceM Term
    | []  => throw "invalid expression"
    | [x] => return x
    | x :: xs => do if (<- fn x) then return x else bop fn xs
  bopFn : String -> TraceM (Term -> TraceM Bool)
    | "Or"  => return Term.isTrue
    | "And" => return Term.isFalse
    | s     => throw s!"unsupported boolean operator {s}"

-- Binary Operators

-- Multiply a sequence (tuple, list, string) by a scalar
-- It is tempting to use Const.toInt here, but that would be
-- more permissive than Python. The only allowed cases are:
--   [1,2] * 2     => [1,2,1,2]
--   [1,2] * 0     => []
--   [1,2] * -10   => []
--   [1,2] * True  => [1,2]
--   [1,2] * False => []

private def mulseq (l : List a) : Const -> TraceM (List a)
  | .bool false => return []
  | .bool true  => return l
  | .int i      => return List.flatten $ List.replicate i.toNat l
  | _           => throw "invalid multiply"

-- Binary operators on constants
private def constOp : String -> Const -> Const -> TraceM Term
  | "Add",  .int l, .int r => return int (l + r)
  | "Sub",  .int l, .int r => return int (l - r)
  | "Mult", .int l, .int r => return int (l * r)
  | "Div",  .int l, .int r => return int (l / r)
  | _,_,_ => throw "unimp"
where
  int (i : Int) : Term := .expr (.const (.int i)) .int

-- Binary operators on tensors (see Tensor.lean)
private def exprOp : String -> Expr -> Expr -> TraceM Term
  -- tensors
  | op, .tensor l, .tensor r => tensor_tensor op l r
  | op, .tensor t, .const  c => tensor_scalar op t c
  | op, .const  c, .tensor t => scalar_tensor op c t
  | _ , .tensor _, _
  | _ , _        , .tensor _ => throw "invalid tensor op"
  -- constants
  | op, .const  l, .const  r => constOp op l r
  | _ , _        , _         => throw "non-constant expression"

-- Binary operators on terms
def binOp : String -> Term -> Term -> TraceM Term
  -- lists and tuples
  | "Add",  .list   l,          .list   r => return .list (l ++ r)
  | "Add",  .tuple  l,          .tuple  r => return .tuple (l ++ r)
  | "Mult", .list   l,          .expr (.const  c) _
  | "Mult", .expr (.const c) _, .list l   => return .list (<- mulseq l c)
  | "Mult", .tuple  l,          .expr (.const  c) _
  | "Mult", .expr (.const c) _, .tuple l  => return .tuple (<- mulseq l c)
  | op   ,  .expr l _,          .expr r _ => exprOp op l r
  | _, _, _ => throw "unsupported operator"

-- Unary operators
def unOp : String -> Term -> TraceM Term
  | op   , .expr (.tensor t) _ => tensor_op op t
  | "Not", t                   => return .expr (.const $ .bool (<- t.isFalse)) .bool
  | op, _ => throw s!"unimp {op}"

-- Comparison operators

def cmpOp : String -> Term -> Term -> TraceM Bool
  | s, l, r => throw s!"unsupported comparison operator {s} {repr l} {repr r}"

def compare : Term -> List String -> List Term -> TraceM Term
  | x, [op], [y] => return bool (<- cmpOp op x y)
  | x, op::ops, y::ys => do
     if (<- cmpOp op x y)
     then compare y ops ys
     else return (bool false)
  | _, _, _ => throw "invalid comparison"
where
  bool b := .expr (.const $ .bool b) .bool

def Term.attr : Term -> String -> TraceM Term
  | .object o, id => o.attr id
  | .expr _ (.tensor d _), "dtype" => return (str d)
  | .expr _ (.tensor _ s), "shape" => return (list s)
  | .expr e _, id => throw s!"unsupported attribute {id} on {repr e}"
  | t, id => throw s!"unsupported attribute {id} on {repr t}"
where
  str s  := .expr (.const $ .string s) .string
  list l := .list $ l.map fun i => .expr (.const (.int i)) .int

def Item.attr : Item -> String -> Tracer Item
  | .module n, id => lookup_global (n.str id)
  | .global g, id => return .term (<- g.attr id)
  | .source _, id => throw s!"unsupported attribute {id}"
  | .term   t, id => return .term (<- t.attr id)

def Term.call (f : Term)
              (args : List Expr)
              (kws : List (String × Expr)) : TraceM Term := do
  match f with
  | .object o => o.call args kws
  | .tuple _  => throw "tuple is not a callable type"
  | .list _   => throw "list is not a callable type"
  | .expr f _ => return .expr (.call f args kws) (.any "?".toName)
