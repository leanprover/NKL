/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau, Sean McLaughlin
-/
import Lean

/-!
# Abstract syntax of Core NKL language

This language is the result of "tracing", and is used as the
portable format, a.k.a. Kernel Language Representation (KLR).
-/

namespace KLR.Core

-- TODO switch to TensorLib's version of these types
--export TensorLib (Tensor Dtype Shape)

abbrev Dtype := String
abbrev Shape := List Int

/-
A TensorName is essentially a typed variable, where the type
must be a tensor type. When we flush out Typ below we may replace
this with `Expr.var name (Typ.tensor dtype shape)`. For now, this
only refers to dynamic tensors, or compile-time tensors, not
trace-time tensors.
-/

structure TensorName where
  name  : String
  dtype : Dtype
  shape : Shape
  deriving Repr, BEq, Lean.ToJson

-- TODO
inductive Typ where

inductive Const where
  | none
  | bool (value : Bool)
  | int (value : Int)
  | float (value : Float)
  | string (value : String)
  deriving Repr, BEq, Lean.ToJson

-- This corresponds to the "Quasi-Affine Expressions" in Neuron.
-- Note, `floor` is the usual integer division.
inductive IndexExpr where
  | var (name : String)
  | int (i : Int)
  | neg (expr : IndexExpr)
  | add (left right : IndexExpr)
  | mul (scalar : Int) (expr : IndexExpr)
  | floor (expr : IndexExpr) (scalar : Int)
  | ceil (expr : IndexExpr) (scalar : Int)
  | mod (expr : IndexExpr) (scalar : Int)
  deriving Repr, BEq, Lean.ToJson

-- Note: `np.newindex` is represented as `(.coord none)`
inductive Index where
  | ellipsis
  | coord (e : Option IndexExpr)
  | slice (l u step : Option IndexExpr)
  deriving Repr, BEq, Lean.ToJson

inductive Expr where
  | var (x : String)
  | const (c : Const)
  | tensor (t : TensorName)
  | access (t : Expr) (ix : List Index)
  | call (f : Expr) (args : List Expr) (kwargs : List (String × Expr))
  deriving Repr, BEq, Lean.ToJson

inductive Stmt where
  | pass
  | expr (v : Expr)
  | ret (v : Expr)
  | assign (x : String) (e : Expr)
  | loop (x : String) (l u step : IndexExpr) (body : List Stmt)
  deriving Repr, BEq, Lean.ToJson
