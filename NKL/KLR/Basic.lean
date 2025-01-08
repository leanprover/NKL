/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
--import TensorLib.Tensor

/-!
# Abstract syntax of Core NKL language

This language is the result of "tracing", and is used as the
portable format, a.k.a. Kernel Language Representation (KLR).
-/

namespace NKL.KLR

-- TODO switch to tensor lib
--export TensorLib (Tensor Dtype Shape)
-- Mostly, NKL deals with empty tensors, so just check dtype and shape
-- TODO: talk to Sean about a more general BEq for Tensor
--instance : BEq Tensor where
--  beq t₁ t₂ := t₁.dtype == t₂.dtype && t₁.shape == t₂.shape

abbrev Dtype := String
abbrev Shape := List Int
structure Tensor where
  dtype : Dtype
  shape : Shape
  deriving Repr, BEq

-- TODO
inductive Typ where

inductive Const where
  | none
  | bool (value : Bool)
  | int (value : Int)
  | float (value : Float)
  | string (value : String)
  deriving Repr, BEq

namespace Const

-- Python-like rules for conversion to boolean
def isTrue : Const -> Bool
  | .none     => false
  | .bool b   => b
  | .int i    => i != 0
  | .float f  => f != 0.0
  | .string s => s != ""

-- Python-like rules for conversion to integer
def toInt : Const -> Except String Int
  | .none       => throw "none cannot be converted to an integer"
  | .bool true  => return 1
  | .bool false => return 0
  | .int i      => return i
  | .float f    =>
      -- Python is a bit strange here, it truncates both
      -- positive and negative numbers toward zero
      if f < 0.0 then
        return (Int.ofNat (Float.floor (-f)).toUInt64.toNat).neg
      else
        return Int.ofNat (Float.floor f).toUInt64.toNat
  | .string s   =>
      -- Fortunately, Lean's String.toInt appears to be compatible
      -- with Python's int(string) conversion.
      match s.toInt? with
      | .none  => throw s!"string {s} cannot be converted to an integer"
      | .some i => return i

end Const

-- This correspondes to the "Quasi-Affine Expressions" in Neuron.
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
  deriving Repr, BEq

-- Note: `np.newindex` is represented as `(.coord none)`
inductive Index where
  | ellipsis
  | coord (e : Option IndexExpr)
  | slice (l u step : Option IndexExpr)
  deriving Repr, BEq

inductive Expr where
  | var (x : String)
  | const (c : Const)
  | tensor (t : Tensor)
  | access (t : Expr) (ix : List Index)
  | call (f : Expr) (args : List Expr) (kwargs : List (String × Expr))
  deriving Repr, BEq

inductive Stmt where
  | pass
  | expr (v : Expr)
  | ret (v : Expr)
  | assign (x : String) (e : Expr)
  | loop (x : String) (l u step : IndexExpr) (body : List Stmt)
  deriving Repr, BEq
