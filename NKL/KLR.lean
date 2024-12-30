/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/


/-!
# Abstract syntax of Core NKL language

This language is the result of "tracing", and is used as the
portable format, a.k.a. Kernel Language Representation (KLR).
-/

namespace NKL.KLR

-- TODO
inductive Ty where

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
        return (Int.ofNat (Float.floor (-f)).toUInt64.toBitVec.toNat).neg
      else
        return Int.ofNat (Float.floor f).toUInt64.toBitVec.toNat
  | .string s   =>
      match s.toInt? with
      | .none  => throw s!"string {s} cannot be converted to an integer"
      | .some i => return i

end Const

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

inductive Index where
  | ellipsis
  | coord (e : Option IndexExpr)
  | range (l u step : Option IndexExpr)
  deriving Repr, BEq

inductive Expr where
  | var (x : String)
  | const (c : Const)
  | tensor (name : String) (shape : List Int)
  | tuple (xs : List Expr)
  | list (xs : List Expr)
  | access (t : Expr) (ix : List Index)
  | binop (op : String) (left right : Expr)
  | unop (op : String) (e : Expr)
  | call (f : Expr) (args : List Expr) (keywords : List (String × Expr))
  deriving Repr, BEq

namespace Expr

-- TODO: Just a place-holder for now
def toAffine : Expr -> Except String IndexExpr
  | .var v => return .var v
  | .const (.int i) => return .int i
  | e => throw s!"toAffine unimp {repr e}"

-- TODO: Just a place-holder for now
def simplify : Expr -> Expr :=
  fun x => x

end Expr

inductive Stmt where
  | pass
  | expr (v : Expr)
  | ret (v : Expr)
  | assign (x : String) (e : Expr)
  | loop (x : String) (l u step : IndexExpr) (body : List Stmt)
  deriving Repr, BEq
