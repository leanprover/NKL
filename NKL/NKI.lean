/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import Lean

/-!
# Concrete Syntax of NKI kernels

Representation of the "concrete" syntax of NKI kernels
generated by the python frontend.
-/

namespace NKL

inductive Const where
  | nil
  | bool (value: Bool)
  | int (value: Int)
  | float (value: Float)
  | string (value: String)
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

inductive BinOp where
  | And | Or
  | Eq | NotEq | Lt | LtE | Gt | GtE
  | Add | Sub | Mul | Div
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

mutual
inductive Expr where
  | value (c: Const)
  | bvar (name: String)
  | var (name: String)
  | subscript (tensor: String) (ix: Array Index)
  | binop (op: BinOp) (left right: Expr)
  | call (f: String) (args: Array Expr)
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

inductive Index where
  | coord (i : Expr)
  | slice (l u step: Expr)
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson
end

inductive Stmt where
  | ret(e: Expr)
  | assign (x: String) (e: Expr)
  | forloop (x: String) (iter: Expr) (body: List Stmt)
  | gridcall (f: String) (ix: Array Index) (args: Array Expr)
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

structure Arg where
  name  : String
  type  : Option String := .none
  value : Option Const := .none
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

structure Fun where
  name : String
  args : Array Arg
  body : List Stmt
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson
