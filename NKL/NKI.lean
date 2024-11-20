/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import Lean

/-!
# Syntax of NKI kernels

Representation for the abstract syntax of NKI kernels
generated by the python frontend.
-/

namespace NKL

inductive Const where
  | nil
  | bool (value: Bool)
  | int (value: Int)
  | float (value: Float)
  | string (value: String)
  | dots
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

inductive Expr where
  | value (c: Const)
  | bvar (name: String)
  | var (name value: String)
  | subscript (tensor: Expr) (ix: List Expr)
  | slice (l u step: Expr)
  | binop (op: String) (left right: Expr)
  | cond (e thn els: Expr)
  | tuple (xs: List Expr)
  | list (xs: List Expr)
  | call (f: Expr) (args: List Expr)
  | gridcall (f: Expr) (ix: List Expr) (args: List Expr)
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

inductive Stmt where
  | ret (e: Expr)
  | assign (x: Expr) (e: Expr)
  | ifstm (e : Expr) (thn els: List Stmt)
  | forloop (x: String) (iter: Expr) (body: List Stmt)
  | check (e : Expr)
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

--structure Arg where
--  name  : String
--  type  : Option String := .none
--  value : Option Const := .none
--  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

structure Fun where
  name : String
  args : List String
  body : List Stmt
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson
