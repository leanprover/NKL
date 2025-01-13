/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import NKL.KLR
import NKL.Trace.Types

/-
# Tracing for Tensor related operations

TODO: These are just place holders...
-/
namespace NKL.Trace
open NKL.KLR

private def tensor_call (op : String) (args : List Expr) : Term :=
  let type := if let .tensor t :: _ := args
              then TermType.tensor t.dtype t.shape
              else TermType.any "?".toName
  let name := Expr.var ("tensor_".append op)
  .expr (.call name args []) type

-- Unary operations on tensors

def tensor_op (op : String) (t : TensorName) : TraceM Term :=
  return tensor_call op [.tensor t]

-- Binary operations on tensors / scalars

def tensor_tensor (op : String) (l r : TensorName) : TraceM Term :=
  return tensor_call op [.tensor l, .tensor r]

private def broadcast (t : TensorName) (c : Const) : Expr :=
  let args := t.shape.map fun i => Expr.const (.int i)
  let args := .const c :: args
  .call (.var "broadcast") args []

def tensor_scalar (op : String) (t : TensorName) (c : Const) : TraceM Term :=
  return tensor_call op [ .tensor t, broadcast t c]

def scalar_tensor (op : String) (c : Const) (t : TensorName) : TraceM Term :=
  return tensor_call op [ .tensor t, broadcast t c]
