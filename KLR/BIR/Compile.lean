/-
Copyright (c) 2025 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau, Sean McLaughlin
-/
import KLR.BIR.Instruction
import KLR.BIR.Instructions
import KLR.BIR.Function
import KLR.Core
import KLR.Util

namespace KLR.BIR
open KLR.Core

abbrev Compile := Err

def compileStore (_t : TensorName) (_ix : List Index) (_e : Expr) : Compile Inst :=
  return .NoOp { name := "noop" }

def compileStmt : Stmt -> Compile Inst
  | .ret _ => throw "unimp ret"
  | .store t ix e => compileStore t ix e
  | .assign .. => throw "unimp assign"
  | .loop .. => throw "unimp loop"

-- TODO: just a for instance...
def physicalShape (t : TensorName) : Shape :=
  match t.dtype, t.shape with
  | "float16", [x, y] => [x, y * 2]
  | "float32", [x, y] => [x, y * 4]
  | _, _ => t.shape -- TODO incorrect

-- Create memory regions corresponding to the tensors
def allocate (kind : TensorKind) (t : TensorName) : Compile Allocation := do
  let type : MemoryType :=
    match kind, t.memory with
    | .Input, _ => .Input
    | .Output, _ => .Output
    | _, .dram => .DRAM
    | _, .sbuf => .SB
    | _, .pmem => .PSUM
    | _, .reg => .REG
  return {
    addr_space := some .Shared  -- LNC ?
    dtype := some t.dtype
    partition_dim := some 0
    tensor_shape := t.shape
    name := t.name
    kind := kind
    memorylocations := [{
        name := t.name
        type := type
        dims := physicalShape t
      }]
    }

def compile_kernel (k : Kernel) : Compile BIR := do
  let inputs <- k.inputs.mapM (allocate .Input)
  let outputs <- k.outputs.mapM (allocate .Output)
  let internal <- k.internal.mapM (allocate .Internal)
  let allocs := inputs ++ outputs ++ internal
  let insts <- compileStmt ▷ k.body
  let insts := insts.map InstLoop.I
  -- There is alway one function with one block...
  return {
    functions := [{
      name := "sg0000"
      allocations := allocs
      blocks := [{
        name := "Block1"
        instructions := insts
        }]
    }]
  }

-- Will need this if we change Compile monad
def compile (klr : Kernel) : Err BIR :=
  compile_kernel klr
