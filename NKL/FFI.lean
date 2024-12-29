/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import Lean
import NKL.Python

namespace NKL

local instance : MonadLift (Except String) IO where
  monadLift
    | .ok x => return x
    | .error s => throw $ .userError s

@[export parse_json]
def parse_json (s : String) : IO Unit := do
  let kernel <- Python.Parsing.parse s
  let names := kernel.funcs.map fun x => x.fst
  let names := String.intercalate "," names
  IO.println s!"Found functions: {names}"
  for x in kernel.args do
    IO.println s!"arg: {repr x}"
  for x in kernel.kwargs do
    IO.println s!"arg: {repr x}"
  for x in kernel.globals do
    IO.println s!"global: {repr x}"
