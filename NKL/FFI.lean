/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import Lean
import NKL.KLR.Pretty
import NKL.Python
import NKL.Trace

namespace NKL
open NKL.KLR

local instance : MonadLift (Except String) IO where
  monadLift
    | .ok x => return x
    | .error s => throw $ .userError s

@[export parse_json]
def parse_json (s : String) : IO Unit := do
  let kernel <- Python.Parsing.parse s
  let stmts <- NKL.Trace.runNKIKernel kernel
  for s in stmts do
    IO.println ("  " ++ Lean.format s) --s!"{s}\n{repr s}"
