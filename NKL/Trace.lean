/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import NKL.KLR
import NKL.Python
import NKL.Trace.Types
import NKL.Trace.Basic
import NKL.Trace.Builtin
import NKL.Trace.Python
import NKL.Trace.NKI

namespace NKL.Trace

def runNKIKernel (k : NKL.Python.Kernel) : Except String (List NKL.KLR.Stmt) :=
  tracer ⟨ .ofList NKIEnv, #[] ⟩ do
    traceKernel k
    let g <- get
    return g.body.toList
