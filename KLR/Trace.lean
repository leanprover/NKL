/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import KLR.Core
import KLR.Python
import KLR.Trace.Types
import KLR.Trace.Basic
import KLR.Trace.Builtin
import KLR.Trace.Python
import KLR.Trace.NKI

namespace KLR.Trace

def runNKIKernel (k : KLR.Python.Kernel) : Err (List KLR.Core.Stmt) :=
  tracer ⟨ .ofList NKIEnv, #[] ⟩ do
    traceKernel k
    let g <- get
    return g.body.toList
