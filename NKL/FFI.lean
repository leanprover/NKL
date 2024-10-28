/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import Lean
import NKL.NKI

namespace NKL

-- temporary for testing

@[export parse_json]
def parse_json (json : String) : IO Unit := do
  match Lean.Json.parse json with
  | .error str => throw $ .userError str
  | .ok jsn => do
    match Lean.fromJson? jsn with
    | .error str => throw $ .userError str
    | .ok (_:Fun) => return ()
