/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import NKL.KLR
import NKL.Trace.Types
import NKL.Trace.Builtin

/-
# NKI built-ins

This module defines the builtin constants used by tracing for NKI kernels.
-/
namespace NKL.Trace
open NKL.KLR

private def module (s : String) : Name × Item :=
  let name := s.toName
  (name, .module name)

private def const_var (s : String) : Name × Item :=
  let name := s.toName
  (name, .term (.expr (.var s) (.any name)))

/-
Note: this object contains a bunch of architecture parameters that
need to be set according to which HW we are compiling for.
TODO: figure out the mechanism for this.
-/
def tile_size : Global :=
  let name := "nki.langauge.tile_size".toName
  { name := name
  , attr := attrs
  , call := uncallable name
  }
where
  attrs : GlobalAttr
  | "pmax" => return .expr (.const $ .int 128) .int
  | a => throw s!"unsupported attribute {a}"

def NKIEnv : List (Name × Item) :=
  [ module "nki"
  , module "nki.language"
  , const_var "nki.language.add"
  , const_var "nki.language.load"
  , const_var "nki.language.store"
  , ("nki.language.tile_size".toName, .global tile_size)
  ]
