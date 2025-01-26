/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import NKL.KLR
import NKL.Trace.Types

/-
# Utilities for creating Builtins and Globals

-/

namespace NKL.Trace
open NKL.KLR

abbrev BuiltinAttr := String -> Err Term
abbrev GlobalAttr  := String -> TraceM Term

abbrev BuiltinFn := List Expr -> List (String × Expr) -> Err Term
abbrev GlobalFn  := List Term -> List (String × Term) -> TraceM Term

def noattrs [Monad m] [MonadExcept String m] : Name -> String -> m a :=
  fun name attr => throw s!"{attr} is not an attribute of {name}"

def uncallable [Monad m] [MonadExcept String m] : Name -> a -> b -> m c :=
  fun name _ _ => throw s!"{name} is not a callable type"

-- Create a built-in representing a function; no attributes supported.

def simple_function (name : Name) (f : BuiltinFn) : Object :=
  { name := name
  , type := .any name
  , attr := noattrs name
  , call := f
  }

-- Create a built-in representing a function; basic attributes supported.

def python_function (name : Name) (f : BuiltinFn) : Object :=
  { name := name
  , type := .any name
  , attr := attrs
  , call := f
  }
where
  attrs : BuiltinAttr
  | "__name__" => return .expr (.const $ .string name.toString) .string
  | "__call__" => return .object (simple_function name f)
  | a => throw s!"unsupported attribute {a}"

-- Create a built-in representing a simple object from a list of attributes.

def simple_object {a : Type}
                  (name : Name)
                  (attrs : List (String × (a -> BuiltinFn)))
                  (x : a) : Object :=
  { name := name
  , type := .none
  , attr := attr_fn
  , call := uncallable name
  }
where
  attr_fn (attr : String) : Err Term :=
    match attrs.find? (fun x => x.fst == attr) with
    | none => .error s!"{attr} is not an attribute of {name}"
    | some (_,fn) => .ok (.object $ simple_function (name.str attr) (fn x))


-- Basic Python types could be represented as built-ins. In practice, we don't
-- do this as it is more convenient to have tuples, etc. represented directly
-- in the `Term` type. However, as an example, the tuple class may be defined
-- similar to below.

-- Note: not used
def tuple_obj : List Term -> Object :=
  simple_object "tuple".toName
    [ ("count", fun l _ _ => .ok (.expr (.const $ .int l.length) .int))
    , ("index", index_fn)
    ]
where
  index_fn : List Term -> BuiltinFn
  | l, [x], [] => match l.indexOf? (.expr x .none) with
                  | none => throw s!"{repr x} not in tuple"
                  | some i => return .expr (.const $ .int i) .int
  | _, _, _ => throw "invalid arguments"

-- Note: not used
def tuple_class : Global :=
  let name := "class_tuple".toName
  { name := name
  , attr := noattrs name
  , call := make_tuple
  }
where
  make_tuple : GlobalFn
  | args, [] => return .object (tuple_obj args)
  | _, _ => throw "invalid arguments"
