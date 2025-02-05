/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import KLR.Core
import KLR.Trace.Types

/-
# Utilities for creating Builtins and Globals

-/

namespace KLR.Trace
open KLR.Core

abbrev BuiltinAttr := String -> Err Term
abbrev GlobalAttr  := String -> TraceM Term

abbrev BuiltinFn := List Expr -> List (String × Expr) -> Err Term
abbrev GlobalFn  := List Term -> List (String × Term) -> TraceM Term

def noAttr [Monad m] [MonadExcept String m] : Name -> String -> m a :=
  fun name attr => throw s!"{attr} is not an attribute of {name}"

def noAccess [Monad m] [MonadExcept String m] : Name -> a -> m b :=
  fun name _ => throw s!"{name} does not support subscript"

def noIndex [Monad m] [MonadExcept String m] : Name -> a -> b -> m c :=
  fun name _ _ => throw s!"{name} cannot be used as an index"

def noBinop [Monad m] [MonadExcept String m] : Name -> a -> BinOp -> b -> m c :=
  fun name _ op _ => throw s!"{name} does not support operator {repr op}"

def uncallable [Monad m] [MonadExcept String m] : Name -> a -> b -> m c :=
  fun name _ _ => throw s!"{name} is not a callable type"

-- Note, this is more strict that python
def noKWArgs [Monad m] [MonadExcept String m]
              (f : List a -> m b) : (List a -> List c -> m b) :=
  fun args kwargs =>
    if kwargs.length > 0 then
      throw "invalid arguments (no kwargs supported)"
    else f args

-- Create a built-in representing a function; no attributes supported.

def simple_function (name : Name) (f : BuiltinFn) : Object :=
  { name := name
  , type := .obj name  -- TODO: use a function type
  , attr := noAttr name
  , access := noAccess name
  , index := noIndex name
  , binop := noBinop name
  , call := f
  }

-- Create a built-in representing a function; basic attributes supported.

def python_function (name : Name) (f : BuiltinFn) : Object :=
  { name := name
  , type := .obj name  -- TODO: use a function type
  , attr := attrs
  , access := noAccess name
  , index := noIndex name
  , binop := noBinop name
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
  , access := noAccess name
  , index := noIndex name
  , binop := noBinop name
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
  , attr := noAttr name
  , call := make_tuple
  }
where
  make_tuple : GlobalFn
  | args, [] => return .object (tuple_obj args)
  | _, _ => throw "invalid arguments"

-- A convenience class for building Objects

class Obj (a : Type) where
  name : Name
  type : TermType := .obj name
  attr : a -> String -> Err Term := fun _ => noAttr name
  access : a -> List Index -> Err Term := fun _ => noAccess name
  index : a -> TermType -> Nat -> Err Index := fun _ => noIndex name
  binop : a -> Bool -> BinOp -> Expr -> Err Term := fun _ => noBinop name
  call : a -> List Expr -> List (String × Expr) -> Err Term := fun _ => uncallable name

def toObject [inst: Obj a] (x : a) : Object :=
  { name := inst.name
  , type := inst.type
  , attr := inst.attr x
  , access := inst.access x
  , index := inst.index x
  , binop := inst.binop x
  , call := inst.call x
  }
