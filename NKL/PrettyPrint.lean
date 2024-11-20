/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/

import NKL.NKI

namespace NKL

instance : ToString Const where
  toString
  | .nil => "None"
  | .bool b => toString b
  | .int i => toString i
  | .float f => toString f
  | .string s => s
  | .dots => "..."

mutual
private partial def exps_ s l := String.intercalate s (List.map expr l)
private partial def exps := exps_ ","

private partial def expr : Expr -> String
  | .value c => toString c
  | .bvar s | .var s _ => s
  | .subscript e ix => expr e ++ "[" ++ exps ix ++ "]"
  | .slice l u s => exps_ ":" [l,u,s]
  | .binop op l r => op ++ "(" ++ expr l ++ "," ++ expr r ++ ")"
  | .cond e thn els => expr thn ++ " if " ++ expr e ++ " else " ++ expr els
  | .tuple es => "(" ++ exps es ++ ")"
  | .list es => "[" ++  exps es ++ "]"
  | .call f es => expr f ++ "(" ++ exps es ++ ")"
  | .gridcall f ix es => expr f ++ "[" ++ exps ix ++ "](" ++ exps es ++ ")"
end

instance : ToString Expr where
  toString := expr

mutual
private partial def stmts sp l :=
  String.intercalate "\n" $ List.map (stmt sp) l

private partial def stmt (sp : String) (stmt : Stmt) : String :=
  let stmts := stmts (sp ++ "  ")
  sp ++ match stmt with
  | .ret e => s!"ret {e}"
  | .assign x e => s!"{x} = {e}"
  | .ifstm e thn els => s!"if ({e}):\n{stmts thn}¬{sp}else:\n{stmts els}"
  | .forloop x e b => s!"for {x} in {expr e}:\n{stmts b}"
  | .check e => "assert(" ++ expr e ++ ")"
end

instance : ToString Stmt where
  toString := stmt ""

def print_nki (f : Fun) : IO Unit := do
  IO.println $ f.name ++"("++ String.intercalate "," f.args ++")"
  IO.println $ stmts "  " f.body

