/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau, Sean McLaughlin
-/

-- TODO This file is out of date

import KLR.Core.Basic

namespace KLR.Core

-- Some experimental definitions of semantics for expresssions
-- For now, I am abstracting over the types of tensors and
-- tensor operators.

-- Evaluating expressions results in values, which
-- may contain tensors and tensor operators
inductive Value (tensor op : Type) where
  | const (c : Const)
  | tensor (t : tensor) (ix : List (Value tensor op))
  | slice (l u step : Value tensor op)
  | oper (f : op)
  | tuple (l : List (Value tensor op))
  | list (l : List (Value tensor op))
  deriving Repr, BEq

-- The semantics is defined w.r.t. an environment that defines
-- the types of tensors and tensor operators, and provides
-- a lookup function for getting the values of variables.
class Env (a : Type) where
  Tensor : Type
  Oper : Type
  lookup? : a -> String -> Option (Value Tensor Oper)
  apply : a -> Oper -> List (Value Tensor Oper) -> Option (Value Tensor Oper)

-- A convenient abbreviation for Value types
abbrev Val (a :Type) [Env a] := Value (Env.Tensor a) (Env.Oper a)

-- TODO: should probably rename "nil" to something else
def nil [Env E]: Val E := .const .nil

def Env.lookup [Env a] (x : a) (s : String) : Val a :=
  Option.getD (Env.lookup? x s) nil

-- Operational semantics of Expressions
-- gridcall not included: this likely needs to be a statement

mutual
inductive StepExpr [Env E] : (env : E) -> Expr -> Val E -> Prop
 | bvar :
     Env.lookup? env a = some v ->
     StepExpr env (.bvar a) v
 | var :
     Env.lookup env a = v ->
     StepExpr env (.var a b) v
 | subscript :
     StepExpr env e (.tensor t []) ->
     StepList env le lv ->
     StepExpr env (.subscript e le) (.tensor t lv)
 | slice :
     StepExpr env l l' ->
     StepExpr env u u' ->
     StepExpr env step step' ->
     StepExpr env (.slice l u step) (.slice l' u' step')
 | binop :
     Env.lookup env op = .oper f ->
     StepExpr env left left' ->
     StepExpr env right right' ->
     Env.apply env f [left',right'] = some res ->
     StepExpr env (.binop op left right) res
 | cond_true :
     StepExpr env e (.const (.bool true)) ->
     StepExpr env t t' ->
     StepExpr env (.cond e t f) t'
 | cond_false :
     StepExpr env e (.const (.bool false)) ->
     StepExpr env f f' ->
     StepExpr env (.cond e t f) f'
 | tuple :
     StepList env l l' -> StepExpr env (.tuple l) (.tuple l')
 | list :
     StepList env l l' -> StepExpr env (.list l) (.list l')
 | call :
     StepExpr env f (.oper f') ->
     StepList env l l' ->
     Env.apply env f' l' = some v ->
     StepExpr env (.call f l) v

inductive StepList [Env E] : (env : E) -> List Expr -> List (Val E) -> Prop
  | nil : StepList env .nil .nil
  | cons : StepExpr env x x' -> StepList env l l' -> StepList env (x::l) (x'::l')
end

def ornil [Env E]: Option (Val E) -> Val E := flip Option.getD nil

-- A simple evaluator for expressions

mutual
def evalExpr [Env E] (env : E) : Expr -> Val E
  | .bvar s | .var s _ => Env.lookup env s
  | .subscript t l =>
     match evalExpr env t with
     | .tensor t [] => .tensor t (evalList env l)
     | _ => nil
  | .slice l u step => .slice (evalExpr env l) (evalExpr env u) (evalExpr env step)
  | .binop op l r =>
     match Env.lookup env op with
     | .oper f => ornil $ Env.apply env f [evalExpr env l, evalExpr env r]
     | _ => nil
  | .cond e t f =>
     match evalExpr env e with
     | .const (.bool true) => evalExpr env t
     | .const (.bool false) => evalExpr env f
     | _ => nil
  | .tuple l => .tuple (evalList env l)
  | .list l => .list (evalList env l)
  | .call f l =>
     match evalExpr env f with
     | .oper f => ornil $ Env.apply env f (evalList env l)
     | _ => nil
  | _ => nil

def evalList [Env E] (env : E) : List Expr -> List (Val E)
  | [] => []
  | x :: l => evalExpr env x :: evalList env l
end

-- Here is a simple theorem relating eval to the semantics in one direction
-- Proving the reverse direction is one goal of the type system (TBD)

mutual
theorem step_eval_expr [Env E] {v : Val E} (env : E) (h : StepExpr env e v) : evalExpr env e = v := by
  match h with
  | .bvar h =>
      unfold evalExpr Env.lookup Option.getD
      rw [h]
  | .var h =>
      unfold evalExpr
      rw [h]
  | .subscript h hl =>
      unfold evalExpr
      rw [step_eval_expr env h]
      rw [step_eval_list env hl]
  | .slice h1 h2 h3 =>
      unfold evalExpr
      rw [step_eval_expr env h1]
      rw [step_eval_expr env h2]
      rw [step_eval_expr env h3]
  | .binop h1 h2 h3 h4 =>
      unfold evalExpr
      rw [h1]; simp
      rw [step_eval_expr env h2]
      rw [step_eval_expr env h3]
      rw [h4]
      trivial
  | .cond_true h1 h2
  | .cond_false h1 h2 =>
      unfold evalExpr
      rw [step_eval_expr env h1]
      rw [step_eval_expr env h2]
  | .tuple h
  | .list h =>
      unfold evalExpr
      rw [step_eval_list env h]
  | .call h1 h2 h3 =>
      unfold evalExpr ornil Option.getD flip
      rw [step_eval_expr env h1]
      rw [step_eval_list env h2]
      simp
      rw [h3]
  done

theorem step_eval_list [Env E] {l' : List (Val E)} (env : E) (h : StepList env l l') : evalList env l = l' := by
  match h with
  | .nil => unfold evalList; trivial
  | .cons h1 h2 =>
      generalize (step_eval_expr env h1) = rw1
      generalize (step_eval_list env h2) = rw2
      unfold evalList
      rw [rw1,rw2]
  done
end
