import Lean
import KLR.Core.Basic

/-
Support types for automatically generated instructions

This file contains type definitions required by the automatically
generated BIR Instructions.

Note: the naming of fields is carefully designed so that the derived
instances of to and from json are compatible with the compiler.
-/
namespace KLR.BIR

export Core(Index IndexExpr)

deriving instance Lean.ToJson for IndexExpr
deriving instance Lean.ToJson for Index

deriving instance Lean.FromJson for IndexExpr
deriving instance Lean.FromJson for Index

abbrev ShapeVector := List Nat
abbrev LoopAxis := String

inductive EngineType where
  | Unassigned
  | Pool
  | Activation
  | PE
  | DMA
  | DVE
  | SP
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive InstSyncType where
  | DataPathType
  | SequencerType
  | DMAType
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure APPair where
  step : Int := 1
  num : Nat := 1
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Instruction where
  opcode : String
  name : String
  engine : EngineType
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson
