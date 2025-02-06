import Lean
import KLR.BIR.Instruction
import KLR.BIR.Instructions

/-
Definitions of BIR container and function types.

This file contains BIR type definitions not produced by brewer.

Note: the naming of fields is carefully designed so that the derived
instances of to and from json are compatible with the compiler.
-/
namespace KLR.BIR

inductive StorageKind where
  | memory_location_set
  | memory_location
  | register
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive TensorKind where
  | ExternalInput
  | ExternalInputParameter
  | Input
  | ExternalOutput
  | ExternalOutputParameter
  | Output
  | Const
  | Internal
  | Pointer
  | InternalInterface
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive TensorClass where
  | Tensor
  | LoopTensor
  | SingleValueTensor
  | NeuronTensor
  | NeuronWeightTensor
  | IdentityWeightTensor
  | NeuronBlockTensor
  | DRAM2DBlockTensor
  | Neuron3DBlockTensor
  | DRAM3DBlockTensor
  | NeuronLocalTensor
  | NeuronSBTensor
  | NeuronPSUMTensor
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive AddrSpace where
  | Local | Shared
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive MemoryType where
  | Unallocated
  | Input
  | Output
  | DRAM
  | SB
  | PSUM
  | RNGSTATE
  | REG
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure MemoryLocation where
  Skind : StorageKind := .memory_location
  name : String
  type : MemoryType
  allocated : Option Bool := some false
  addr : Nat := 0
  dims : List Nat
  bank : Nat := 0
  base : Nat := 0
  pinned : Bool := false
  tensor_id : Option Nat := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Allocation where
  Skind : StorageKind := .memory_location_set
  addr_space : Option AddrSpace
  dtype : String
  partition_dim : Nat
  name : String
  kind : TensorKind
  no_spill : Bool := False
  volatile : Bool := False
  virtual : Bool := False
  memorylocations : List MemoryLocation
  tensorId2MemLocSize : Option Nat := some memorylocations.length
  tensor_shape : List Nat
  tensor_class : TensorClass
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson


instance : Lean.ToJson Inst where toJson := Inst.toJson
instance : Lean.FromJson Inst where fromJson? := Inst.fromJson?


structure Block where
  name : String
  instructions : List Inst
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Function where
  name : String
  attributes : List String
  allocations : List Allocation
  blocks : List Block
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Attributes where
  neuron_core_id : Nat := 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure BIR where
  attributes : Attributes := { }
  functions: List Function
  arch : String := "gen3"
  version : Nat := 2
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson
