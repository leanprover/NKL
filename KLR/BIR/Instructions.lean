/-
Copyright (c) 2025 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau, Sean McLaughlin
-/
import Lean
import KLR.BIR.Instruction

-- This file is automatically generated
namespace KLR.BIR

inductive ActivationFunctionType where
  | Abs
  | Abs_reciprocal_sqrt
  | Arctan
  | Copy
  | Derivative_Erf
  | Derivative_Gelu
  | Derivative_silu
  | Dsqrt
  | Erf
  | Exp
  | Gelu
  | Gelu_apprx_sigmoid
  | Gelu_apprx_tanh
  | Identity
  | Is_finite
  | Ln
  | Ln_prime
  | Lrelu
  | MemsetZero
  | Mish
  | Prelu
  | Reciprocal
  | Relu
  | Rsqrt
  | Sigmoid
  | Sign
  | Silu
  | Sin
  | Softplus
  | Sqrt
  | Square
  | Tanh
  | Unknown
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive EngineAccumulationType where
  | Accumulate
  | AddAccumulate
  | Idle
  | Zero
  | ZeroAccumulate
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive MatmultPerfMode where
  | DoubleColumn
  | DoublePixel
  | DoubleRow
  | None
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive MemsetMode where
  | Const
  | Random
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive DGEType where
  | HWDGE
  | None
  | SWDGE
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive PoolFunctionType where
  | Avg
  | Max
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive AxisListType where
  | C
  | X
  | XY
  | XYZ
  | XYZW
  | XYZWC
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive CopyMode where
  | CCE
  | Copy
  | Dynamic
  | Replicate
  | Transpose
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive TransposeOps where
  | None
  | WXZY
  | WYXZ
  | WZXY
  | XWZY
  | XYWZ
  | XZYW
  | YWZX
  | YXWZ
  | YXZW
  | ZWYX
  | ZYWX
  | ZYXW
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive CollectiveKind where
  | AllGather
  | AllReduce
  | AllToAll
  | Permute
  | PermuteImplicit
  | PermuteReduce
  | PermuteReduceImplicit
  | ReduceScatter
  | SendRecv
  | SendRecvCCE
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive CollectiveComputeTypeHint where
  | FSDP
  | None
  | TP
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive RandomAlgorithmKind where
  | LFSR
  | PCG32
  | PHILOX_1
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive RandomDistributionKind where
  | Binomial
  | Normal
  | Raw
  | Uniform
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

inductive BranchCompareOp where
  | IS_EQIMM
  | IS_EQREG
  | IS_GEIMM
  | IS_GEREG
  | IS_GTIMM
  | IS_GTREG
  | IS_LEIMM
  | IS_LEREG
  | IS_LTIMM
  | IS_LTREG
  | IS_NEIMM
  | IS_NEREG
  | Unsupported
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- Instruction

structure Activation extends Instruction where
  scale : Option Float := some 0
  alpha : Option Float := some 0
  can_read_uninit : Option Bool := some False
  func : ActivationFunctionType
  acc : Option EngineAccumulationType := some .Idle
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure ReadActivationAccumulator extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure LoadActFuncSet extends Instruction where
  act_func_set_id : Option Int := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Memset extends Instruction where
  mode : MemsetMode
  constant : Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure GetGlobalRankId extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure NoOp extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure EventSemaphore extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure AllEngineBarrier extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Drain extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Halt extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Pool extends Instruction where
  func : PoolFunctionType
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Reciprocal extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Shuffle extends Instruction where
  indices : List Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorCopy extends Instruction where
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorCopyDynamicSrc extends Instruction where
  scale : Option Nat := none
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorCopyDynamicDst extends Instruction where
  scale : Option Nat := none
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure IndirectCopy extends Instruction where
  num_valid_indices : Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorReduce extends Instruction where
  op : AluOp
  axis : AxisListType
  apply_transpose : Option Bool := some False
  negate : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorScalar extends Instruction where
  op0 : AluOp
  const0 : Float
  reverse0 : Bool
  op1 : AluOp
  const1 : Float
  reverse1 : Bool
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorScalarPtr extends Instruction where
  op0 : AluOp
  reverse0 : Bool
  op1 : AluOp
  reverse1 : Bool
  apply_transpose : Option Bool := some False
  is_tensor_scalar_addr : Option Bool := some False
  is_scalar_tensor_tensor : Option Bool := some False
  is_tensor_tensor_scan : Option Bool := some False
  acc : Option EngineAccumulationType := some .Idle
  negate_second_output : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorTensor extends Instruction where
  op : AluOp
  acc : Option EngineAccumulationType := some .Idle
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure BNStats extends Instruction where
  apply_transpose : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure BNStatsAggregate extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure BNGradients extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure BNBackprop extends Instruction where
  total_elements : Float
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure BNBackprop2 extends Instruction where
  total_elements : Float
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure StreamShuffle extends Instruction where
  mask : List Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CalcVarAddr extends Instruction where
  baseoffset : Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure StreamTranspose extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Select extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CopyPredicated extends Instruction where
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CustomOp extends Instruction where
  opFunctionName : String
  opLibFile : String
  ulib_to_ucode_version : String
  ulib_to_isa_version : String
  is_builtin : Option Bool := some False
  srcsShape : List ShapeVector
  dstsShape : List ShapeVector
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure BIRKernel extends Instruction where
  kernel_name : String
  srcs_shape : List ShapeVector
  dsts_shape : List ShapeVector
  kernel_attrs : List (String × String)
  sb_buf_shape : Nat × Nat
  psum_buf_shape : Nat × Nat × Nat
  auto_cast : String
  auto_cast_type : String
  is_causal : Option Bool := some False
  fused_rmsnorm : Option Bool := some True
  norm_type : Option Int := none
  lnc_size : Option Int := none
  lower_bound : Option Float := some 0
  store_add : Option Bool := some False
  quant_kernel : Option Bool := some False
  output_layout : Option Int := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure NKIKernel extends Instruction where
  func : String
  sb_buf_shape : Nat × Nat
  psum_buf_shape : Nat × Nat × Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure GetRandState extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure SetRandState extends Instruction where
  rng_engine : Engine
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Rand extends Instruction where
  random_algorithm : RandomAlgorithmKind
  rand_num_steps : Nat
  distribution : RandomDistributionKind
  params : List Float
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Iota extends Instruction where
  base : Option Int := some 0
  pattern : List APPair
  channel_multiplier : Option Int := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorScalarAffineSelect extends Instruction where
  base : Option Int := some 0
  pattern : List APPair
  channel_multiplier : Option Int := some 0
  compare_op : AluOp
  fill_value : Option Float := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Dropout extends Instruction where
  is_keep_threshold : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure GetCurProcessingRankID extends Instruction where
  iter_id : Nat
  channel_id : Nat
  stream_id : Option Int := some 0
  replica_groups : Option (List (List Nat)) := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure RegisterAlu extends Instruction where
  op : AluOp
  is_64bit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure RegisterMove extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorLoad extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorSave extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Call extends Instruction where
  target : Option String := none
  table_id : Option Int := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure SwitchQueueInstance extends Instruction where
  queue : Option String := none
  instance_name : Option String := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CoreBarrier extends Instruction where
  runtime_semaphore : Option Nat := some 0
  id : Nat
  cores : List Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Max extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure MaxIndex extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure MatchReplace extends Instruction where
  imm_value : Float
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Gather extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure InlineASMBytes extends Instruction where
  asm_bytes : String
  sync_type : InstSyncType
  latency_estimate : Int
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- Generic

structure Generic extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure GenericCopy extends Generic where
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure GenericRelu extends Generic where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure AbstractCopy extends Generic where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- MatmultBase

structure MatmultBase extends Instruction where
  start_tensor_calc : Bool
  stop_tensor_calc : Option Bool := some False
  replication_resolution : Option Nat := some 0
  replication_shift_amnt : Option Nat := some 0
  replication_num_rows : Option Nat := some 0
  is_transpose : Option Bool := some False
  is_fmap_onezero : Option Bool := some False
  is_weight_onezero : Option Bool := some False
  tile_size : Option (Nat × Nat) := some (0, 0)
  tile_position : Option (List (Nat × Nat)) := some [(0,0)]
  perf_mode : Option MatmultPerfMode := some .None
  ifmap_quant_offset : Option Nat := some 0
  weights_quant_offset : Option Nat := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Matmult extends MatmultBase where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure MatmultSparse extends MatmultBase where
  compress_ratio : Int
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- DMA

structure DMA extends Instruction where
  queue : Option String := none
  dge_type : Option DGEType := some .None
  duplicate : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Load extends DMA where
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Save extends DMA where
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure DMACopy extends DMA where
  mode : CopyMode
  oob_is_err : Option Bool := some True
  transpose_op : Option TransposeOps := some .None
  constants : Option (List Float) := none
  cce_op : Option AluOp := some .bypass
  replica_groups : Option (List (List Nat)) := none
  remote_notification : Option (List Nat) := some []
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure ReadVarAddr extends DMA where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure GenericIndirectLoad extends DMA where
  indirect_dims : Option (List Nat) := some []
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure IndirectLoad extends DMA where
  runtime_semaphore_wait_value : Option Nat := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure GenericIndirectSave extends DMA where
  indirect_dims : Option (List Nat) := some []
  op : Option AluOp := some .add
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure IndirectSave extends DMA where
  runtime_semaphore_wait_value : Option Nat := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure IndirectSaveAccumulate extends DMA where
  num_indices : Nat
  entry_step_elements : Nat
  op : Option AluOp := some .add
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure DMATrigger extends DMA where
  dma_blocks : Option (List String) := some []
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- Collective

structure Collective extends Instruction where
  queue : Option String := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CollectiveCompute extends Collective where
  kind : Option CollectiveKind := some .AllReduce
  replica_groups : Option (List (List Nat)) := none
  cc_channel : Option QuasiAffineExpr := none
  stream_id : Option Int := some 0
  src_target_pairs : Option (List (List Nat)) := none
  op : Option AluOp := some .bypass
  can_read_uninit : Option Bool := some False
  is_local : Option Bool := some False
  pipe_id : Option Nat := some 0
  recv_from_rank : Option (List QuasiAffineExpr) := some []
  send_to_rank : Option QuasiAffineExpr := none
  initial_corebarrier : Option Bool := some True
  cc_type_hint : Option CollectiveComputeTypeHint := some .None
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CollectiveSend extends Collective where
  peer_id : Nat
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CollectiveRecv extends Collective where
  peer_id : Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- DMADescriptor

structure DMADescriptor extends Instruction where
  num_tiling_dimensions : Option Nat := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure DMADescriptorCopy extends DMADescriptor where
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure DMADescriptorCCE extends DMADescriptor where
  constants : List Float
  op : AluOp
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure DMADescriptorTranspose extends DMADescriptor where
  transpose_op : TransposeOps
  can_read_uninit : Option Bool := some False
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure DMADescriptorReplicate extends DMADescriptor where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- Terminator

structure Terminator extends Instruction where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CompareAndBranch extends Terminator where
  comp_op : BranchCompareOp
  on_true : Option String := none
  on_false : Option String := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure UnconditionalBranch extends Terminator where
  target : Option String := none
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Return extends Terminator where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure Break extends Terminator where
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- Symbolic

structure MatmultSymbolic extends Instruction where
  replication_resolution : Nat
  replication_shift_amnt : Nat
  replication_num_rows : Nat
  start_tensor_calc : Bool
  stop_tensor_calc : Option Bool := some False
  is_transpose : Option Bool := some False
  rowgrp_symbolic : QuasiAffineExpr
  colgrp_symbolic : QuasiAffineExpr
  is_fmap_onezero : Option Bool := some False
  is_weight_onezero : Option Bool := some False
  tile_position : Option (List (Nat × Nat)) := some [(0,0)]
  perf_mode : Option MatmultPerfMode := some .None
  ifmap_quant_offset : Option Nat := some 0
  weights_quant_offset : Option Nat := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure ShuffleSymbolic extends Instruction where
  elts : List Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorScalarSymbolic extends Instruction where
  op0 : AluOp
  value0 : List Float
  expr0 : QuasiAffineExpr
  reverse0 : Bool
  op1 : AluOp
  value1 : List Float
  expr1 : QuasiAffineExpr
  reverse1 : Bool
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TongaReduceMacroSymbolic extends Instruction where
  op : AluOp
  is_first_reduce : Bool
  reduce_axes : List LoopAxis
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure CalcVarAddrSymbolic extends Instruction where
  expr_baseoffset : QuasiAffineExpr
  size : Nat
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure IotaSymbolic extends Instruction where
  base : QuasiAffineExpr
  pattern : List APPair
  channel_multiplier : Option Nat := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

structure TensorScalarAffineSelectSymbolic extends Instruction where
  base : QuasiAffineExpr
  pattern : List APPair
  channel_multiplier : Option Nat := some 0
  compare_op : AluOp
  fill_value : Option Float := some 0
  deriving BEq, Repr, Lean.FromJson, Lean.ToJson

-- Union of all instruction types
inductive Inst where
  | Activation : Activation -> Inst
  | ReadActivationAccumulator : ReadActivationAccumulator -> Inst
  | LoadActFuncSet : LoadActFuncSet -> Inst
  | Memset : Memset -> Inst
  | GetGlobalRankId : GetGlobalRankId -> Inst
  | NoOp : NoOp -> Inst
  | EventSemaphore : EventSemaphore -> Inst
  | AllEngineBarrier : AllEngineBarrier -> Inst
  | Drain : Drain -> Inst
  | Halt : Halt -> Inst
  | Pool : Pool -> Inst
  | Reciprocal : Reciprocal -> Inst
  | Shuffle : Shuffle -> Inst
  | TensorCopy : TensorCopy -> Inst
  | TensorCopyDynamicSrc : TensorCopyDynamicSrc -> Inst
  | TensorCopyDynamicDst : TensorCopyDynamicDst -> Inst
  | IndirectCopy : IndirectCopy -> Inst
  | TensorReduce : TensorReduce -> Inst
  | TensorScalar : TensorScalar -> Inst
  | TensorScalarPtr : TensorScalarPtr -> Inst
  | TensorTensor : TensorTensor -> Inst
  | BNStats : BNStats -> Inst
  | BNStatsAggregate : BNStatsAggregate -> Inst
  | BNGradients : BNGradients -> Inst
  | BNBackprop : BNBackprop -> Inst
  | BNBackprop2 : BNBackprop2 -> Inst
  | StreamShuffle : StreamShuffle -> Inst
  | CalcVarAddr : CalcVarAddr -> Inst
  | StreamTranspose : StreamTranspose -> Inst
  | Select : Select -> Inst
  | CopyPredicated : CopyPredicated -> Inst
  | CustomOp : CustomOp -> Inst
  | BIRKernel : BIRKernel -> Inst
  | NKIKernel : NKIKernel -> Inst
  | GetRandState : GetRandState -> Inst
  | SetRandState : SetRandState -> Inst
  | Rand : Rand -> Inst
  | Iota : Iota -> Inst
  | TensorScalarAffineSelect : TensorScalarAffineSelect -> Inst
  | Dropout : Dropout -> Inst
  | GetCurProcessingRankID : GetCurProcessingRankID -> Inst
  | RegisterAlu : RegisterAlu -> Inst
  | RegisterMove : RegisterMove -> Inst
  | TensorLoad : TensorLoad -> Inst
  | TensorSave : TensorSave -> Inst
  | Call : Call -> Inst
  | SwitchQueueInstance : SwitchQueueInstance -> Inst
  | CoreBarrier : CoreBarrier -> Inst
  | Max : Max -> Inst
  | MaxIndex : MaxIndex -> Inst
  | MatchReplace : MatchReplace -> Inst
  | Gather : Gather -> Inst
  | InlineASMBytes : InlineASMBytes -> Inst
  | Generic : Generic -> Inst
  | GenericCopy : GenericCopy -> Inst
  | GenericRelu : GenericRelu -> Inst
  | AbstractCopy : AbstractCopy -> Inst
  | MatmultBase : MatmultBase -> Inst
  | Matmult : Matmult -> Inst
  | MatmultSparse : MatmultSparse -> Inst
  | DMA : DMA -> Inst
  | Load : Load -> Inst
  | Save : Save -> Inst
  | DMACopy : DMACopy -> Inst
  | ReadVarAddr : ReadVarAddr -> Inst
  | GenericIndirectLoad : GenericIndirectLoad -> Inst
  | IndirectLoad : IndirectLoad -> Inst
  | GenericIndirectSave : GenericIndirectSave -> Inst
  | IndirectSave : IndirectSave -> Inst
  | IndirectSaveAccumulate : IndirectSaveAccumulate -> Inst
  | DMATrigger : DMATrigger -> Inst
  | Collective : Collective -> Inst
  | CollectiveCompute : CollectiveCompute -> Inst
  | CollectiveSend : CollectiveSend -> Inst
  | CollectiveRecv : CollectiveRecv -> Inst
  | DMADescriptor : DMADescriptor -> Inst
  | DMADescriptorCopy : DMADescriptorCopy -> Inst
  | DMADescriptorCCE : DMADescriptorCCE -> Inst
  | DMADescriptorTranspose : DMADescriptorTranspose -> Inst
  | DMADescriptorReplicate : DMADescriptorReplicate -> Inst
  | Terminator : Terminator -> Inst
  | CompareAndBranch : CompareAndBranch -> Inst
  | UnconditionalBranch : UnconditionalBranch -> Inst
  | Return : Return -> Inst
  | Break : Break -> Inst
  | MatmultSymbolic : MatmultSymbolic -> Inst
  | ShuffleSymbolic : ShuffleSymbolic -> Inst
  | TensorScalarSymbolic : TensorScalarSymbolic -> Inst
  | TongaReduceMacroSymbolic : TongaReduceMacroSymbolic -> Inst
  | CalcVarAddrSymbolic : CalcVarAddrSymbolic -> Inst
  | IotaSymbolic : IotaSymbolic -> Inst
  | TensorScalarAffineSelectSymbolic : TensorScalarAffineSelectSymbolic -> Inst
  deriving BEq, Repr

instance : Lean.ToJson Inst where
  toJson
  | .Activation x => tagObj "opcode" "Activation" x
  | .ReadActivationAccumulator x => tagObj "opcode" "ReadActivationAccumulator" x
  | .LoadActFuncSet x => tagObj "opcode" "LoadActFuncSet" x
  | .Memset x => tagObj "opcode" "Memset" x
  | .GetGlobalRankId x => tagObj "opcode" "GetGlobalRankId" x
  | .NoOp x => tagObj "opcode" "NoOp" x
  | .EventSemaphore x => tagObj "opcode" "EventSemaphore" x
  | .AllEngineBarrier x => tagObj "opcode" "AllEngineBarrier" x
  | .Drain x => tagObj "opcode" "Drain" x
  | .Halt x => tagObj "opcode" "Halt" x
  | .Pool x => tagObj "opcode" "Pool" x
  | .Reciprocal x => tagObj "opcode" "Reciprocal" x
  | .Shuffle x => tagObj "opcode" "Shuffle" x
  | .TensorCopy x => tagObj "opcode" "TensorCopy" x
  | .TensorCopyDynamicSrc x => tagObj "opcode" "TensorCopyDynamicSrc" x
  | .TensorCopyDynamicDst x => tagObj "opcode" "TensorCopyDynamicDst" x
  | .IndirectCopy x => tagObj "opcode" "IndirectCopy" x
  | .TensorReduce x => tagObj "opcode" "TensorReduce" x
  | .TensorScalar x => tagObj "opcode" "TensorScalar" x
  | .TensorScalarPtr x => tagObj "opcode" "TensorScalarPtr" x
  | .TensorTensor x => tagObj "opcode" "TensorTensor" x
  | .BNStats x => tagObj "opcode" "BNStats" x
  | .BNStatsAggregate x => tagObj "opcode" "BNStatsAggregate" x
  | .BNGradients x => tagObj "opcode" "BNGradients" x
  | .BNBackprop x => tagObj "opcode" "BNBackprop" x
  | .BNBackprop2 x => tagObj "opcode" "BNBackprop2" x
  | .StreamShuffle x => tagObj "opcode" "StreamShuffle" x
  | .CalcVarAddr x => tagObj "opcode" "CalcVarAddr" x
  | .StreamTranspose x => tagObj "opcode" "StreamTranspose" x
  | .Select x => tagObj "opcode" "Select" x
  | .CopyPredicated x => tagObj "opcode" "CopyPredicated" x
  | .CustomOp x => tagObj "opcode" "CustomOp" x
  | .BIRKernel x => tagObj "opcode" "BIRKernel" x
  | .NKIKernel x => tagObj "opcode" "NKIKernel" x
  | .GetRandState x => tagObj "opcode" "GetRandState" x
  | .SetRandState x => tagObj "opcode" "SetRandState" x
  | .Rand x => tagObj "opcode" "Rand" x
  | .Iota x => tagObj "opcode" "Iota" x
  | .TensorScalarAffineSelect x => tagObj "opcode" "TensorScalarAffineSelect" x
  | .Dropout x => tagObj "opcode" "Dropout" x
  | .GetCurProcessingRankID x => tagObj "opcode" "GetCurProcessingRankID" x
  | .RegisterAlu x => tagObj "opcode" "RegisterAlu" x
  | .RegisterMove x => tagObj "opcode" "RegisterMove" x
  | .TensorLoad x => tagObj "opcode" "TensorLoad" x
  | .TensorSave x => tagObj "opcode" "TensorSave" x
  | .Call x => tagObj "opcode" "Call" x
  | .SwitchQueueInstance x => tagObj "opcode" "SwitchQueueInstance" x
  | .CoreBarrier x => tagObj "opcode" "CoreBarrier" x
  | .Max x => tagObj "opcode" "Max" x
  | .MaxIndex x => tagObj "opcode" "MaxIndex" x
  | .MatchReplace x => tagObj "opcode" "MatchReplace" x
  | .Gather x => tagObj "opcode" "Gather" x
  | .InlineASMBytes x => tagObj "opcode" "InlineASMBytes" x
  | .Generic x => tagObj "opcode" "Generic" x
  | .GenericCopy x => tagObj "opcode" "GenericCopy" x
  | .GenericRelu x => tagObj "opcode" "GenericRelu" x
  | .AbstractCopy x => tagObj "opcode" "AbstractCopy" x
  | .MatmultBase x => tagObj "opcode" "MatmultBase" x
  | .Matmult x => tagObj "opcode" "Matmult" x
  | .MatmultSparse x => tagObj "opcode" "MatmultSparse" x
  | .DMA x => tagObj "opcode" "DMA" x
  | .Load x => tagObj "opcode" "Load" x
  | .Save x => tagObj "opcode" "Save" x
  | .DMACopy x => tagObj "opcode" "DMACopy" x
  | .ReadVarAddr x => tagObj "opcode" "ReadVarAddr" x
  | .GenericIndirectLoad x => tagObj "opcode" "GenericIndirectLoad" x
  | .IndirectLoad x => tagObj "opcode" "IndirectLoad" x
  | .GenericIndirectSave x => tagObj "opcode" "GenericIndirectSave" x
  | .IndirectSave x => tagObj "opcode" "IndirectSave" x
  | .IndirectSaveAccumulate x => tagObj "opcode" "IndirectSaveAccumulate" x
  | .DMATrigger x => tagObj "opcode" "DMATrigger" x
  | .Collective x => tagObj "opcode" "Collective" x
  | .CollectiveCompute x => tagObj "opcode" "CollectiveCompute" x
  | .CollectiveSend x => tagObj "opcode" "CollectiveSend" x
  | .CollectiveRecv x => tagObj "opcode" "CollectiveRecv" x
  | .DMADescriptor x => tagObj "opcode" "DMADescriptor" x
  | .DMADescriptorCopy x => tagObj "opcode" "DMADescriptorCopy" x
  | .DMADescriptorCCE x => tagObj "opcode" "DMADescriptorCCE" x
  | .DMADescriptorTranspose x => tagObj "opcode" "DMADescriptorTranspose" x
  | .DMADescriptorReplicate x => tagObj "opcode" "DMADescriptorReplicate" x
  | .Terminator x => tagObj "opcode" "Terminator" x
  | .CompareAndBranch x => tagObj "opcode" "CompareAndBranch" x
  | .UnconditionalBranch x => tagObj "opcode" "UnconditionalBranch" x
  | .Return x => tagObj "opcode" "Return" x
  | .Break x => tagObj "opcode" "Break" x
  | .MatmultSymbolic x => tagObj "opcode" "MatmultSymbolic" x
  | .ShuffleSymbolic x => tagObj "opcode" "ShuffleSymbolic" x
  | .TensorScalarSymbolic x => tagObj "opcode" "TensorScalarSymbolic" x
  | .TongaReduceMacroSymbolic x => tagObj "opcode" "TongaReduceMacroSymbolic" x
  | .CalcVarAddrSymbolic x => tagObj "opcode" "CalcVarAddrSymbolic" x
  | .IotaSymbolic x => tagObj "opcode" "IotaSymbolic" x
  | .TensorScalarAffineSelectSymbolic x => tagObj "opcode" "TensorScalarAffineSelectSymbolic" x

instance : Lean.FromJson Inst where
  fromJson? j := do
  let name <- j.getObjValAs? String "opcode"
  match name with
  | "Activation" => return .Activation (<- Lean.fromJson? j)
  | "ReadActivationAccumulator" => return .ReadActivationAccumulator (<- Lean.fromJson? j)
  | "LoadActFuncSet" => return .LoadActFuncSet (<- Lean.fromJson? j)
  | "Memset" => return .Memset (<- Lean.fromJson? j)
  | "GetGlobalRankId" => return .GetGlobalRankId (<- Lean.fromJson? j)
  | "NoOp" => return .NoOp (<- Lean.fromJson? j)
  | "EventSemaphore" => return .EventSemaphore (<- Lean.fromJson? j)
  | "AllEngineBarrier" => return .AllEngineBarrier (<- Lean.fromJson? j)
  | "Drain" => return .Drain (<- Lean.fromJson? j)
  | "Halt" => return .Halt (<- Lean.fromJson? j)
  | "Pool" => return .Pool (<- Lean.fromJson? j)
  | "Reciprocal" => return .Reciprocal (<- Lean.fromJson? j)
  | "Shuffle" => return .Shuffle (<- Lean.fromJson? j)
  | "TensorCopy" => return .TensorCopy (<- Lean.fromJson? j)
  | "TensorCopyDynamicSrc" => return .TensorCopyDynamicSrc (<- Lean.fromJson? j)
  | "TensorCopyDynamicDst" => return .TensorCopyDynamicDst (<- Lean.fromJson? j)
  | "IndirectCopy" => return .IndirectCopy (<- Lean.fromJson? j)
  | "TensorReduce" => return .TensorReduce (<- Lean.fromJson? j)
  | "TensorScalar" => return .TensorScalar (<- Lean.fromJson? j)
  | "TensorScalarPtr" => return .TensorScalarPtr (<- Lean.fromJson? j)
  | "TensorTensor" => return .TensorTensor (<- Lean.fromJson? j)
  | "BNStats" => return .BNStats (<- Lean.fromJson? j)
  | "BNStatsAggregate" => return .BNStatsAggregate (<- Lean.fromJson? j)
  | "BNGradients" => return .BNGradients (<- Lean.fromJson? j)
  | "BNBackprop" => return .BNBackprop (<- Lean.fromJson? j)
  | "BNBackprop2" => return .BNBackprop2 (<- Lean.fromJson? j)
  | "StreamShuffle" => return .StreamShuffle (<- Lean.fromJson? j)
  | "CalcVarAddr" => return .CalcVarAddr (<- Lean.fromJson? j)
  | "StreamTranspose" => return .StreamTranspose (<- Lean.fromJson? j)
  | "Select" => return .Select (<- Lean.fromJson? j)
  | "CopyPredicated" => return .CopyPredicated (<- Lean.fromJson? j)
  | "CustomOp" => return .CustomOp (<- Lean.fromJson? j)
  | "BIRKernel" => return .BIRKernel (<- Lean.fromJson? j)
  | "NKIKernel" => return .NKIKernel (<- Lean.fromJson? j)
  | "GetRandState" => return .GetRandState (<- Lean.fromJson? j)
  | "SetRandState" => return .SetRandState (<- Lean.fromJson? j)
  | "Rand" => return .Rand (<- Lean.fromJson? j)
  | "Iota" => return .Iota (<- Lean.fromJson? j)
  | "TensorScalarAffineSelect" => return .TensorScalarAffineSelect (<- Lean.fromJson? j)
  | "Dropout" => return .Dropout (<- Lean.fromJson? j)
  | "GetCurProcessingRankID" => return .GetCurProcessingRankID (<- Lean.fromJson? j)
  | "RegisterAlu" => return .RegisterAlu (<- Lean.fromJson? j)
  | "RegisterMove" => return .RegisterMove (<- Lean.fromJson? j)
  | "TensorLoad" => return .TensorLoad (<- Lean.fromJson? j)
  | "TensorSave" => return .TensorSave (<- Lean.fromJson? j)
  | "Call" => return .Call (<- Lean.fromJson? j)
  | "SwitchQueueInstance" => return .SwitchQueueInstance (<- Lean.fromJson? j)
  | "CoreBarrier" => return .CoreBarrier (<- Lean.fromJson? j)
  | "Max" => return .Max (<- Lean.fromJson? j)
  | "MaxIndex" => return .MaxIndex (<- Lean.fromJson? j)
  | "MatchReplace" => return .MatchReplace (<- Lean.fromJson? j)
  | "Gather" => return .Gather (<- Lean.fromJson? j)
  | "InlineASMBytes" => return .InlineASMBytes (<- Lean.fromJson? j)
  | "Generic" => return .Generic (<- Lean.fromJson? j)
  | "GenericCopy" => return .GenericCopy (<- Lean.fromJson? j)
  | "GenericRelu" => return .GenericRelu (<- Lean.fromJson? j)
  | "AbstractCopy" => return .AbstractCopy (<- Lean.fromJson? j)
  | "MatmultBase" => return .MatmultBase (<- Lean.fromJson? j)
  | "Matmult" => return .Matmult (<- Lean.fromJson? j)
  | "MatmultSparse" => return .MatmultSparse (<- Lean.fromJson? j)
  | "DMA" => return .DMA (<- Lean.fromJson? j)
  | "Load" => return .Load (<- Lean.fromJson? j)
  | "Save" => return .Save (<- Lean.fromJson? j)
  | "DMACopy" => return .DMACopy (<- Lean.fromJson? j)
  | "ReadVarAddr" => return .ReadVarAddr (<- Lean.fromJson? j)
  | "GenericIndirectLoad" => return .GenericIndirectLoad (<- Lean.fromJson? j)
  | "IndirectLoad" => return .IndirectLoad (<- Lean.fromJson? j)
  | "GenericIndirectSave" => return .GenericIndirectSave (<- Lean.fromJson? j)
  | "IndirectSave" => return .IndirectSave (<- Lean.fromJson? j)
  | "IndirectSaveAccumulate" => return .IndirectSaveAccumulate (<- Lean.fromJson? j)
  | "DMATrigger" => return .DMATrigger (<- Lean.fromJson? j)
  | "Collective" => return .Collective (<- Lean.fromJson? j)
  | "CollectiveCompute" => return .CollectiveCompute (<- Lean.fromJson? j)
  | "CollectiveSend" => return .CollectiveSend (<- Lean.fromJson? j)
  | "CollectiveRecv" => return .CollectiveRecv (<- Lean.fromJson? j)
  | "DMADescriptor" => return .DMADescriptor (<- Lean.fromJson? j)
  | "DMADescriptorCopy" => return .DMADescriptorCopy (<- Lean.fromJson? j)
  | "DMADescriptorCCE" => return .DMADescriptorCCE (<- Lean.fromJson? j)
  | "DMADescriptorTranspose" => return .DMADescriptorTranspose (<- Lean.fromJson? j)
  | "DMADescriptorReplicate" => return .DMADescriptorReplicate (<- Lean.fromJson? j)
  | "Terminator" => return .Terminator (<- Lean.fromJson? j)
  | "CompareAndBranch" => return .CompareAndBranch (<- Lean.fromJson? j)
  | "UnconditionalBranch" => return .UnconditionalBranch (<- Lean.fromJson? j)
  | "Return" => return .Return (<- Lean.fromJson? j)
  | "Break" => return .Break (<- Lean.fromJson? j)
  | "MatmultSymbolic" => return .MatmultSymbolic (<- Lean.fromJson? j)
  | "ShuffleSymbolic" => return .ShuffleSymbolic (<- Lean.fromJson? j)
  | "TensorScalarSymbolic" => return .TensorScalarSymbolic (<- Lean.fromJson? j)
  | "TongaReduceMacroSymbolic" => return .TongaReduceMacroSymbolic (<- Lean.fromJson? j)
  | "CalcVarAddrSymbolic" => return .CalcVarAddrSymbolic (<- Lean.fromJson? j)
  | "IotaSymbolic" => return .IotaSymbolic (<- Lean.fromJson? j)
  | "TensorScalarAffineSelectSymbolic" => return .TensorScalarAffineSelectSymbolic (<- Lean.fromJson? j)
  | _ => throw s!"unknown opcode {name}"
