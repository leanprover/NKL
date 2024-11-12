/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import NKL.NKI

/-!
# Serialization and Deserialization

-/

namespace NKL

-- All of the encode function are pure; decoding uses an instance of EStateM.

abbrev DecodeM := EStateM String ByteArray.Iterator

def decode' (f : DecodeM a) (ba : ByteArray) : Option a :=
  EStateM.run' f ba.iter

def decode (f : DecodeM a) (ba : ByteArray) : Except String a :=
  match EStateM.run f ba.iter with
  | .ok x _ => .ok x
  | .error s _ => .error s

def decodeFile (f : DecodeM a) (path : System.FilePath) : IO a := do
 let buf <- IO.FS.readBinFile path
 match decode f buf with
 | .ok x => return x
 | .error s => throw $ IO.userError s

private def next : DecodeM UInt8 := do
  let it <- get
  if it.atEnd then throw "unexpected EOF"
  let b := it.curr
  set it.next
  return b

-- TODO: this could be more efficient if we complicate the monad state
private def take (n : Nat) : DecodeM ByteArray :=
  let rec f (ba : ByteArray) (n : Nat) : DecodeM ByteArray :=
    match n with
    | .zero => return ba
    | .succ n => next >>= fun b => f (ba.push b) n
  f (.mk #[]) n

------------------------------------------------------------------------------
-- Int64 is encoded as 8 bytes in little-endian order

private def encUInt64 (u : UInt64) : ByteArray :=
  let n := u.toBitVec.toFin.val
  .mk #[ UInt8.ofNat $ n >>> 0
       , UInt8.ofNat $ n >>> 8
       , UInt8.ofNat $ n >>> 16
       , UInt8.ofNat $ n >>> 24
       , UInt8.ofNat $ n >>> 32
       , UInt8.ofNat $ n >>> 40
       , UInt8.ofNat $ n >>> 48
       , UInt8.ofNat $ n >>> 56
       ]

private def decUInt64 : DecodeM UInt64 :=
  let u8_64 : DecodeM UInt64 := next >>= fun x => return x.toUInt64
  return (<- u8_64) <<< 0  |||
         (<- u8_64) <<< 8  |||
         (<- u8_64) <<< 16 |||
         (<- u8_64) <<< 24 |||
         (<- u8_64) <<< 32 |||
         (<- u8_64) <<< 40 |||
         (<- u8_64) <<< 48 |||
         (<- u8_64) <<< 56

private def encInt64 (i : Int64) : ByteArray := encUInt64 i.toUInt64
private def decInt64 : DecodeM Int64 := return ⟨ (<- decUInt64) ⟩

-- Int64 isn't supported by ToJson/FromJson deriving
-- For now, keep using Int in AST
-- TODO: switch to Int64 in AST
private def encInt (i : Int) : ByteArray := encInt64 (.ofInt i)
private def decInt : DecodeM Int := return (<- decInt64).toInt

private def encFloat (f : Float) : ByteArray := encUInt64 f.toBits
private def decFloat : DecodeM Float := return Float.ofBits (<- decUInt64)

local instance : BEq ByteArray where
  beq a b := a.data == b.data

#guard decode' decInt64 (.mk #[1,2,3,4,5,6,7]) == none
#guard decode' decInt64 (.mk #[0,1,0,0,0,0,0,0]) == some 256
#guard decode' decInt64 (encInt64 0) == some 0
#guard decode' decInt64 (encInt64 1) == some 1
#guard decode' decInt64 (encInt64 $ -1) == some (-1)
#guard decode' decInt64 (encInt64 256) == some 256

#guard encInt 1    == .mk #[1, 0, 0, 0, 0, 0, 0, 0]
#guard encInt 0    == .mk #[0, 0, 0, 0, 0, 0, 0, 0]
#guard encInt (-1) == .mk #[255, 255, 255, 255, 255, 255, 255, 255]
#guard encInt (-2) == .mk #[254, 255, 255, 255, 255, 255, 255, 255]

#guard decode' decInt (encInt 1) == some 1
#guard decode' decInt (encInt 0) == some 0
#guard decode' decInt (encInt $ -1) == some (-1)

#guard decode' decFloat (encFloat 0) == some 0
#guard decode' decFloat (encFloat 0.5) == some 0.5
#guard decode' decFloat (encFloat 1.0) == some 1.0
#guard decode' decFloat (encFloat 1.5) == some 1.5
#guard decode' decFloat (encFloat (-0)) == some (0.0)
#guard decode' decFloat (encFloat (-0.5)) == some (-0.5)
#guard decode' decFloat (encFloat (-1.0)) == some (-1.0)
#guard decode' decFloat (encFloat (-1.5)) == some (-1.5)

------------------------------------------------------------------------------
-- Strings are encoded as a length followed by a sequence of UTF8 bytes
-- TODO: what if string length is > 2^63?

private def encString (s : String) : ByteArray :=
  let ba := s.toUTF8
  (encInt64 ba.size.toInt64).append ba

private def decString : DecodeM String := do
  let len := (<- decInt64).toNat
  let ba <- take len
  match String.fromUTF8? ba with
  | none => throw "invalid UTF8 string"
  | some s => return s

#guard decode' decString (encString "") = some ""
#guard decode' decString (encString "Hello World") = some "Hello World"
#guard decode' decString (encString "Hello • World") = some "Hello • World"

------------------------------------------------------------------------------
-- Lists are encoded as a length followed by a sequence of encoded values
-- TODO: not efficient

private def encList (f : a -> ByteArray) (l : List a) : ByteArray :=
  let rec mapa : List a -> ByteArray
    | .nil => .mk #[]
    | .cons x l => (f x).append (mapa l)
  (encInt64 l.length.toInt64).append (mapa l)

private def decList (f : DecodeM a) : DecodeM (List a) := do
  let rec gena : Nat -> DecodeM (List a)
    | .zero => return []
    | .succ n => return (<- f) :: (<- gena n)
  gena (<- decInt64).toNat

-- TODO: there are no types in serialized format, do we need that?
#guard decode' (decList decInt) (encList encFloat []) = some []
#guard decode' (decList decInt) (encList encInt [1,2,3]) = some [1,2,3]

------------------------------------------------------------------------------
-- Finally, constants are encoded with a tag followed by the values

private def tag (t : UInt8) : List ByteArray -> ByteArray :=
  List.foldl ByteArray.append (.mk #[t])

def encConst : Const -> ByteArray
  | .nil        => tag 0x00 []
  | .bool false => tag 0x01 []
  | .bool true  => tag 0x02 []
  | .int i      => tag 0x03 [encInt i]
  | .float f    => tag 0x04 [encFloat f]
  | .string s   => tag 0x05 [encString s]

def decConst : DecodeM Const := do
  let val <- next
  match val with
  | 0x00 => return .nil
  | 0x01 => return .bool false
  | 0x02 => return .bool true
  | 0x03 => return .int (<- decInt)
  | 0x04 => return .float (<- decFloat)
  | 0x05 => return .string (<- decString)
  | _ => throw s!"Unknown Const tag value {val}"

private def chkConst (c: Const) : Bool :=
  (decode' decConst $ encConst c) == some c

#guard chkConst .nil
#guard chkConst (.bool true)
#guard chkConst (.bool false)
#guard chkConst (.int 1)
#guard chkConst (.float 1.0)
#guard chkConst (.string "str")

------------------------------------------------------------------------------
-- Expressions

mutual
partial def encExpr : Expr -> ByteArray
  | .value c          => tag 0x10 [encConst c]
  | .bvar s           => tag 0x11 [encString s]
  | .var s _          => tag 0x12 [encString s]
  | .subscript e ix   => tag 0x13 [encExpr e, encList encIndex ix]
  | .binop op l r     => tag 0x14 [encString op, encExpr l, encExpr r]
  | .cond c t e       => tag 0x15 [encExpr c, encExpr t, encExpr e]
  | .tuple es         => tag 0x16 [encList encExpr es]
  | .list es          => tag 0x17 [encList encExpr es]
  | .call f ax        => tag 0x18 [encExpr f, encList encExpr ax]
  | .gridcall f ix ax => tag 0x19 [encExpr f, encList encIndex ix, encList encExpr ax]

partial def encIndex : Index -> ByteArray
  | .coord i        => tag 0x20 [encExpr i]
  | .slice l u step => tag 0x21 [encExpr l, encExpr u, encExpr step]
  | .dots           => tag 0x22 []
end

mutual
partial def decExpr : DecodeM Expr := do
  match (<- next) with
  | 0x10 => return .value (<- decConst)
  | 0x11 => return .bvar (<- decString)
  | 0x12 => return .var (<- decString) ""
  | 0x13 => return .subscript (<-decExpr) (<- decList decIndex)
  | 0x14 => return .binop (<- decString) (<- decExpr) (<- decExpr)
  | 0x15 => return .cond (<- decExpr) (<- decExpr) (<- decExpr)
  | 0x16 => return .tuple (<- decList decExpr)
  | 0x17 => return .list (<- decList decExpr)
  | 0x18 => return .call (<- decExpr) (<- decList decExpr)
  | 0x19 => return .gridcall (<- decExpr) (<- decList decIndex) (<- decList decExpr)
  | t => throw s!"Unknown tag in Expr {t}"

partial def decIndex : DecodeM Index := do
  match (<- next) with
  | 0x20 => return .coord (<- decExpr)
  | 0x21 => return .slice (<- decExpr) (<- decExpr) (<- decExpr)
  | 0x22 => return .dots
  | t => throw s!"Unknown tag in Index {t}"
end

private def chkExpr (e : Expr) : Bool :=
  (decode' decExpr $ encExpr e) == some e
private def chkIndex (i : Index) : Bool :=
  (decode' decIndex $ encIndex i) == some i

private def nil := Expr.value .nil
private def ndx := Index.coord nil

#guard chkExpr nil
#guard chkExpr (.bvar "var")
#guard chkExpr (.var "var" "")
#guard chkExpr (.subscript nil [ndx, ndx, ndx])
#guard chkExpr (.binop "op" nil nil)
#guard chkExpr (.cond nil nil nil)
#guard chkExpr (.tuple [nil, nil, nil])
#guard chkExpr (.list [nil, nil, nil])
#guard chkExpr (.call nil [nil, nil, nil])
#guard chkExpr (.gridcall nil [ndx, ndx, ndx] [nil, nil, nil])

#guard chkIndex ndx
#guard chkIndex (.slice nil nil nil)
#guard chkIndex .dots

------------------------------------------------------------------------------
-- Statements

partial def encStmt : Stmt -> ByteArray
  | .ret e         => tag 0x30 [encExpr e]
  | .assign x e    => tag 0x31 [encExpr x, encExpr e]
  | .ifstm c t e   => tag 0x32 [encExpr c, encList encStmt t, encList encStmt e]
  | .forloop x e b => tag 0x33 [encString x, encExpr e, encList encStmt b]
  | .check e       => tag 0x34 [encExpr e]

partial def decStmt : DecodeM Stmt := do
  match (<- next) with
  | 0x30 => return .ret (<- decExpr)
  | 0x31 => return .assign (<- decExpr) (<- decExpr)
  | 0x32 => return .ifstm (<- decExpr) (<- decList decStmt) (<- decList decStmt)
  | 0x33 => return .forloop (<- decString) (<- decExpr) (<- decList decStmt)
  | 0x34 => return .check (<- decExpr)
  | t => throw s!"Unknown tag in Stmt {t}"

private def chkStmt (s : Stmt) : Bool :=
  (decode' decStmt $ encStmt s) == some s

private def stm := Stmt.check nil

#guard chkStmt (.ret nil)
#guard chkStmt (.assign nil nil)
#guard chkStmt (.ifstm nil [stm, stm, stm] [stm, stm, stm])
#guard chkStmt (.forloop "x" nil [stm, stm, stm])
#guard chkStmt (.check nil)
