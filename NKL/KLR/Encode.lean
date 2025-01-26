/-
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
-/
import NKL.Util
import NKL.KLR.Basic

/-!
# Serialization and Deserialization

-/

namespace NKL.KLR

-- All of the encode function are pure; decoding uses an instance of StM.

abbrev DecodeM := StM ByteArray.Iterator

def decode' (f : DecodeM a) (ba : ByteArray) : Option a :=
  f.run' ba.iter

def decode (f : DecodeM a) (ba : ByteArray) : Err a :=
  match f.run ba.iter with
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

private def encBV64 (bv : BitVec 64) : ByteArray :=
  let n := bv.toFin.val
  .mk #[ UInt8.ofNat $ n >>> 0
       , UInt8.ofNat $ n >>> 8
       , UInt8.ofNat $ n >>> 16
       , UInt8.ofNat $ n >>> 24
       , UInt8.ofNat $ n >>> 32
       , UInt8.ofNat $ n >>> 40
       , UInt8.ofNat $ n >>> 48
       , UInt8.ofNat $ n >>> 56
       ]

private def decBV64 : DecodeM (BitVec 64) :=
  let u8_64 : DecodeM UInt64 := next >>= fun x => return x.toUInt64
  return ((<- u8_64) <<< 0  |||
          (<- u8_64) <<< 8  |||
          (<- u8_64) <<< 16 |||
          (<- u8_64) <<< 24 |||
          (<- u8_64) <<< 32 |||
          (<- u8_64) <<< 40 |||
          (<- u8_64) <<< 48 |||
          (<- u8_64) <<< 56).toBitVec

-- Int64 isn't supported by ToJson/FromJson deriving
-- For now, keep using Int in AST
-- TODO: switch to Int64 in AST
private def encInt (i : Int) : ByteArray := encBV64 (.ofInt 64 i)
private def decInt : DecodeM Int := return (<- decBV64).toInt

private def encFloat (f : Float) : ByteArray := encBV64 f.toBits.toBitVec
private def decFloat : DecodeM Float := return Float.ofBits ⟨ (<- decBV64) ⟩

local instance : BEq ByteArray where
  beq a b := a.data == b.data

#guard decode' decBV64 (.mk #[1,2,3,4,5,6,7]) == none
#guard decode' decBV64 (.mk #[0,1,0,0,0,0,0,0]) == some 256
#guard decode' decBV64 (encBV64 0) == some 0
#guard decode' decBV64 (encBV64 1) == some 1
#guard decode' decBV64 (encBV64 $ -1) == some (-1)
#guard decode' decBV64 (encBV64 256) == some 256

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
  (encInt ba.size).append ba

private def decString : DecodeM String := do
  let len := (<- decInt).toNat
  let ba <- take len
  match String.fromUTF8? ba with
  | none => throw "invalid UTF8 string"
  | some s => return s

#guard decode' decString (encString "") = some ""
#guard decode' decString (encString "Hello World") = some "Hello World"
#guard decode' decString (encString "Hello • World") = some "Hello • World"

------------------------------------------------------------------------------
-- Lists are encoded as a length followed by a sequence of encoded values

private def encList (f : a -> ByteArray) (l : List a) : ByteArray :=
  let rec mapa : List a -> ByteArray
    | .nil => .mk #[]
    | .cons x l => (f x).append (mapa l)
  (encInt l.length).append (mapa l)

private def decList (f : DecodeM a) : DecodeM (List a) := do
  let rec gena : Nat -> DecodeM (List a)
    | .zero => return []
    | .succ n => return (<- f) :: (<- gena n)
  gena (<- decInt).toNat

-- TODO: there are no types in serialized format, do we need that?
#guard decode' (decList decInt) (encList encFloat []) = some []
#guard decode' (decList decInt) (encList encInt [1,2,3]) = some [1,2,3]

------------------------------------------------------------------------------
-- Options are encoded using a tag followed by the encoded value

private def tag (t : UInt8) : List ByteArray -> ByteArray :=
  List.foldl ByteArray.append (.mk #[t])

private def encOption (f : a -> ByteArray) : Option a -> ByteArray
  | .none   => tag 0 []
  | .some x => tag 1 [f x]

private def decOption (f : DecodeM a) : DecodeM (Option a) := do
  match (<- next) with
  | 0 => return .none
  | 1 => f
  | t => throw s!"invalid option tag {t}"

#guard decode' (decOption decInt) (encOption encInt none) = some none
#guard decode' (decOption decInt) (encOption encInt $ some 1) = some (some 1)

------------------------------------------------------------------------------
-- Constants are encoded with a tag followed by the values

def encConst : Const -> ByteArray
  | .none       => tag 0x00 []
  | .bool false => tag 0x01 []
  | .bool true  => tag 0x02 []
  | .int i      => tag 0x03 [encInt i]
  | .float f    => tag 0x04 [encFloat f]
  | .string s   => tag 0x05 [encString s]

def decConst : DecodeM Const := do
  let val <- next
  match val with
  | 0x00 => return .none
  | 0x01 => return .bool false
  | 0x02 => return .bool true
  | 0x03 => return .int (<- decInt)
  | 0x04 => return .float (<- decFloat)
  | 0x05 => return .string (<- decString)
  | _ => throw s!"Unknown Const tag value {val}"

private def chkConst (c: Const) : Bool :=
  (decode' decConst $ encConst c) == some c

#guard chkConst .none
#guard chkConst (.bool true)
#guard chkConst (.bool false)
#guard chkConst (.int 1)
#guard chkConst (.float 1.0)
#guard chkConst (.string "str")

------------------------------------------------------------------------------
-- Affine Expressions

def encIndexExpr : IndexExpr -> ByteArray
  | .var name  => tag 0x10 [encString name]
  | .int i     => tag 0x11 [encInt i]
  | .neg e     => tag 0x12 [encIndexExpr e]
  | .add l r   => tag 0x13 [encIndexExpr l, encIndexExpr r]
  | .mul i e   => tag 0x14 [encInt i, encIndexExpr e]
  | .floor e i => tag 0x15 [encIndexExpr e, encInt i]
  | .ceil e i  => tag 0x16 [encIndexExpr e, encInt i]
  | .mod e i   => tag 0x17 [encIndexExpr e, encInt i]

partial def decIndexExpr : DecodeM IndexExpr := do
  match (<- next) with
  | 0x10 => return .var (<- decString)
  | 0x11 => return .int (<- decInt)
  | 0x12 => return .neg (<- decIndexExpr)
  | 0x13 => return .add (<- decIndexExpr) (<- decIndexExpr)
  | 0x14 => return .mul (<- decInt) (<- decIndexExpr)
  | 0x15 => return .floor (<- decIndexExpr) (<- decInt)
  | 0x16 => return .ceil (<- decIndexExpr) (<- decInt)
  | 0x17 => return .mod (<- decIndexExpr) (<- decInt)
  | t    => throw s!"Unknown tag in IndexExpr {t}"

private def chkIE (e: IndexExpr) : Bool :=
  (decode' decIndexExpr $ encIndexExpr e) == some e

private def ie_var : IndexExpr := .var "s"

#guard chkIE (.var "v")
#guard chkIE (.int 1)
#guard chkIE (.neg ie_var)
#guard chkIE (.add ie_var ie_var)
#guard chkIE (.mul 2 ie_var)
#guard chkIE (.floor ie_var 2)
#guard chkIE (.ceil ie_var 2)
#guard chkIE (.mod ie_var 2)

def encIndex : Index -> ByteArray
  | .ellipsis    => tag 0x20 []
  | .coord e     => tag 0x21 [enc e]
  | .slice l u s => tag 0x22 [enc l, enc u, enc s]
where
  enc := encOption encIndexExpr

def decIndex : DecodeM Index := do
  match (<- next) with
  | 0x20 => return .ellipsis
  | 0x21 => return .coord (<- dec)
  | 0x22 => return .slice (<- dec) (<- dec) (<- dec)
  | t    => throw s!"Unknown tag in Index {t}"
where
  dec:= decOption decIndexExpr

private def chkIndex (i : Index) : Bool :=
  (decode' decIndex $ encIndex i) == some i

#guard chkIndex .ellipsis
#guard chkIndex (.coord none)
#guard chkIndex (.coord $ some ie_var)
#guard chkIndex (.slice (some ie_var) none none)

------------------------------------------------------------------------------
-- Expressions

partial def encExpr : Expr -> ByteArray
  | .var s        => tag 0x30 [encString s]
  | .tensor t     => tag 0x31 [encString t.name, encString t.dtype, encList encInt t.shape]
  | .const c      => tag 0x32 [encConst c]
  | .access e ix  => tag 0x33 [encExpr e, encList encIndex ix]
  | .call f ax kw => tag 0x34 [encExpr f, encList encExpr ax, encList encKeyword kw]
where
  encKeyword : String × Expr -> ByteArray
  | (key, expr) => (encString key).append (encExpr expr)

partial def decExpr : DecodeM Expr := do
  match (<- next) with
  | 0x30 => return .var (<- decString)
  | 0x31 => return .tensor $ .mk (<- decString) (<- decString) (<- decList decInt)
  | 0x32 => return .const (<- decConst)
  | 0x33 => return .access (<- decExpr) (<- decList decIndex)
  | 0x34 => return .call (<- decExpr) (<- decList decExpr) (<- decList decKeyword)
  | t => throw s!"Unknown tag in Expr {t}"
where
  decKeyword : DecodeM (String × Expr) :=
    return ((<- decString), (<- decExpr))

private def chkExpr (e : Expr) : Bool :=
  (decode' decExpr $ encExpr e) == some e

private def nil := Expr.const .none
private def ixz := Index.coord (IndexExpr.int 0)

#guard chkExpr nil
#guard chkExpr (.var "var")
#guard chkExpr (.tensor $ .mk "t" "float32" [1,2,3])
#guard chkExpr (.const (.int 1))
#guard chkExpr (.access nil [ixz, ixz, ixz])
#guard chkExpr (.call nil [nil, nil, nil] [("a", nil), ("b", nil)])

------------------------------------------------------------------------------
-- Statements

partial def encStmt : Stmt -> ByteArray
  | .pass          => tag 0x40 []
  | .expr e        => tag 0x41 [encExpr e]
  | .ret e         => tag 0x42 [encExpr e]
  | .assign x e    => tag 0x43 [encString x, encExpr e]
  | .loop x l u step body =>
      tag 0x44 [ encString x,
                 encIndexExpr l, encIndexExpr u, encIndexExpr step,
                 encList encStmt body ]

partial def decStmt : DecodeM Stmt := do
  match (<- next) with
  | 0x40 => return .pass
  | 0x41 => return .expr (<- decExpr)
  | 0x42 => return .ret (<- decExpr)
  | 0x43 => return .assign (<- decString) (<- decExpr)
  | 0x44 => do
      let x <- decString
      let l <- decIndexExpr
      let u <- decIndexExpr
      let step <- decIndexExpr
      let body <- decList decStmt
      return .loop x l u step body
  | t => throw s!"Unknown tag in Stmt {t}"

private def chkStmt (s : Stmt) : Bool :=
  (decode' decStmt $ encStmt s) == some s

#guard chkStmt .pass
#guard chkStmt (.expr nil)
#guard chkStmt (.ret nil)
#guard chkStmt (.assign "x" nil)
#guard chkStmt (.loop "x" ie_var ie_var ie_var [.pass, .pass])
