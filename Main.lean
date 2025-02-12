import KLR
import Cli
open Cli

local instance : MonadLift Err IO where
  monadLift
    | .ok x => return x
    | .error s => throw $ .userError s

private def parse (p : Parsed) : IO KLR.Python.Kernel := do
  let file := p.positionalArg! "file" |>.as! String
  let s <- IO.FS.readFile file
  KLR.Python.Parsing.parse s

def parseJson (p : Parsed) : IO UInt32 := do
  let kernel <- parse p
  let klr <- KLR.Trace.runNKIKernel kernel.inferArguments
  let json := Lean.toJson klr
  IO.println json
  return 0

def trace (p : Parsed) : IO UInt32 := do
  let kernel <- parse p
  let klr <- KLR.Trace.runNKIKernel kernel.inferArguments
  if p.hasFlag "repr" then
    IO.println (toString $ repr klr)
  else if p.hasFlag "json" then
    IO.println (toString $ Lean.toJson klr)
  else
    IO.println (Lean.format klr).pretty
  return 0

def parseJsonCmd := `[Cli|
  "parse-json" VIA parseJson;
  "Parse KLR kernels as JSON"

  ARGS:
    file : String;      "File of Python AST printed as JSON"
]

def traceCmd := `[Cli|
  "trace" VIA trace;
  "Trace Python to KLR"

  FLAGS:
    r, repr; "Output Repr format"
    j, json; "Output Json format"
  ARGS:
    file : String; "File of Python AST printed as JSON"
]

def klrCmd : Cmd := `[Cli|
  klr NOOP; ["0.0.1"]
  "KLR is an IR for NKI and other tensor-like languages in Lean."

  SUBCOMMANDS:
    parseJsonCmd;
    traceCmd
]

def main (args : List String) : IO UInt32 :=
  if args.isEmpty then do
    IO.println klrCmd.help
    return 0
  else do
    klrCmd.validate args
