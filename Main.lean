import KLR
import Cli
import KLR.Python
import KLR.Trace
open Cli


local instance : MonadLift Err IO where
  monadLift
    | .ok x => return x
    | .error s => throw $ .userError s

def parseJson (p : Parsed) : IO UInt32 := do
  let file := p.positionalArg! "file" |>.as! String
  let s <- IO.FS.readFile file
  let kernel <- KLR.Python.Parsing.parse s
  let stmts <- KLR.Trace.runNKIKernel kernel
  let json := Lean.Json.arr (stmts.map Lean.toJson).toArray
  IO.println json
  return 0

def parseJsonCmd := `[Cli|
  "parse-json" VIA parseJson;
  "Parse KLR kernels as JSON"

  ARGS:
    file : String;      "File of Python AST printed as JSON"
]

def klrCmd : Cmd := `[Cli|
  klr NOOP; ["0.0.1"]
  "KLR is an IR for NKI and other tensor-like languages in Lean."

  SUBCOMMANDS:
    parseJsonCmd
]

def main (args : List String) : IO UInt32 :=
  if args.isEmpty then do
    IO.println klrCmd.help
    return 0
  else do
   klrCmd.validate args
