import KLR
import Cli
import KLR.Python
open Cli

local instance : MonadLift Err IO where
  monadLift
    | .ok x => return x
    | .error s => throw $ .userError s

private def parseJsonString (s : String) : IO Unit := do
  let kernel <- KLR.Python.Parsing.parse s
  let stmts <- KLR.Trace.runNKIKernel kernel
  for s in stmts do
    IO.println ("  " ++ repr s)

def parseJsonFile (p : Parsed) : IO UInt32 := do
  let file := p.positionalArg! "file" |>.as! String
  let s <- IO.FS.readFile file
  let _ <- parseJsonString s
  return 0

def parseJsonCmd := `[Cli|
  "parse-json" VIA parseJsonFile;
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
