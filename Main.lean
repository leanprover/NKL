import NKL
import Cli
import NKL.Python
open Cli

def parseJson (p : Parsed) : IO UInt32 := do
  let file := p.positionalArg! "file" |>.as! String
  let s <- IO.FS.readFile file
  let _ <- NKL.parse_json s
  return 0

def parseJsonCmd := `[Cli|
  "parse-json" VIA parseJson;
  "Parse NKI kernels as JSON"

  ARGS:
    file : String;      "shape to test"
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
