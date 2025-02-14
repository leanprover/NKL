import KLR
import Cli
open Cli

local instance : MonadLift Err IO where
  monadLift
    | .ok x => return x
    | .error s => throw $ .userError s

def gather (p : Parsed) : IO UInt32 := do
  let kernel := p.positionalArg! "kernel" |>.as! String
  let path1 <- IO.appPath
  let path2 := (<- IO.currentDir).join "bin"
  for p in [path1, path2] do
    let exe := (p.join "gather")
    if <- exe.pathExists then
      try do
        let output <- IO.Process.run {
          cmd := exe.toString
          args := #[ kernel ]
        }
        IO.println output
        return 0
      catch _ =>
        IO.println s!"gather failed: please make sure {kernel} is in your PYTHONPATH"
        return 1
  IO.println s!"could not find gather program"
  return 2

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

def parseBIR (p : Parsed) : IO UInt32 := do
  let file := p.positionalArg! "file" |>.as! String
  let str <- IO.FS.readFile file
  let json <- Lean.Json.parse str
  let bir : KLR.BIR.BIR <- Lean.fromJson? json
  if p.hasFlag "repr" then
    IO.println s!"{repr bir}"
  else
    IO.println s!"{Lean.toJson bir}"
  return 0

def compile (p : Parsed) : IO UInt32 := do
  let file := p.positionalArg! "file" |>.as! String
  let s <- IO.FS.readFile file
  let kernel <- KLR.Python.Parsing.parse s
  let klr <- KLR.Trace.runNKIKernel kernel.inferArguments
  let bir <- KLR.BIR.compile klr
  if p.hasFlag "repr" then
    IO.println (toString $ repr bir)
  else
    IO.println (toString $ Lean.toJson bir)
  return 0

-- Command configuration

def gatherCmd := `[Cli|
  "gather" VIA gather;
  "Gather Python sources into a JSON file"

  ARGS:
    kernel : String; "Full path of kernel function"
]

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

def parseBIRCmd := `[Cli|
  "parse-bir" VIA parseBIR;
  "Parse a BIR Json file"

  FLAGS:
    r, repr; "Output Repr format"
  ARGS:
    file : String; "File of BIR JSON"
]

def compileCmd := `[Cli|
  "compile" VIA compile;
  "Compile Python to BIR"

  FLAGS:
    r, repr; "Output Repr format"
  ARGS:
    file : String; "File of Python AST printed as JSON"
]

def klrCmd : Cmd := `[Cli|
  klr NOOP; ["0.0.7"]
  "KLR is an IR for NKI and other tensor-like languages in Lean."

  SUBCOMMANDS:
    compileCmd;
    gatherCmd;
    parseBIRCmd;
    parseJsonCmd;
    traceCmd
]

def main (args : List String) : IO UInt32 :=
  if args.isEmpty then do
    IO.println klrCmd.help
    return 0
  else do
    klrCmd.validate args
