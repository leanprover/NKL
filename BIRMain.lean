import Lean
import KLR.Util
import KLR.BIR

instance : MonadLift Err IO where
  monadLift
    | .ok x => return x
    | .error s => throw $ .userError s

def do_main (file: String) : IO Unit := do
  let str <- IO.FS.readFile file
  let json <- Lean.Json.parse str
  let bir : KLR.BIR.BIR <- Lean.fromJson? json
  IO.println s!"{repr bir}"

def main : List String -> IO Unit
  | [ file ] => do_main file
  | _ => IO.println "invalid arguments"
