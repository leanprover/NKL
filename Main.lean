import NKL

def main (args : List String) : IO Unit :=
  match args with
  | .nil => IO.println s!"Hello, NKL!"
  | .cons x _ => do
    let s <- IO.FS.readFile x
    NKL.parse_json s
