import NKL

def main : List String -> IO Unit
  | [ file ] => IO.FS.readFile file >>= NKL.parse_json
  | _ => IO.println "invalid arguments"
