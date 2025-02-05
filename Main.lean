import KLR

def main : List String -> IO Unit
  | [ file ] => IO.FS.readFile file >>= KLR.parse_json
  | _ => IO.println "invalid arguments"
