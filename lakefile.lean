import Lake
open Lake DSL

package "ANPU" where
  -- add package configuration options here

lean_lib «ANPU» where
  -- add library configuration options here

@[default_target]
lean_exe "anpu" where
  root := `Main
