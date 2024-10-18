import Lake
open Lake DSL

package "NKL" where

@[default_target]
lean_exe "nkl" where
  root := `Main
