import Lake
open Lake DSL

package "NKL" where

lean_lib "NKL" where
  defaultFacets := #[LeanLib.staticFacet]

@[default_target]
lean_exe "nkl" where
  root := `Main

script greet (args) do
  IO.println s!"Hello {args}"
  return 0



require Cli from git
  "https://github.com/leanprover/lean4-cli.git" @ "v2.2.0-lv4.14.0-rc1"
