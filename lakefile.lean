import Lake
open Lake DSL

package "NKL" where

lean_lib "NKL" where
  defaultFacets := #[LeanLib.staticFacet]

lean_lib "Export" where

@[default_target]
lean_exe "nkl" where
  root := `Main
