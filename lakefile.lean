import Lake
open Lake DSL

package "KLR" where

lean_lib "KLR" where
  defaultFacets := #[LeanLib.staticFacet]

--lean_lib "Export" where

@[default_target]
lean_exe "klr" where
  root := `Main
