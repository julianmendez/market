import Lake
open Lake DSL

package «market» where
  -- add package configuration options here

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git" @ "v4.33.0"

lean_lib «Soda» where
  -- add library configuration options here

@[default_target]
lean_exe «market» where
  root := `Soda.se.umu.cs.soda.prototype.example.market.main.Main

