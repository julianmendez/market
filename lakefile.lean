import Lake
open Lake DSL

package «soda» where
  -- add package configuration options here

lean_lib «Soda» where
  -- add library configuration options here

@[default_target]
lean_exe «market» where
  root := `Soda.se.umu.cs.soda.prototype.example.market.main.Main

