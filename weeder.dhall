-- weeder.dhall

{ roots = [ "^Main.main$"
          , "^Paths_cabal2nix_v3\\..*"
          ]
, type-class-roots = True
}
