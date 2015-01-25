import Test.DocTest

main :: IO ()
main = do
  doctest ["-idist/build/autogen", "-isrc", "src/cabal2nix.hs"]
  doctest ["-isrc", "src/hackage2nix.hs"]
