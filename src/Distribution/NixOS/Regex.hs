{- |
   Module      :  Distribution.NixOS.Derivation.Cabal
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   A represtation of Nix expressions based on Cabal builder defined in
   @pkgs\/development\/libraries\/haskell\/cabal\/cabal.nix@.
-}

module Distribution.NixOS.Regex
  ( module Text.Regex.Posix
  , regsubmatch
  )
  where

import Text.Regex.Posix

regsubmatch :: String -> String -> [String]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (String,String,String,[String])
        f = match (makeRegexOpts compExtended execBlank patt) buf
