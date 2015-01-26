module Distribution.Nixpkgs.Util.Regex
  ( module Text.Regex.Posix
  , regsubmatch
  )
  where

import Text.Regex.Posix

regsubmatch :: String -> String -> [String]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (String,String,String,[String])
        f = match (makeRegexOpts compExtended execBlank patt) buf
