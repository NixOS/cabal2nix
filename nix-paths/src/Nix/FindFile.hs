{-# LANGUAGE CPP #-}

module Nix.FindFile ( findFile, findFileWithDefault ) where

import Nix.Paths ( nixInstantiate )

import Data.Maybe
import System.Exit
import System.Process

-- | Use @nix-instantiate --find-file@ to resolve an abbreviated path into a
-- full absolute path using @$NIX_PATH@.
--
-- >>> findFile "nixpkgs/pkgs/development/haskell-modules/configuration-hackage2nix.yaml"
-- Just "/home/foobar/.nix-defexpr/pkgs/development/haskell-modules/configuration-hackage2nix.yaml"
--
-- >>> findFile "nixpkgs/crazy/non-existent-path"
-- Nothing

findFile :: FilePath -> IO (Maybe FilePath)
findFile path = do
   let cmd = proc nixInstantiate ["--find-file", path ]
   (ec, out, _) <- readCreateProcessWithExitCode cmd ""
   case (ec, lines out) of
      (ExitFailure _, _)     -> return Nothing
      (ExitSuccess, [path']) -> return (Just path')
      _                      -> fail "unexpected response from nix-instantiate"

-- | A convenience wrapper around 'findFile' that returns the given default
-- value in case the function returns nothing.
--
-- @
-- findFileWithDefault def = 'fmap' ('fromMaybe' def) . 'findFile'
-- @

findFileWithDefault :: FilePath -> FilePath -> IO FilePath
findFileWithDefault def = fmap (fromMaybe def) . findFile
