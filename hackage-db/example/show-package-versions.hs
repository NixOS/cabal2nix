{-# LANGUAGE CPP #-}

#if !MIN_VERSION_Cabal(2,0,0)
#  define mkPackageName PackageName
#endif

module Main ( main ) where

import Distribution.Hackage.DB

import Control.Monad ( forM_ )
import Data.Map as Map
import Distribution.Package
import Distribution.Text ( display )
import System.Environment ( getArgs )

main :: IO ()
main = do
  db <- hackageTarball >>= readTarball Nothing
  pkgs <- getArgs
  forM_ pkgs $ \pkg -> do
    let vs = maybe [] Map.keys (Map.lookup (mkPackageName pkg) db)
    putStrLn $ pkg ++ ": " ++ unwords (fmap display vs)
