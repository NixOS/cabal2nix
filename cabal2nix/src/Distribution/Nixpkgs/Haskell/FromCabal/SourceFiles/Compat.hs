{-# LANGUAGE CPP #-}

module Distribution.Nixpkgs.Haskell.FromCabal.SourceFiles.Compat
  ( module Distribution.Nixpkgs.Haskell.FromCabal.SourceFiles.Compat,
  )
where

#if MIN_VERSION_Cabal(3,6,0)

import Distribution.Utils.Path as Distribution.Nixpkgs.Haskell.FromCabal.SourceFiles.Compat (getSymbolicPath)

#endif

#if MIN_VERSION_Cabal(3,14,0)

import Distribution.Utils.Path (SymbolicPathX)

#endif

#if MIN_VERSION_Cabal(3,14,0)

getSymbolicPath_314 :: SymbolicPathX a b c -> FilePath
getSymbolicPath_314 = getSymbolicPath

#else

extraFiles :: [FilePath]
extraFiles = []

getSymbolicPath_314 :: FilePath -> FilePath
getSymbolicPath_314 = id

#if !MIN_VERSION_Cabal(3,6,0)

getSymbolicPath :: FilePath -> FilePath
getSymbolicPath = id

#endif

#endif
