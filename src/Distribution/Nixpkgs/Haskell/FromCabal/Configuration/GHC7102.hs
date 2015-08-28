{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Configuration.GHC7102
  ( Configuration(..), ghc7102
  ) where

import qualified Data.Set as Set
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration.Maintainers

ghc7102 :: Configuration
ghc7102 = Configuration
  { platform = Platform X86_64 Linux

  , compilerInfo = unknownCompilerInfo (CompilerId GHC (Version [7,10,2] [])) NoAbiTag

  , packageMaintainers = globalPackageMaintainers

  , defaultPackageOverrides =
    [ "HUnit < 1.3.0.0"         -- newer versions break test-framework-hunit-0.3.0.1
    ]

  , corePackages =
    [ "Cabal-1.22.4.0"
    , "array-0.5.1.0"
    , "base-4.8.1.0"
    , "binary-0.7.5.0"
    , "bytestring-0.10.6.0"
    , "containers-0.5.6.2"
    , "deepseq-1.4.1.1"
    , "directory-1.2.2.0"
    , "filepath-1.4.0.0"
    , "ghc-prim-0.4.0.0"
    , "haskeline-0.7.2.1"
    , "hoopl-3.10.0.2"
    , "hpc-0.6.0.2"
    , "integer-gmp-1.0.0.0"
    , "pretty-1.1.2.0"
    , "process-1.2.3.0"
    , "template-haskell-2.10.0.0"
    , "terminfo-0.4.0.1"
    , "time-1.5.0.1"
    , "transformers-0.4.2.0"
    , "unix-2.7.1.0"
    , "xhtml-3000.2.1"
    ]

  , hardCorePackages =
    [ "bin-package-db-0.0.0.0"
    , "ghc-7.10.2"
    , "rts-1.0"
    ]

  , extraPackages =
    [ "aeson < 0.8"                     -- newer versions don't work with GHC 6.12.3
    , "Cabal == 1.18.*"                 -- required for cabal-install et al on old GHC versions
    , "Cabal == 1.20.*"                 -- required for cabal-install et al on old GHC versions
    , "cabal-install == 1.18.*"         -- required for ghc-mod on 7.8.x
    , "conduit < 1.2.4.2"               -- newer versions trigger non-deterministic ID bugs in GHC versions prior to 7.10.x
    , "containers < 0.5"                -- required to build alex with GHC 6.12.3
    , "control-monad-free < 0.6"        -- newer versions don't compile with anything but GHC 7.8.x
    , "deepseq == 1.3.0.1"              -- required to build Cabal with GHC 6.12.3
    , "descriptive < 0.1"               -- required for structured-haskell-mode-1.0.8
    , "gloss < 1.9.3"                   -- new versions don't compile with GHC 7.8.x
    , "haddock-api < 2.16"              -- required on GHC 7.8.x
    , "haskell-src-exts < 1.16"         -- required for structured-haskell-mode-1.0.8
    , "mtl < 2.2"                       -- newer versions require transformers > 0.4.x, which we cannot provide in GHC 7.8.x
    , "mtl-prelude < 2"                 -- required for to build postgrest on mtl 2.1.x platforms
    , "parallel == 3.2.0.3"             -- newer versions don't work with GHC 6.12.3
    , "primitive == 0.5.1.*"            -- required to build alex with GHC 6.12.3
    , "QuickCheck < 2"                  -- required by test-framework-quickcheck and its users
    , "seqid < 0.2"                     -- newer versions depend on transformers 0.4.x which we cannot provide in GHC 7.8.x
    , "seqid-streams < 0.2"             -- newer versions depend on transformers 0.4.x which we cannot provide in GHC 7.8.x
    , "split < 0.2"                     -- newer versions don't work with GHC 6.12.3
    , "tar < 0.4.2.0"                   -- later versions don't work with GHC < 7.6.x
    , "vector < 0.10.10"                -- newer versions don't work with GHC 6.12.3
    , "zlib < 0.6"                      -- newer versions break cabal-install
    ]

  , dontDistributePackages = Set.fromList
    [
    ]

  }
