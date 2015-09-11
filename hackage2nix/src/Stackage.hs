{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stackage where

import Control.DeepSeq.Generics
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans
import Data.Map as Map
import Data.Maybe
import Data.Yaml ( decodeFile )
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import GHC.Generics ( Generic )
import Stackage.Types hiding ( display )
import System.Directory
import System.FilePath
import Text.Show.Pretty
import Distribution.Compiler

yo :: IO ()
yo = readView "/home/simons/src/cabal2nix/hackage2nix/stackage-nightly/nightly-2015-09-11.yaml" >>= putStrLn . ppShow

data Spec = Spec
            { version :: Version
            , flagOverrides :: (Map FlagName Bool)
            , runTests :: Bool
            , runHaddock :: Bool
            , runLinuxBuilds :: Bool
            }
  deriving (Show, Eq, Ord, Generic)

data View = View
            { compiler :: CompilerId
            , corePackages :: Map PackageName Version
            , packages :: Map PackageName Spec
            }
  deriving (Show, Generic)

instance NFData Spec where
instance NFData View where

views :: IO [View]
views = runParIO $ readStackage "lts-haskell"

readStackage :: FilePath -> ParIO [View]
readStackage dirPath = do
  filePaths <- liftIO (listFiles dirPath)
  parMapM (liftIO . readView) [ dirPath </> p | p <- filePaths, takeExtension p == ".yaml" ]

readView :: FilePath -> IO View
readView = fmap fromBuildPlan . readBuildPlan

fromBuildPlan :: BuildPlan -> View
fromBuildPlan bp = View (CompilerId GHC (siGhcVersion (bpSystemInfo bp)))
                        (siCorePackages (bpSystemInfo bp))
                        (Map.map fromPackagePlan (bpPackages bp))

fromPackagePlan :: PackagePlan -> Spec
fromPackagePlan pp = Spec (ppVersion pp)
                          (pcFlagOverrides ppc)
                          (pcTests ppc == ExpectSuccess)
                          (pcHaddocks ppc == ExpectSuccess)
                          (pcSkipBuild ppc)
  where ppc = ppConstraints pp

readBuildPlan :: FilePath -> IO BuildPlan
readBuildPlan = fmap fromJust . decodeFile

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
  let isFile p = doesFileExist (path </> p)
  getDirectoryContents path >>= filterM isFile . Prelude.filter (\x -> head x /= '.')
