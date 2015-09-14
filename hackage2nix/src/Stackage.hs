{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stackage where

import Control.DeepSeq.Generics
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans
import Data.List
import Data.List.Split
import Data.Map as Map
import Data.Maybe
import Data.Time.Calendar
import Data.Yaml
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import GHC.Generics ( Generic )
import Stackage.Types hiding ( display )
import System.Directory
import System.FilePath
import Text.PrettyPrint.HughesPJClass

data Spec = Spec
            { version :: Version
            , flagOverrides :: Map FlagName Bool
            , runTests :: Bool
            , runHaddock :: Bool
            , runLinuxBuilds :: Bool
            }
  deriving (Show, Generic)

instance Eq Spec where
  a == b = version a == version b

instance Ord Spec where
  compare a b = compare (version a) (version b)

data Snapshot = Snapshot
            { snapshot :: SnapshotType
            , compiler :: CompilerId
            , corePackages :: Map PackageName Version
            , packages :: Map PackageName Spec
            }
  deriving (Show, Generic)

deriving instance Generic SnapshotType
instance NFData Spec where
instance NFData Snapshot where
instance NFData SnapshotType where

instance Pretty SnapshotType where
  pPrint STNightly = text "stackage-nightly"
  pPrint (STNightly2 _) = text "stackage-nightly"
  pPrint (STLTS m n) = text "lts-" <> int m <> char '.' <> int n

readStackage :: FilePath -> ParIO [Snapshot]
readStackage dirPath = do
  filePaths <- liftIO (listFiles dirPath)
  parMapM (liftIO . readSnapshot) [ dirPath </> p | p <- filePaths, takeExtension p == ".yaml" ]

readSnapshot :: FilePath -> IO Snapshot
readSnapshot p = fmap (fromBuildPlan (parseSnapshotType (takeFileName p))) (readBuildPlan p)

parseSnapshotType :: FilePath -> SnapshotType
parseSnapshotType p
  | "lts-" `isPrefixOf` p
  , [major,minor] <- splitOn "." (drop 4 (dropExtension p))
                = STLTS (read major) (read minor)
  | "nightly-" `isPrefixOf` p
  , [year,month,day] <- splitOn "-" (drop 8 (dropExtension p))
                = STNightly2 (fromGregorian (read year) (read month) (read day))
  | otherwise   = error ("parseSnapshotType: invalid file name " ++ p)

readBuildPlan :: FilePath -> IO BuildPlan
readBuildPlan = fmap fromJust . decodeFile

fromBuildPlan :: SnapshotType -> BuildPlan -> Snapshot
fromBuildPlan snt bp = Snapshot snt
                                (CompilerId GHC (siGhcVersion (bpSystemInfo bp)))
                                (siCorePackages (bpSystemInfo bp))
                                (Map.map fromPackagePlan (bpPackages bp))

fromPackagePlan :: PackagePlan -> Spec
fromPackagePlan pp = Spec (ppVersion pp)
                          (pcFlagOverrides ppc)
                          (pcTests ppc == ExpectSuccess)
                          (pcHaddocks ppc == ExpectSuccess)
                          (not (pcSkipBuild ppc))
  where ppc = ppConstraints pp

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
  let isFile p = doesFileExist (path </> p)
  getDirectoryContents path >>= filterM isFile . Prelude.filter (\x -> head x /= '.')
