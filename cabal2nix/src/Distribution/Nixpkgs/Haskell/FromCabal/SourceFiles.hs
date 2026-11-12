{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nixpkgs.Haskell.FromCabal.SourceFiles
  ( insertFilesetItems,
  )
where

import Control.Lens (findMOf, folded, (%%~), _Left)
import Data.Foldable (toList)
import Data.Foldable1 (fold1)
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Distribution.Nixpkgs.Fetch (DerivFilesetFilter (..), DerivFilesetItem (..), derivKindLens)
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.FromCabal.SourceFiles.Compat
import Distribution.PackageDescription
import System.Directory (doesFileExist)
import System.FilePath

insertFilesetItems :: FilePath -> GenericPackageDescription -> Derivation -> IO Derivation
insertFilesetItems root GenericPackageDescription {..} = src . derivKindLens . _Left %%~ mkItems
  where
    PackageDescription {..} = packageDescription

    -- Written using https://hackage-content.haskell.org/package/Cabal-3.18.2.0/docs/src/Distribution.Simple.SrcDist.html#listPackageSources'
    -- as a reference. Comments refer to the same comments in that source.
    mkItems :: (FilePath, [DerivFilesetItem]) -> IO (FilePath, [DerivFilesetItem])
    mkItems (pkgFile, _) = do
      setupFiles <- fmap catMaybes $
        for ["Setup", "SetupHooks"] $ \name ->
          findMOf folded (doesFileExist . combine root) $ map (addExtension name) ["hs", "lhs"]

      let sourceTries =
            -- The .cabal file itself
            -- [or, in our case, possibly the package.yaml file]
            filePathToTrie pkgFile
              :| catMaybes
                [ -- Library sources
                  buildInfosToTrie (allLibraries packageDescription) condLibrary condSubLibraries libBuildInfo,
                  -- Executables sources
                  buildInfosToTrie executables Nothing condExecutables buildInfo,
                  -- Foreign library sources
                  buildInfosToTrie foreignLibs Nothing condForeignLibs foreignLibBuildInfo,
                  -- Test suites sources
                  buildInfosToTrie testSuites Nothing condTestSuites testBuildInfo,
                  -- Benchmarks sources
                  buildInfosToTrie benchmarks Nothing condBenchmarks benchmarkBuildInfo,
                  -- Data files
                  if nullDataDir dataDir then sfoldMap symbolicGlobToTrie dataFiles else Just $ dataDirToTrie dataDir,
                  -- Extra source files, extra doc files, extra files
                  sfoldMap symbolicGlobToTrie (extraSrcFiles ++ extraDocFiles ++ extraFiles),
                  -- License file(s)
                  sfoldMap symbolicPathToTrie licenseFiles,
                  -- Install-include files, without autogen-include files
                  -- [already included in buildInfoTrie]

                  -- Setup script, if it exists
                  -- SetupHooks script, if it exists
                  sfoldMap filePathToTrie setupFiles
                ]

      pure (pkgFile, trieToItems (normalise root) $ fold1 sourceTries)

    buildInfosToTrie :: (Foldable t) => [a] -> Maybe (t a) -> [(b, t a)] -> (a -> BuildInfo) -> Maybe FilesetTrie
    buildInfosToTrie components more morePairs biField =
      foldMap (buildInfoTrie . biField) $ components <> foldMap toList (more `maybeCons` map snd morePairs)

    buildInfoTrie :: BuildInfo -> Maybe FilesetTrie
    buildInfoTrie BuildInfo {..} =
      mconcat
        [ sfoldMap symbolicPathToTrie hsSourceDirs,
          sfoldMap symbolicFileToTrie $ concat [cSources, cxxSources, cmmSources, asmSources, jsSources],
          if null includeDirs then sfoldMap symbolicFileToTrie (installIncludes \\ autogenIncludes) else sfoldMap dataDirToTrie includeDirs
        ]

    nullDataDir = nullPath . getSymbolicPath_314
    dataDirToTrie = filePathToTrie . getSymbolicPath_314
    symbolicFileToTrie = filePathToTrie . getSymbolicPath_314
    symbolicGlobToTrie = globToTrie . getSymbolicPath_314
    symbolicPathToTrie = filePathToTrie . getSymbolicPath

data FilesetTrie
  = -- | file or unfiltered directory
    Tip
  | Filters {names :: Set String, extensions :: Set String, children :: Map String FilesetTrie}

instance Semigroup FilesetTrie where
  Filters lnames lexts lchildren <> Filters rnames rexts rchildren =
    Filters (lnames <> rnames) (lexts <> rexts) (M.unionWith (<>) lchildren rchildren)
  _ <> _ = Tip

trieCons :: String -> FilesetTrie -> FilesetTrie
trieCons name = Filters mempty mempty . M.singleton name

trieAppend :: [String] -> FilesetTrie -> FilesetTrie
trieAppend = flip $ foldr trieCons

filePathToTrie :: FilePath -> FilesetTrie
filePathToTrie = (`trieAppend` Tip) . splitDirectoriesNormalised

-- Per the Cabal documentation, there are only three valid glob forms:
--   path/to/*.ext
--   path/to/**/*.ext
--   path/to/**/file.ext
--
-- (See: https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#pkg-field-data-files)
--
-- This function assumes that stars don't appear anywhere else in a glob.
globToTrie :: FilePath -> FilesetTrie
globToTrie glob =
  let (filename, isRecursive, segments) = case reverse $ splitDirectoriesNormalised glob of
        (f : "**" : s) -> (f, True, s)
        (f : s) -> (f, False, s)
        [] -> ("", False, [])
   in reverse segments `trieAppend` case splitExtensions filename of
        ("*", '.' : ext) ->
          -- Strictly speaking, this clause handles the isRecursive case, and
          -- it morally ought to branch on isRecursive in order to handle
          -- non-recursive *.ext globs. But expressing non-recursive globs in
          -- the current lib.fileset API is wildly verbose. Omitting this
          -- branch errs on the side of including more files than necessary; if
          -- the glob is foo/*.ext, the fileset source would include
          -- foo/bar/qux.ext. This doesn't seem like a big deal.
          Filters mempty (S.singleton ext) mempty
        _
          | isRecursive -> Filters (S.singleton filename) mempty mempty
          | otherwise -> filename `trieCons` Tip

trieToItems :: FilePath -> FilesetTrie -> [DerivFilesetItem]
trieToItems prefix = \case
  Tip -> [DerivFilesetSingleItem prefix]
  Filters {..} ->
    ( fmap (DerivFilesetFilterItem prefix) . NE.nonEmpty $
        map DerivFilesetFilterByName (S.toList names)
          ++ map DerivFilesetFilterByExtension (S.toList extensions)
    )
      `maybeCons` M.foldMapWithKey (trieToItems . combine prefix) children

nullPath :: FilePath -> Bool
nullPath = \case
  "" -> True
  "." -> True
  _ -> False

splitDirectoriesNormalised :: FilePath -> [FilePath]
splitDirectoriesNormalised = (\p -> if nullPath p then [] else splitDirectories p) . normalise

maybeCons :: Maybe a -> [a] -> [a]
maybeCons = maybe id (:)

sfoldMap :: (Foldable t, Semigroup m) => (a -> m) -> t a -> Maybe m
sfoldMap f = foldMap $ Just . f
