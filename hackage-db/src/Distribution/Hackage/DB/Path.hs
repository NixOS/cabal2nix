{- |
   Module      :  Distribution.Hackage.DB.Path
   License     :  BSD3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   Find the location of the local Hackage database that is maintained by running
   @cabal update@.
 -}

module Distribution.Hackage.DB.Path where

import Distribution.Hackage.DB.Errors

import Control.Exception
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

-- |
-- Determines the /state/ directory (which e.g. holds the hackage tarball)
-- cabal-install uses via the following logic:
--
-- 1. If the @CABAL_DIR@ environment variable is set, its content is used as the
--    cabal state directory
-- 2. If @~/.cabal@ (see 'getAppUserDataDirectory') exists, use that.
-- 3. Otherwise, use @${XDG_CACHE_HOME}/cabal@ (see @'getXdgDirectory' 'XdgCache'@)
--    which is the new directory cabal-install can use starting with
--    version @3.10.*@.
--
-- This logic is mostly equivalent to what upstream cabal-install is
-- [doing](https://github.com/haskell/cabal/blob/0ed12188525335ac9759dc957d49979ab09382a1/cabal-install/src/Distribution/Client/Config.hs#L594-L610)
-- with the following exception:
-- The state directory can freely be configured to use a different location
-- in the cabal-install configuration file. hackage-db doesn't parse this
-- configuration file, so differing state directories are ignored.
cabalStateDir :: IO FilePath
cabalStateDir = do
  envCabal <- lookupEnv "CABAL_DIR"
  dotCabal <- getAppUserDataDirectory "cabal"
  dotCabalExists <- doesDirectoryExist dotCabal
  case envCabal of
    Just dir -> pure dir
    Nothing ->
      if dotCabalExists
      then pure dotCabal
      else getXdgDirectory XdgCache "cabal"

cabalTarballDir :: String -> IO FilePath
cabalTarballDir repo = do
  csd <- cabalStateDir
  return $ joinPath [csd, "packages", repo]

hackageTarballDir :: IO FilePath
hackageTarballDir = cabalTarballDir "hackage.haskell.org"

-- | Determine the default path of the Hackage database, which typically
-- resides in @$HOME\/.cabal\/packages\/hackage.haskell.org\/@.
-- Running the command @cabal update@ or @cabal v2-update@ will keep the index
-- up-to-date.
--
-- See 'cabalStateDir' on how @hackage-db@ searches for the cabal state directory.
hackageTarball :: IO FilePath
hackageTarball = do
  htd <- hackageTarballDir
  let idx00 = htd </> "00-index.tar"
      idx01 = htd </> "01-index.tar"
  -- Using 'msum' here would be nice, but unfortunetaly there was no reliable
  -- MonadPlus instance for IO in pre 8.x versions of GHC. So we use the ugly
  -- code for sake of portability.
  have01 <- doesFileExist idx01
  if have01 then return idx01 else do
    have00 <- doesFileExist idx00
    if have00 then return idx00 else
      throwIO NoHackageTarballFound
