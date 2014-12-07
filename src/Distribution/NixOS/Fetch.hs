{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.NixOS.Fetch
  ( Source(..)
  , DerivationSource(..), fromDerivationSource
  , fetch
  , fetchWith
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

-- | A source is a location from which we can fetch, such as a HTTP URL, a GIT URL, ....
data Source = Source
  { sourceUrl       :: String       -- ^ URL to fetch from.
  , sourceRevision  :: String       -- ^ Revision to use. For protocols where this doesn't make sense (such as HTTP), this
                                    --   should be the empty string.
  , sourceHash      :: Maybe String -- ^ The expected hash of the source, if available.
  } deriving (Show, Eq, Ord)

-- | A source for a derivation. It always needs a hash and also has a protocol attached to it (url, git, svn, ...).
-- A @DerivationSource@ also always has it's revision fully resolved (not relative revisions like @master@, @HEAD@, etc).
data DerivationSource = DerivationSource
  { derivKind     :: String -- ^ The kind of the source. The name of the build-support fetch derivation should be fetch<kind>.
  , derivUrl      :: String -- ^ URL to fetch from.
  , derivRevision :: String -- ^ Revision to use. Leave empty if the fetcher doesn't support revisions.
  , derivHash     :: String -- ^ The hash of the source.
  } deriving (Show, Eq, Ord)

instance NFData DerivationSource where
  rnf (DerivationSource a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()

fromDerivationSource :: DerivationSource -> Source
fromDerivationSource DerivationSource{..} = Source derivUrl derivRevision $ Just derivHash

-- | Fetch a source, trying any of the various nix-prefetch-* scripts.
fetch :: forall a. (String -> MaybeT IO a)      -- ^ This function is passed the output path name as an argument.
                                                -- It should return 'Nothing' if the file doesn't match the expected format.
                                                -- This is required, because we cannot always check if a download succeeded otherwise.
      -> Source                                 -- ^ The source to fetch from.
      -> IO (Maybe (DerivationSource, a))       -- ^ The derivation source and the result of the processing function. Returns Nothing if the download failed.
fetch f = runMaybeT . fetchers where
  fetchers :: Source -> MaybeT IO (DerivationSource, a)
  fetchers source = msum . (fetchLocal source :) $ map (\fetcher -> fetchWith fetcher source >>= process)
    [ (False, "zip", [])
    , (False, "url", [])
    , (True, "git", ["--fetch-submodules"])
    , (True, "hg", [])
    , (True, "svn", [])
    , (True, "bzr", [])
    ]

  -- | Remove '/' from the end of the path. Nix doesn't accept paths that
  -- end in a slash.
  stripSlashSuffix :: String -> String
  stripSlashSuffix = reverse . dropWhile (== '/') . reverse

  fetchLocal :: Source -> MaybeT IO (DerivationSource, a)
  fetchLocal src = do
    let path = stripSlashSuffix $ stripPrefix "file://" $ sourceUrl src
    existsFile <- liftIO $ doesFileExist path
    existsDir  <- liftIO $ doesDirectoryExist path
    guard $ existsDir || existsFile
    let path' | '/' `elem` path = path
              | otherwise       = "./" ++ path
    process (DerivationSource "" path' "" "", path') <|> localArchive path'

  localArchive :: FilePath -> MaybeT IO (DerivationSource, a)
  localArchive path = do
    absolutePath <- liftIO $ canonicalizePath path
    unpacked <- snd <$> fetchWith (False, "zip", []) (Source ("file://" ++ absolutePath) "" Nothing)
    process (DerivationSource "" absolutePath "" "", unpacked)

  process :: (DerivationSource, FilePath) -> MaybeT IO (DerivationSource, a)
  process (derivSource, file) = (,) derivSource <$> f file

-- | Like 'fetch', but allows to specify which script to use.
fetchWith :: (Bool, String, [String]) -> Source -> MaybeT IO (DerivationSource, FilePath)
fetchWith (supportsRev, kind, addArgs) source = do
  unless ((sourceRevision source /= "") || isNothing (sourceHash source) || not supportsRev) $
    liftIO (hPutStrLn stderr "** need a revision for VCS when the hash is given. skipping.") >> mzero

  MaybeT $ liftIO $ do
    envs <- getEnvironment
    (Nothing, Just stdoutH, _, processH) <- createProcess (proc script args)
      { env = Just $ ("PRINT_PATH", "1") : envs
      , std_in = Inherit
      , std_err = Inherit
      , std_out = CreatePipe
      }

    exitCode <- waitForProcess processH
    case exitCode of
      ExitSuccess   -> fmap Just . processOutputSwitch . lines =<< hGetContents stdoutH
      ExitFailure _ -> return Nothing

 where

   script :: String
   script = "nix-prefetch-" ++ kind

   args :: [String]
   args = addArgs ++ sourceUrl source : [ sourceRevision source | supportsRev ] ++ maybeToList (sourceHash source)

   processOutputWithRev :: [String] -> IO (DerivationSource, FilePath)
   processOutputWithRev [rev,hash,path] = return (DerivationSource kind (sourceUrl source) (extractRevision rev) hash, path)
   processOutputWithRev out = unexpectedOutput out

   extractRevision :: String -> String
   extractRevision = unwords . drop 3 . words -- drop the "<vcs> revision is" prefix

   processOutput :: [String] -> IO (DerivationSource, FilePath)
   processOutput [hash,path] = return (DerivationSource kind (sourceUrl source) (sourceRevision source) hash, path)
   processOutput out = unexpectedOutput out

   unexpectedOutput :: [String] -> IO a
   unexpectedOutput out = do
     hPutStrLn stderr $ "*** unexpected output from " ++ "nix-prefetch-" ++ kind ++ " script: "
     hPutStr   stderr $ unlines out
     exitFailure

   processOutputSwitch :: [String] -> IO (DerivationSource, FilePath)
   processOutputSwitch
     | supportsRev && isNothing (sourceHash source) = processOutputWithRev
     | otherwise                                   = processOutput

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix as
  | prefix' == prefix = stripped
  | otherwise = as
 where
  (prefix', stripped) = splitAt (length prefix) as
