module Main ( main ) where

import Cabal2Nix.Normalize
import Cabal2Nix.Generate
import Control.Exception ( bracket )
import Control.Monad.RWS
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Version
import qualified Distribution.Hackage.DB as DB
import Distribution.NixOS.Derivation.Cabal
import Distribution.Package
import Distribution.PackageDescription ( GenericPackageDescription() )
import Distribution.Text
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Regex.Posix

data Pkg = Pkg Derivation FilePath Bool
  deriving (Show, Eq, Ord)

type PkgSet = Set.Set Pkg

data Configuration = Configuration
  { _msgDebug  :: String -> IO ()
  , _msgInfo   :: String -> IO ()
  , _hackageDb :: DB.Hackage
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { _msgDebug  = hPutStrLn stderr
  , _msgInfo   = hPutStrLn stderr
  , _hackageDb = DB.empty
  }

type Hackage4Nix a = RWST Configuration () PkgSet IO a

io :: (MonadIO m) => IO a -> m a
io = liftIO

readDirectory :: FilePath -> IO [FilePath]
readDirectory dirpath = do
  entries <- getDirectoryContents dirpath
  return [ x | x <- entries, x /= ".", x /= ".." ]

msgDebug, msgInfo :: String -> Hackage4Nix ()
msgDebug msg = ask >>= \s -> io (_msgDebug s msg)
msgInfo msg = ask >>= \s -> io (_msgInfo s msg)

getCabalPackage :: String -> Version -> Hackage4Nix GenericPackageDescription
getCabalPackage name vers = do
  db <- asks _hackageDb
  case DB.lookup name db of
    Just db' -> case DB.lookup vers db' of
                  Just pkg -> return pkg
                  Nothing  -> fail ("hackage doesn't know about " ++ name ++ " version " ++ display vers)
    Nothing  -> fail ("hackage doesn't know about " ++ show name)

discoverNixFiles :: (FilePath -> Hackage4Nix ()) -> FilePath -> Hackage4Nix ()
discoverNixFiles yield dirOrFile = do
  isFile <- io (doesFileExist dirOrFile)
  case (isFile, takeExtension dirOrFile) of
    (True,".nix") -> yield dirOrFile
    (True,_)     -> return ()
    (False,_)    -> io (readDirectory dirOrFile) >>= mapM_ (discoverNixFiles yield . (dirOrFile </>))

regenerateDerivation :: Derivation -> String -> Bool
regenerateDerivation deriv buf = not (buf =~ "(pre|post)Configure|(pre|post)Install|patchPhase|patches")

parseNixFile :: FilePath -> String -> Hackage4Nix (Maybe Pkg)
parseNixFile path buf
  | not (buf =~ "cabal.mkDerivation")
               = msgDebug ("ignore non-cabal package " ++ path) >> return Nothing
  | any (`isSuffixOf`path) badPackagePaths
               = msgDebug ("ignore known bad package " ++ path) >> return Nothing
  | buf =~ "src = (fetchurl|fetchgit|sourceFromHead)"
               = msgDebug ("ignore non-hackage package " ++ path) >> return Nothing
  | Just deriv <- parseDerivation buf
               = return (Just (Pkg deriv path (regenerateDerivation deriv buf)))
  | otherwise = msgInfo ("failed to parse file " ++ path) >> return Nothing

selectLatestVersions :: PkgSet -> PkgSet
selectLatestVersions = Set.fromList . nubBy f2 . sortBy f1 . Set.toList
  where
    f1 (Pkg deriv1 _ _) (Pkg deriv2 _ _)
      | pname deriv1 == pname deriv2     = compare (version deriv2) (version deriv1)
      | otherwise                        = compare (pname deriv1) (pname deriv2)
    f2 (Pkg deriv1 _ _) (Pkg deriv2 _ _) = pname deriv1 == pname deriv2

discoverUpdates :: String -> Version -> Hackage4Nix [Version]
discoverUpdates name vers = do
  db <- asks _hackageDb
  let versions = DB.keys (fromJust (DB.lookup name db))
  return (filter (>vers) versions)

updateNixPkgs :: [FilePath] -> Hackage4Nix ()
updateNixPkgs paths = do
  msgDebug $ "updating = " ++ show paths
  forM_ paths $ \fileOrDir ->
    flip discoverNixFiles fileOrDir $ \file -> do
      nix' <- io (readFile file) >>= parseNixFile file
      flip (maybe (return ())) nix' $ \nix -> do
        let Pkg deriv path regenerate = nix
            maints = maintainers (metaSection deriv)
            plats  = platforms (metaSection deriv)
        modify (Set.insert nix)
        when regenerate $ do
          msgDebug ("re-generate " ++ path)
          pkg <- getCabalPackage (pname deriv) (version deriv)
          let deriv'  = (cabal2nix pkg) { sha256 = sha256 deriv, runHaddock = runHaddock deriv, jailbreak = jailbreak deriv, doCheck = doCheck deriv }
              meta    = metaSection deriv'
              plats'  = if null plats then platforms meta else plats
              deriv'' = deriv' { metaSection = meta
                                               { maintainers = maints -- ++ ["andres"]
                                               , platforms   = plats'
                                               }
                               }
          io $ writeFile path (show (disp (normalize deriv'')))
  pkgset <- gets selectLatestVersions
  updates' <- forM (Set.elems pkgset) $ \pkg -> do
    let Pkg deriv _ _ = pkg
    updates <- discoverUpdates (pname deriv) (version deriv)
    return (pkg,updates)
  let updates = [ u | u@(_,_:_) <- updates' ]
  unless (null updates) $ do
    msgInfo "The following updates are available:"
    forM_ updates $ \(pkg,versions) -> do
      let Pkg deriv path regenerate = pkg
      msgInfo ""
      msgInfo $ display (packageId deriv) ++ ":"
      forM_ versions $ \newVersion -> do
        let deriv' = deriv { version = newVersion }
        msgInfo $ "  " ++ genCabal2NixCmdline (Pkg deriv' path regenerate)
  return ()

genCabal2NixCmdline :: Pkg -> String
genCabal2NixCmdline (Pkg deriv path _) = unwords $ ["cabal2nix"] ++ opts ++ ['>':path']
  where
    meta = metaSection deriv
    opts = [cabal] ++ maints' ++ plats' ++ (if runHaddock deriv then [] else ["--no-haddock"]) ++ (if doCheck deriv then [] else ["--no-check"])
    cabal = "cabal://" ++ display (packageId deriv)
    maints' = [ "--maintainer=" ++ normalizeMaintainer m | m <- maintainers meta ]
    plats'
      | ["self.ghc.meta.platforms"] == platforms meta = []
      | otherwise                                     = [ "--platform=" ++ p | p <- platforms meta ]
    path'
      | path =~ "/[0-9\\.]+\\.nix$" = replaceFileName path (display (version deriv) <.> "nix")
      | otherwise                   = path

normalizeMaintainer :: String -> String
normalizeMaintainer x
  | "self.stdenv.lib.maintainers." `isPrefixOf` x = drop 28 x
  | otherwise                                     = x

data CliOption = PrintHelp | Verbose
  deriving (Eq)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  let options :: [OptDescr CliOption]
      options =
        [ Option "h" ["help"]     (NoArg PrintHelp)                 "show this help text"
        , Option "v" ["verbose"]  (NoArg Verbose)                   "enable noisy debug output"
        ]

      usage :: String
      usage = usageInfo "Usage: hackage4nix [options] [dir-or-file ...]" options ++ unlines
              [ ""
              , "The purpose of 'hackage4nix' is to keep all Haskell packages in our"
              , "repository packages up-to-date. It scans a checked-out copy of"
              , "Nixpkgs for expressions that use 'cabal.mkDerivation', and"
              , "re-generates them in-place with cabal2nix."
              ]

      cmdlineError :: String -> IO a
      cmdlineError ""     = hPutStrLn stderr usage >> exitFailure
      cmdlineError errMsg = hPutStrLn stderr errMsg >> cmdlineError ""

  args' <- getArgs
  (opts,args) <- case getOpt Permute options args' of
     (o,n,[]  ) -> return (o,n)
     (_,_,errs) -> cmdlineError (concatMap (\e -> '*':'*':'*':' ':e) errs)

  when (PrintHelp `elem` opts) (cmdlineError "")

  hackage <- DB.readHackage
  let cfg = defaultConfiguration
            { _msgDebug  = if Verbose `elem` opts then _msgDebug defaultConfiguration else const (return ())
            , _hackageDb = hackage
            }
  ((),_,()) <- runRWST (updateNixPkgs args) cfg Set.empty
  return ()


-- Packages that we cannot parse.

badPackagePaths :: [FilePath]
badPackagePaths = [ "haskell-platform/2009.2.0.2.nix", "haskell-platform/2010.1.0.0.nix"
                  , "haskell-platform/2010.2.0.0.nix", "haskell-platform/2011.2.0.0.nix"
                  , "haskell-platform/2011.2.0.1.nix", "haskell-platform/2011.4.0.0.nix"
                  , "haskell-platform/2012.2.0.0.nix", "haskell-platform/2012.4.0.0.nix"
                  , "top-level/all-packages.nix",      "top-level/haskell-packages.nix"
                  ]
