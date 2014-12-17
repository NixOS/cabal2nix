module Main ( main ) where

import Cabal2Nix.Generate
import Cabal2Nix.Normalize
import Control.Exception ( bracket )
import Control.Monad.RWS
import Data.List
import qualified Data.Set as Set
import Data.Version
import qualified Cabal2Nix.Hackage as DB
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
  , _force     :: Bool
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { _msgDebug  = hPutStrLn stderr
  , _msgInfo   = hPutStrLn stderr
  , _hackageDb = DB.empty
  , _force     = False
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
regenerateDerivation _ buf = not (buf =~ "(pre|post)Configure|(pre|post)Install|patchPhase|patches|broken")

parseNixFile :: FilePath -> String -> Hackage4Nix (Maybe Pkg)
parseNixFile path buf
  | not (buf =~ "cabal.mkDerivation")
               = msgDebug ("ignore non-cabal package " ++ path) >> return Nothing
  | any (`isSuffixOf`path) badPackagePaths
               = msgDebug ("ignore known bad package " ++ path) >> return Nothing
  | Just deriv <- parseDerivation buf
               = do forceReGen <- asks _force
                    let reGen = regenerateDerivation deriv buf
                    return (Just (Pkg deriv path (forceReGen || reGen)))
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
  case DB.lookup name db of
    Just pkgs -> return (filter (>vers) (DB.keys pkgs))
    Nothing   -> fail ("discoverUpdates cannot find package " ++ show name ++ " on Hackage")

updateNixPkgs :: [FilePath] -> Hackage4Nix ()
updateNixPkgs paths = do
  -- Traverse the given directorcy structure and discover all available
  -- Haskell packages.
  forM_ paths $ \fileOrDir -> do
    msgDebug $ "scanning " ++ show fileOrDir
    flip discoverNixFiles fileOrDir $ \file -> do
      nix' <- io (readFile file) >>= parseNixFile file
      flip (maybe (return ())) nix' $ \nix ->
        modify (Set.insert nix)

  -- Re-generate all Haskell packages in-place (unless
  -- 'regenerateDerivation' decided that this particular package
  -- shouldn't be touched.
  latestVersions <- gets selectLatestVersions
  let latestVersionMap :: [(String,Version)]
      latestVersionMap = [ (pname deriv, version deriv) | Pkg deriv _ _ <- Set.toList latestVersions ]
  let isLatest :: Derivation -> Bool
      isLatest deriv = maybe False (version deriv ==) (lookup (pname deriv) latestVersionMap)
  get >>= \pkgset -> forM_ (Set.toList pkgset) $ \nix -> do
    let Pkg deriv path regenerate = nix
        maints = maintainers (metaSection deriv)
        plats  = platforms (metaSection deriv)
        hplats = if isLatest deriv then hydraPlatforms (metaSection deriv) else ["self.stdenv.lib.platforms.none"]

    when regenerate $ do
      msgDebug ("re-generate " ++ path)
      pkg <- getCabalPackage (pname deriv) (version deriv)
      let deriv'  = (cabal2nix pkg) { src = src deriv
                                    , runHaddock = runHaddock deriv
                                    , doCheck = doCheck deriv
                                    , jailbreak = jailbreak deriv
                                    , hyperlinkSource = hyperlinkSource deriv
                                    , enableSplitObjs = enableSplitObjs deriv
                                    }
          meta    = metaSection deriv'
          plats'  = if null plats then platforms meta else plats
          deriv'' = deriv' { metaSection = meta
                                           { maintainers    = maints
                                           , platforms      = plats'
                                           , hydraPlatforms = hplats
                                           }
                           }
      io $ writeFile path (show (disp (normalize deriv'')))

  -- Discover available updates and print the appropriate cabal2nix
  -- command line to the console for the user to execute at his/her
  -- discretion.
  updates' <- forM (Set.elems latestVersions) $ \pkg -> do
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
    opts = [cabal] ++ maints' ++ plats' ++ hplats'
                   ++ (if jailbreak deriv then ["--jailbreak"] else [])
                   ++ (if runHaddock deriv then [] else ["--no-haddock"])
                   ++ (if doCheck deriv then [] else ["--no-check"])
    cabal = "cabal://" ++ display (packageId deriv)
    maints' = [ "--maintainer=" ++ normalizeMaintainer m | m <- maintainers meta ]
    plats'
      | ["self.ghc.meta.platforms"] == platforms meta = []
      | otherwise                                     = [ "--platform=" ++ p | p <- platforms meta ]
    hplats'
      | ["self.ghc.meta.platforms"] == platforms meta = []
      | otherwise                                     = [ "--hydra-platform=" ++ p | p <- hydraPlatforms meta ]
    path'
      | path =~ "/[0-9\\.]+\\.nix$" = replaceFileName path (display (version deriv) <.> "nix")
      | otherwise                   = path

normalizeMaintainer :: String -> String
normalizeMaintainer x
  | "self.stdenv.lib.maintainers." `isPrefixOf` x = drop 28 x
  | otherwise                                     = x

data CliOption = PrintHelp | Verbose | ForceRegen
  deriving (Eq)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  let options :: [OptDescr CliOption]
      options =
        [ Option "h" ["help"]     (NoArg PrintHelp)                 "show this help text"
        , Option "v" ["verbose"]  (NoArg Verbose)                   "enable noisy debug output"
        , Option ""  ["force"]    (NoArg ForceRegen)                "re-generate all files in place"
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

  hackage <- DB.readHashedHackage
  let cfg = defaultConfiguration
            { _msgDebug  = if Verbose `elem` opts then _msgDebug defaultConfiguration else const (return ())
            , _hackageDb = hackage
            , _force     = ForceRegen `elem` opts
            }
  ((),_,()) <- runRWST (updateNixPkgs args) cfg Set.empty
  return ()


-- Packages that cabal2nix cannot generate build expressions for.

badPackagePaths :: [FilePath]
badPackagePaths = [ -- These expression are not found on Hackage:
                    "haskell-platform/2009.2.0.2.nix", "haskell-platform/2010.1.0.0.nix"
                  , "haskell-platform/2010.2.0.0.nix", "haskell-platform/2011.2.0.0.nix"
                  , "haskell-platform/2011.2.0.1.nix", "haskell-platform/2011.4.0.0.nix"
                  , "haskell-platform/2012.2.0.0.nix", "haskell-platform/2012.4.0.0.nix"
                  , "haskell-platform/2013.2.0.0.nix", "compilers/flapjax/default.nix"
                  , "pkgs/games/uqm/3dovideo.nix",     "haskell/ghcjs-prim/default.nix"
                  , "system/journal-mailer/default.nix", "development/compilers/ghcjs/default.nix"
                    -- Our primitive parser cannot handle these files.
                  , "top-level/all-packages.nix", "top-level/haskell-packages.nix"
                    -- This build is way too complicated to maintain it automatically.
                  , "pkgs/development/compilers/pakcs/default.nix"
                  , "pkgs/development/libraries/haskell/hoogle/local.nix"
                    -- Not registered on Hackage.
                  , "pkgs/development/compilers/agda/stdlib.nix"
                  , "pkgs/development/compilers/cryptol/1.8.x.nix"
                  , "pkgs/development/compilers/cryptol/2.0.x.nix"
                  , "pkgs/development/compilers/jhc/default.nix"
                  , "pkgs/development/tools/haskell/cabal-delete/default.nix"
                  , "pkgs/tools/networking/sproxy-web/default.nix"
                  , "pkgs/tools/networking/sproxy/default.nix"
                  , "pkgs/applications/editors/yi/yi-custom.nix"
                  ]
