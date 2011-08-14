module Main ( main ) where

import System.IO
import System.FilePath
import System.Directory
import System.Environment
import System.Exit
import Data.List
import qualified Data.Set as Set
import Control.Monad.State
import Control.Exception ( bracket )
import Text.Regex.Posix
import Data.Version
import Text.ParserCombinators.ReadP ( readP_to_S )
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.PackageDescription ( GenericPackageDescription() )
import Distribution.Text
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import Cabal2Nix.Package
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.Derivation.Meta

data Pkg = Pkg Derivation FilePath Bool
  deriving (Show, Eq, Ord)

type PkgSet = Set.Set Pkg

data Configuration = Configuration
  { _msgDebug  :: String -> IO ()
  , _msgInfo   :: String -> IO ()
  , _hackageDb :: FilePath
  , _pkgset    :: PkgSet
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { _msgDebug  = hPutStrLn stderr
  , _msgInfo   = hPutStrLn stderr
  , _hackageDb = "/dev/shm/hackage"
  , _pkgset    = Set.empty
  }

type Hackage4Nix a = StateT Configuration IO a

io :: (MonadIO m) => IO a -> m a
io = liftIO

readDirectory :: FilePath -> IO [FilePath]
readDirectory dirpath = do
  entries <- getDirectoryContents dirpath
  return [ x | x <- entries, x /= ".", x /= ".." ]

msgDebug, msgInfo :: String -> Hackage4Nix ()
msgDebug msg = get >>= \s -> io (_msgDebug s msg)
msgInfo msg = get >>= \s -> io (_msgInfo s msg)

readCabalFile :: String -> String -> Hackage4Nix GenericPackageDescription
readCabalFile name vers = do
  hackageDir <- gets _hackageDb
  let cabal = hackageDir </> name </> vers </> name <.> "cabal"
  pkg' <- fmap parsePackageDescription (io (readFile cabal))
  pkg <- case pkg' of
           ParseOk _ a -> return a
           ParseFailed err -> fail ("cannot parse cabal file " ++ cabal ++ ": " ++ show err)
  return pkg

discoverNixFiles :: (FilePath -> Hackage4Nix ()) -> FilePath -> Hackage4Nix ()
discoverNixFiles yield dirOrFile
  | "." `isPrefixOf` takeFileName dirOrFile  = return ()
  | otherwise                                = do
     isFile <- io (doesFileExist dirOrFile)
     case (isFile, takeExtension dirOrFile) of
       (True,".nix") -> yield dirOrFile
       (True,_)     -> return ()
       (False,_)    -> io (readDirectory dirOrFile) >>= mapM_ (discoverNixFiles yield . (dirOrFile </>))

regenerateDerivation :: Derivation -> String -> Bool
regenerateDerivation deriv buf = not (display (pname deriv) `elem` patchedPackages) &&
                                 not (buf =~ "preConfigure|configureFlags|postInstall|patchPhase")

readVersion :: String -> Version
readVersion str =
  case [ v | (v,[]) <- readP_to_S parseVersion str ] of
    [ v' ] -> v'
    _      -> error ("invalid version specifier " ++ show str)

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

discoverUpdates :: String -> String -> Hackage4Nix [String]
discoverUpdates name vers = do
  hackage <- gets _hackageDb
  versionStrings <- io $ readDirectory (hackage </> name)
  let versions = map readVersion versionStrings
  return [ showVersion v | v <- versions, v > readVersion vers ]

updateNixPkgs :: [FilePath] -> Hackage4Nix ()
updateNixPkgs paths = do
  msgDebug $ "updating = " ++ show paths
  flip mapM_ paths $ \fileOrDir ->
    flip discoverNixFiles fileOrDir $ \file -> do
      nix' <- io (readFile file) >>= parseNixFile file
      flip (maybe (return ())) nix' $ \nix -> do
        let Pkg deriv path regenerate = nix
            maints = maintainers (metaSection deriv)
            plats  = platforms (metaSection deriv)
        when (null maints) (msgInfo ("no maintainers configured for " ++ path))
        modify $ \cfg -> cfg { _pkgset = Set.insert nix (_pkgset cfg) }
        when regenerate $ do
          msgDebug ("re-generate " ++ path)
          let maints' = nub (sort (maints ++ ["self.stdenv.lib.maintainers.andres","self.stdenv.lib.maintainers.simons"]))
              plats'
                | null plats && not (null maints) = ["self.ghc.meta.platforms"]
                | otherwise                       = plats
          pkg <- readCabalFile (display (pname deriv)) (display (version deriv))
          let deriv'  = (cabal2nix pkg) { sha256 = sha256 deriv, runHaddock = runHaddock deriv }
              deriv'' = deriv' { metaSection = (metaSection deriv')
                                               { maintainers = maints'
                                               , platforms   = plats'
                                               }
                               }
          io $ writeFile path (show (disp deriv''))
  pkgset <- gets (selectLatestVersions . _pkgset)
  updates' <- flip mapM (Set.elems pkgset) $ \pkg -> do
    let Pkg deriv _ _ = pkg
    updates <- discoverUpdates (display (pname deriv)) (display (version deriv))
    return (pkg,updates)
  let updates = [ u | u@(_,(_:_)) <- updates' ]
  when (not (null updates)) $ do
    msgInfo "The following updates are available:"
    flip mapM_ updates $ \(pkg,versions) -> do
      let Pkg deriv path regenerate = pkg
      msgInfo ""
      msgInfo $ (display (pname deriv)) ++ "-" ++ (display (version deriv)) ++ ":"
      flip mapM_ versions $ \newVersion -> do
        let deriv' = deriv { version = readVersion newVersion }
        msgInfo $ "  " ++ genCabal2NixCmdline (Pkg deriv' path regenerate)
  return ()

genCabal2NixCmdline :: Pkg -> String
genCabal2NixCmdline (Pkg deriv path _) = unwords $ ["cabal2nix"] ++ opts ++ [">"++path']
  where
    meta = metaSection deriv
    opts = [cabal] ++ maints' ++ plats' ++ if runHaddock deriv then [] else ["--no-haddock"]
    cabal = "cabal://" ++ display (pname deriv) ++ "-" ++ display (version deriv)
    maints' = [ "--maintainer=" ++ normalizeMaintainer m | m <- maintainers meta ]
    plats'
      | ["self.ghc.meta.platforms"] == platforms meta = []
      | otherwise                                     =  [ "--platform=" ++ p | p <- platforms meta ]
    path'
      | path =~ "/[0-9\\.]+\\.nix$" = replaceFileName path (display (version deriv) <.> "nix")
      | otherwise                   = path

normalizeMaintainer :: String -> String
normalizeMaintainer x
  | "self.stdenv.lib.maintainers." `isPrefixOf` x = drop 28 x
  | otherwise                                     = x

data CliOption = PrintHelp | Verbose | HackageDB FilePath
  deriving (Eq)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  let options :: [OptDescr CliOption]
      options =
        [ Option ['h'] ["help"]     (NoArg PrintHelp)                 "show this help text"
        , Option ['v'] ["verbose"]  (NoArg Verbose)                   "enable noisy debug output"
        , Option []    ["hackage"]  (ReqArg HackageDB "HACKAGE-DIR")  "path to hackage database"
        ]

      usage :: String
      usage = usageInfo "Usage: hackage4nix [options] [dir-or-file ...]" options ++
        "\n\
        \The purpose of 'hackage4nix' is to keep all Haskell packages in our\n\
        \repository packages up-to-date. It scans a checked-out copy of\n\
        \Nixpkgs for expressions that use 'cabal.mkDerivation', and\n\
        \re-generates them in-place with cabal2nix.\n\
        \\n\
        \Because we don't want to generate a barrage of HTTP requests during\n\
        \that procedure, the tool expects a copy of the Hackage database\n\
        \available at some local path, i.e. \"/dev/shm/hackage\" by default.\n\
        \That directory can be set up as follows:\n\
        \\n\
        \  cabal update\n\
        \  mkdir -p /dev/shm/hackage\n\
        \  tar xf ~/.cabal/packages/hackage.haskell.org/00-index.tar -C /dev/shm/hackage\n"

      cmdlineError :: String -> IO a
      cmdlineError ""     = hPutStrLn stderr usage >> exitFailure
      cmdlineError errMsg = hPutStrLn stderr errMsg >> cmdlineError ""

  args' <- getArgs
  (opts,args) <- case getOpt Permute options args' of
     (o,n,[]  ) -> return (o,n)
     (_,_,errs) -> cmdlineError (concatMap (\e -> '*':'*':'*':' ':e) errs)

  when (PrintHelp `elem` opts) (cmdlineError "")

  let cfg = defaultConfiguration
            { _msgDebug  = if Verbose `elem` opts then _msgDebug defaultConfiguration else const (return ())
            , _hackageDb = last $ _hackageDb defaultConfiguration : [ p | HackageDB p <- opts ]
            }
  flip evalStateT cfg (updateNixPkgs args)


-- Packages that we cannot parse.

badPackagePaths :: [FilePath]
badPackagePaths = ["haskell-platform/2011.2.0.1.nix"]

-- Packages that we cannot regenerate automatically yet. This list
-- should be empty.

patchedPackages :: [String]
patchedPackages =
   [ "alex"              -- undeclared dependencies
   , "cairo"             -- undeclared dependencies
   , "citeproc-hs"       -- undeclared dependencies
   , "editline"          -- undeclared dependencies
   , "glade"             -- undeclared dependencies
   , "glib"              -- undeclared dependencies
   , "epic"              -- undeclared dependencies
   , "GLUT"              -- undeclared dependencies
   , "gtk"               -- undeclared dependencies
   , "gtksourceview2"    -- undeclared dependencies
   , "haddock"           -- undeclared dependencies
   , "haskell-src"       -- undeclared dependencies
   , "hmatrix"           -- undeclared dependencies
   , "hp2any-graph"      -- undeclared dependencies
   , "lhs2tex"           -- undeclared dependencies
   , "OpenAL"            -- undeclared dependencies
   , "OpenGL"            -- undeclared dependencies
   , "pango"             -- undeclared dependencies
   , "idris"             -- undeclared dependencies
   , "readline"          -- undeclared dependencies
   , "repa-examples"     -- undeclared dependencies
   , "scion"             -- expects non-existent networkBytestring
   , "SDL"               -- undeclared dependencies
   , "SDL-image"         -- undeclared dependencies
   , "SDL-mixer"         -- undeclared dependencies
   , "SDL-ttf"           -- undeclared dependencies
   , "svgcairo"          -- undeclared dependencies
   , "terminfo"          -- undeclared dependencies
   , "xmonad"            -- undeclared dependencies
   , "xmonad-extras"     -- undeclared dependencies
   , "vacuum"            -- undeclared dependencies
   , "wxcore"            -- undeclared dependencies
   , "X11"               -- undeclared dependencies
   , "X11-xft"           -- undeclared dependencies
   ]
