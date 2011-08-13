{-# LANGUAGE PatternGuards #-}

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
import qualified Data.ByteString.Char8 as BS
import Text.Regex.Posix
import Data.Version
import Text.ParserCombinators.ReadP ( readP_to_S )
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.PackageDescription ( GenericPackageDescription() )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import Cabal2Nix.Package ( cabal2nix, showNixPkg, PkgName, PkgSHA256, PkgPlatforms, PkgMaintainers
                         , PkgNoHaddock )
import qualified Cabal2Nix.Package ( Pkg(..) )

type ByteString = BS.ByteString

pack :: String -> ByteString
pack = BS.pack

unpack :: ByteString -> String
unpack = BS.unpack

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
  | "." `isPrefixOf` takeFileName dirOrFile  = msgDebug $ "ignore file or directory " ++ dirOrFile
  | otherwise                                = do
     isFile <- io (doesFileExist dirOrFile)
     case (isFile, takeExtension dirOrFile) of
       (True,".nix") -> do
         msgDebug ("discovered file " ++ dirOrFile)
         yield dirOrFile
       (True,_) -> msgDebug $ "ignore file " ++ dirOrFile
       (False,_) -> do
         msgDebug ("discovered dir " ++ dirOrFile)
         io (readDirectory dirOrFile) >>= mapM_ (discoverNixFiles yield . (dirOrFile </>))

type PkgVersion = String
data Pkg = Pkg PkgName PkgVersion PkgSHA256 PkgNoHaddock PkgPlatforms PkgMaintainers FilePath
  deriving (Show, Eq, Ord)

regmatch :: ByteString -> String -> Bool
regmatch buf patt = match (makeRegexOpts compExtended execBlank (pack patt)) buf

regsubmatch :: ByteString -> String -> [ByteString]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (ByteString,ByteString,ByteString,[ByteString])
        f = match (makeRegexOpts compExtended execBlank (pack patt)) buf

normalizeMaintainer :: String -> String
normalizeMaintainer x
  | "self.stdenv.lib.maintainers." `isPrefixOf` x = drop 28 x
  | otherwise                                     = x

parseNixFile :: FilePath -> ByteString -> Hackage4Nix (Maybe Pkg)
parseNixFile path buf
  | not (buf `regmatch` "cabal.mkDerivation")
               = msgDebug ("ignore non-cabal package " ++ path) >> return Nothing
  | True    <- (pack path) `regmatch` (concat (intersperse "|" badPackages))
               = msgDebug ("ignore known bad package " ++ path) >> return Nothing
  | True    <- buf `regmatch` "src = (fetchgit|sourceFromHead)"
               = msgDebug ("ignore non-hackage package " ++ path) >> return Nothing
  | buf `regmatch` "preConfigure|configureFlags|postInstall|patchPhase"
               = msgInfo ("ignore patched package " ++ path) >> return Nothing
  | True    <- buf =~ pack "cabal.mkDerivation"
  , [name]  <- buf `regsubmatch` "name *= *\"([^\"]+)\""
  , [vers]  <- buf `regsubmatch` "version *= *\"([^\"]+)\""
  , [sha]   <- buf `regsubmatch` "sha256 *= *\"([^\"]+)\""
  , plats   <- buf `regsubmatch` "platforms *= *([^;]+);"
  , maint   <- buf `regsubmatch` "maintainers *= *\\[([^\"]+)]"
  , haddock <- buf `regsubmatch` "noHaddock *= *(true|false) *;"
              = let plats' = concatMap BS.words (map (BS.map (\c -> if c == '+' then ' ' else c)) plats)
                    maint' = concatMap BS.words maint
                    noHaddock
                      | b:[] <- haddock, unpack b == "true" = True
                      | otherwise                           = False
                in
                  return $ Just $ Pkg (unpack name)
                                      (unpack vers)
                                      (unpack sha)
                                      noHaddock
                                      (map unpack plats')
                                      (map (normalizeMaintainer . unpack) maint')
                                      (path)
  | True <- buf `regmatch` "cabal.mkDerivation"
              = msgInfo ("failed to parse file " ++ path) >> return Nothing
  | otherwise = return Nothing

readVersion :: String -> Version
readVersion str =
  case [ v | (v,[]) <- readP_to_S parseVersion str ] of
    [ v' ] -> v'
    _      -> error ("invalid version specifier " ++ show str)

selectLatestVersions :: PkgSet -> PkgSet
selectLatestVersions = Set.fromList . nubBy f2 . sortBy f1 . Set.toList
  where
    f1 (Pkg n1 v1 _ _ _ _ _) (Pkg n2 v2 _ _ _ _ _)
      | n1 == n2  = compare v2 v1
      | otherwise = compare n1 n2
    f2 (Pkg n1 _ _ _ _ _ _) (Pkg n2 _ _ _ _ _ _)
      = n1 == n2

discoverUpdates :: PkgName -> PkgVersion -> Hackage4Nix [PkgVersion]
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
      nix' <- io (BS.readFile file) >>= parseNixFile file
      flip (maybe (return ())) nix' $ \nix -> do
        let Pkg name vers sha noHaddock plats maints path = nix
            maints' = nub (sort (maints ++ ["andres","simons"]))
            plats'
              | null plats && not (null maints) = ["self.ghc.meta.platforms"]
              | otherwise                       = plats
        msgDebug ("re-generate " ++ path)
        when (null maints) (msgInfo ("no maintainers configured for " ++ path))
        pkg <- readCabalFile name vers
        io $ writeFile path (showNixPkg (cabal2nix pkg sha noHaddock plats' maints'))
        modify $ \cfg -> cfg { _pkgset = Set.insert nix (_pkgset cfg) }
  pkgset <- gets (selectLatestVersions . _pkgset)
  updates' <- flip mapM (Set.elems pkgset) $ \pkg -> do
    let Pkg name vers _ _ _ _ _ = pkg
    updates <- discoverUpdates name vers
    return (pkg,updates)
  let updates = [ u | u@(_,(_:_)) <- updates' ]
  when (not (null updates)) $ do
    msgInfo "The following updates are available:"
    flip mapM_ updates $ \(pkg,versions) -> do
      let Pkg name vers _ noHaddock plats maints path = pkg
      msgInfo ""
      msgInfo $ name ++ "-" ++ vers ++ ":"
      flip mapM_ versions $ \newVersion -> do
        msgInfo $ "  " ++ genCabal2NixCmdline (Pkg name newVersion undefined noHaddock plats maints path)
  return ()

genCabal2NixCmdline :: Pkg -> String
genCabal2NixCmdline (Pkg name vers _ noHaddock plats maints path) = unwords $ ["cabal2nix"] ++ opts ++ [">"++path']
    where
      opts = [cabal] ++ maints' ++ plats' ++ if noHaddock then ["--no-haddock"] else []
      cabal = "cabal://" ++ name ++ "-" ++ vers
      maints' = [ "--maintainer=" ++ m | m <- maints ]
      plats'
        | ["self.ghc.meta.platforms"] == plats     = []
        | otherwise                                =  [ "--platform=" ++ p | p <- plats ]
      path'
        | pack path `regmatch` "/[0-9\\.]+\\.nix$" = replaceFileName path (vers <.> "nix")
        | otherwise                                = path

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



-- Packages that we cannot regenerate automatically yet. This list
-- should be empty.

badPackages :: [String]
badPackages = [ "/"++p++"/" | p <- names ]
  where names =
          [ "alex"              -- undeclared dependencies
          , "CS173Tourney"      -- parser error
          , "get-options"       -- parser error
          , "WebServer"         -- these two are not on Hackage
          , "WebServer-Extras"
          , "cairo"             -- undeclared dependencies
          , "citeproc-hs"       -- undeclared dependencies
          , "editline"          -- undeclared dependencies
          , "flapjax"           -- parser error
          , "glade"             -- undeclared dependencies
          , "glib"              -- undeclared dependencies
          , "epic"              -- undeclared dependencies
          , "GLUT"              -- undeclared dependencies
          , "gtk"               -- undeclared dependencies
          , "gtksourceview2"    -- undeclared dependencies
          , "haddock"           -- undeclared dependencies
          , "haskell-platform"  -- not on Hackage
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
          , "vacuum"            -- undeclared dependencies
          , "wxHaskell"         -- undeclared dependencies
          , "X11"
          , "X11-xft"
          ]
