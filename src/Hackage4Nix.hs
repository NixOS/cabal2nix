{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Main ( main ) where

import System.IO
import System.FilePath
import System.Directory
import System.Environment
import System.Exit
import Data.List
import Control.Monad.State
import Control.Exception ( bracket )
import qualified Data.ByteString.Char8 as BS
import Text.Regex.Posix
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.PackageDescription ( GenericPackageDescription() )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import Cabal2Nix.Package ( cabal2nix, showNixPkg, PkgName, PkgSHA256, PkgPlatforms, PkgMaintainers )

type ByteString = BS.ByteString

pack :: String -> ByteString
pack = BS.pack

unpack :: ByteString -> String
unpack = BS.unpack

data Configuration = Configuration
  { _msgDebug  :: String -> IO ()
  , _msgInfo   :: String -> IO ()
  , _hackageDb :: FilePath
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { _msgDebug  = hPutStrLn stderr
  , _msgInfo   = hPutStrLn stderr
  , _hackageDb = "/dev/shm/hackage"
  }

type Hackage4Nix a = StateT Configuration IO a

io :: (MonadIO m) => IO a -> m a
io = liftIO

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

badPackages :: [String]
badPackages =
  [ "haskell-platform"
  , "flapjax"
  ]

discoverNixFiles :: (FilePath -> Hackage4Nix ()) -> FilePath -> Hackage4Nix ()
discoverNixFiles yield dirOrFile
  | takeFileName dirOrFile `elem` [".",".."] = return ()
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
         io (getDirectoryContents dirOrFile) >>= mapM_ (discoverNixFiles yield . (dirOrFile </>))

type PkgVersion = String
data Pkg = Pkg PkgName PkgVersion PkgSHA256 PkgPlatforms PkgMaintainers FilePath
  deriving (Show)

regmatch :: ByteString -> String -> Bool
regmatch buf patt = match (makeRegexOpts compExtended execBlank (pack patt)) buf

regsubmatch :: ByteString -> String -> [ByteString]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (ByteString,ByteString,ByteString,[ByteString])
        f = match (makeRegexOpts compExtended execBlank (pack patt)) buf

parseNixFile :: FilePath -> ByteString -> Hackage4Nix (Maybe Pkg)
parseNixFile path buf
  | True    <- buf `regmatch` "src = (fetchgit|sourceFromHead)"
               = msgDebug ("ignore non-hackage package " ++ path) >> return Nothing
  | True    <- (pack path) `regmatch` (concat (intersperse "|" badPackages))
               = msgInfo ("ignore known bad package " ++ path) >> return Nothing
  | True    <- buf =~ pack "cabal.mkDerivation"
  , [name]  <- buf `regsubmatch` "name *= *\"([^\"]+)\""
  , [vers]  <- buf `regsubmatch` "version *= *\"([^\"]+)\""
  , [sha]   <- buf `regsubmatch` "sha256 *= *\"([^\"]+)\""
  , plats   <- buf `regsubmatch` "platforms *= *([^;]+);"
  , maint   <- buf `regsubmatch` "maintainers *= *\\[([^\"]+)]"
              = let plats' = concatMap BS.words (map (BS.map (\c -> if c == '+' then ' ' else c)) plats)
                    maint' = concatMap BS.words maint
                in
                  return $ Just $ Pkg (unpack name)
                                      (unpack vers)
                                      (unpack sha)
                                      (map unpack plats')
                                      (map unpack maint')
                                      (path)
  | True <- buf `regmatch` "cabal.mkDerivation"
              = msgInfo ("failed to parse file " ++ path) >> return Nothing
  | otherwise = return Nothing

updateNixPkgs :: [FilePath] -> Hackage4Nix ()
updateNixPkgs paths = do
  msgDebug $ "updating = " ++ show paths
  flip mapM_ paths $ \fileOrDir ->
    flip discoverNixFiles fileOrDir $ \file -> do
      nix' <- io (BS.readFile file) >>= parseNixFile file
      flip (maybe (return ())) nix' $ \nix -> do
        let Pkg name vers sha plats maints path = nix
        msgDebug ("re-generate " ++ path)
        pkg <- readCabalFile name vers
        io $ writeFile path (showNixPkg (cabal2nix pkg sha plats maints))

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
