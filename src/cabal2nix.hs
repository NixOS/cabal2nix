module Main ( main ) where

import Cabal2Nix.Generate ( cabal2nix )
import Cabal2Nix.Normalize ( normalize )
import Cabal2Nix.Package
import Cabal2Nix.Version
import Distribution.NixOS.Derivation.Cabal hiding ( version )
import Distribution.NixOS.Fetch

import Control.Exception ( bracket )
import Control.Monad ( when )
import Data.Default.Class
import Distribution.Text ( disp )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, hFlush, stdout, stderr )

data Configuration = Configuration
  { optPrintHelp :: Bool
  , optPrintVersion :: Bool
  , optSha256 :: Maybe String
  , optMaintainer :: [String]
  , optPlatform :: [String]
  , optHaddock :: Bool
  , optDoCheck :: Bool
  , optJailbreak :: Bool
  , optRevision :: String
  , optHyperlinkSource :: Bool
  , optHackageDb :: Maybe FilePath
  }
  deriving (Show)

instance Default Configuration where
  def = Configuration
    { optPrintHelp = False
    , optPrintVersion = False
    , optSha256 = Nothing
    , optMaintainer = []
    , optPlatform = []
    , optHaddock = True
    , optDoCheck = True
    , optJailbreak = False
    , optRevision = ""
    , optHyperlinkSource = True
    , optHackageDb = Nothing
    }

options :: [OptDescr (Configuration -> Configuration)]
options =
  [ Option "h" ["help"]       (NoArg (\o -> o { optPrintHelp = True }))                                  "show this help text"
  , Option "v" ["version"]    (NoArg (\o -> o { optPrintVersion = True }))                               "print version"
  , Option ""  ["hackage-db"] (ReqArg (\x o -> o { optHackageDb = Just x }) "FILEPATH")                  "path to the local hackage db in tar format"
  , Option ""  ["sha256"]     (ReqArg (\x o -> o { optSha256 = Just x }) "HASH")                         "sha256 hash of source tarball"
  , Option "m" ["maintainer"] (ReqArg (\x o -> o { optMaintainer = x : optMaintainer o }) "MAINTAINER")  "maintainer of this package (may be specified multiple times)"
  , Option "p" ["platform"]   (ReqArg (\x o -> o { optPlatform = x : optPlatform o }) "PLATFORM")        "supported build platforms (may be specified multiple times)"
  , Option ""  ["jailbreak"]  (NoArg (\o -> o { optJailbreak = True }))                                  "don't honor version restrictions on build inputs"
  , Option ""  ["no-haddock"] (NoArg (\o -> o { optHaddock = False }))                                   "don't run Haddock when building this package"
  , Option ""  ["no-check"]   (NoArg (\o -> o { optDoCheck = False }))                                   "don't run regression test suites of this package"
  , Option ""  ["rev"]        (ReqArg (\x o -> o { optRevision = x }) "REVISION")                        "revision, only used when fetching from VCS"
  , Option ""  ["no-hyperlink-source"] (NoArg (\o -> o { optHyperlinkSource = False }))                  "don't add pretty-printed source code to the documentation"
  ]

usage :: String
usage = usageInfo "Usage: cabal2nix [options] URI" options ++ unlines
        [ ""
        , "Recognized URI schemes:"
        , ""
        , "  cabal://pkgname-pkgversion     download the specified package from Hackage"
        , "  cabal://pkgname                download latest version of the specified package from Hackage"
        , "  file:///local/path             load the Cabal file from the local disk"
        , "  /local/path                    abbreviated version of file URI"
        , "  <git/svn/bzr/hg URL>           download the source from the specified repository"
        , ""
        , "If the URI refers to a cabal file, information for building the package will be retrieved from that file, but "
        , "hackage will be used as a source for the derivation."
        , "Otherwise, the supplied URI will be used to as the source for the derivation and the information is taken"
        , "from the cabal file at the root of the downloaded source."
        ]

cmdlineError :: String -> IO a
cmdlineError ""     = hPutStrLn stderr usage >> exitFailure
cmdlineError errMsg = hPutStrLn stderr errMsg >> cmdlineError ""

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  args' <- getArgs
  (cfg,args) <- case getOpt Permute options args' of
                  (o,n,[]  ) -> return (foldl (flip ($)) def o,n)
                  (_,_,errs) -> cmdlineError (concatMap ("*** "++) errs)

  when (optPrintHelp cfg) (putStr usage >> exitSuccess)
  when (optPrintVersion cfg) (putStrLn version >> exitSuccess)
  when (length args /= 1) (cmdlineError "*** exactly one url-to-cabal-file must be specified\n")

  pkg <- getPackage (optHackageDb cfg) $ Source (head args) (optRevision cfg) (optSha256 cfg)

  let deriv  = (cabal2nix $ cabal pkg) { src = source pkg
                                       , runHaddock = optHaddock cfg
                                       , jailbreak = optJailbreak cfg
                                       , hyperlinkSource = optHyperlinkSource cfg
                                       }
      deriv' = deriv { metaSection = (metaSection deriv)
                                     { maintainers = optMaintainer cfg
                                     , platforms   = optPlatform cfg
                                     }
                     , doCheck = doCheck deriv && optDoCheck cfg
                     }

  putStr (show (disp (normalize deriv')))
