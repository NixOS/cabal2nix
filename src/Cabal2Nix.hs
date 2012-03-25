module Main ( main ) where

import Cabal2Nix.Hackage ( hashPackage, readCabalFile )
import Cabal2Nix.Generate ( cabal2nix )
import Cabal2Nix.Normalize ( normalize )
import Distribution.NixOS.Derivation.Cabal

import Control.Exception ( bracket )
import Control.Monad ( when )
import Distribution.PackageDescription ( package, packageDescription )
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.Text ( disp )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, hFlush, stdout, stderr )

data Configuration = Configuration
  { optPrintHelp :: Bool
  , optPrintVersion :: Bool
  , optSha256 :: String
  , optMaintainer :: [String]
  , optPlatform :: [String]
  , optHaddock :: Bool
  }
  deriving (Show)

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { optPrintHelp = False
  , optPrintVersion = False
  , optSha256 = ""
  , optMaintainer = []
  , optPlatform = []
  , optHaddock = True
  }

options :: [OptDescr (Configuration -> Configuration)]
options =
  [ Option ['h'] ["help"]       (NoArg (\o -> o { optPrintHelp = True }))                                  "show this help text"
  , Option []    ["sha256"]     (ReqArg (\x o -> o { optSha256 = x }) "HASH")                              "sha256 hash of source tarball"
  , Option ['m'] ["maintainer"] (ReqArg (\x o -> o { optMaintainer = x : optMaintainer o }) "MAINTAINER")  "maintainer of this package (may be specified multiple times)"
  , Option ['p'] ["platform"]   (ReqArg (\x o -> o { optPlatform = x : optPlatform o }) "PLATFORM")        "supported build platforms (may be specified multiple times)"
  , Option []    ["no-haddock"] (NoArg (\o -> o { optHaddock = False }))                                   "don't run Haddock when building this package"
  ]

usage :: String
usage = usageInfo "Usage: cabal2nix [options] url-to-cabal-file" options ++ unlines
        [ ""
        , "Recognized URI schemes:"
        , ""
        , "  cabal://pkgname-pkgversion       download the specified package from Hackage"
        , "  http://host/path                 fetch the Cabal file via HTTP"
        , "  file:///local/path               load the Cabal file from the local disk"
        , "  /local/path                      abbreviated version of file URI"
        ]

cmdlineError :: String -> IO a
cmdlineError ""     = hPutStrLn stderr usage >> exitFailure
cmdlineError errMsg = hPutStrLn stderr errMsg >> cmdlineError ""

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  args' <- getArgs
  (cfg,args) <- case getOpt Permute options args' of
                  (o,n,[]  ) -> return (foldl (flip ($)) defaultConfiguration o,n)
                  (_,_,errs) -> cmdlineError (concatMap ("*** "++) errs)

  when (optPrintHelp cfg) (putStr usage >> exitSuccess)
  when (length args /= 1) (cmdlineError "*** exactly one url-to-cabal-file must be specified\n")

  cabal' <- fmap parsePackageDescription (readCabalFile (head args))
  cabal <- case cabal' of
             ParseOk _ a -> return a
             ParseFailed err -> do
               hPutStrLn stderr ("*** cannot parse cabal file: " ++ show err)
               exitFailure

  let packageId = package (packageDescription cabal)
  sha <- if null (optSha256 cfg) then hashPackage packageId else return (optSha256 cfg)

  let deriv  = (cabal2nix cabal) { sha256 = sha, runHaddock = optHaddock cfg }
      deriv' = deriv { metaSection = (metaSection deriv)
                                     { maintainers = optMaintainer cfg
                                     , platforms   = optPlatform cfg
                                     }
                     }

  putStr (show (disp (normalize (deriv'))))
