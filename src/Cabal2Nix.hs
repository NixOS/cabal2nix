module Main ( main ) where

import System.IO ( hPutStrLn, hFlush, stdout, stderr )
import System.Environment ( getArgs )
import Control.Exception ( bracket )
import System.Exit ( exitFailure )
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.PackageDescription ( package, packageDescription )
import Distribution.Text ( simpleParse )
import Data.List ( isPrefixOf )
import Control.Monad ( when )
import Network.HTTP ( simpleHTTP, getRequest, getResponseBody )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )

import Cabal2Nix.Package ( showNixPkg, cabal2nix )
import Cabal2Nix.Hackage ( hackagePath, Ext(..), hashPackage )

readCabalFile :: FilePath -> IO String
readCabalFile path
  | "cabal://" `isPrefixOf` path = let Just pid = simpleParse (drop 8 path) in readCabalFile (hackagePath pid Cabal)
  | "http://"  `isPrefixOf` path = simpleHTTP (getRequest path) >>= getResponseBody
  | "file://"  `isPrefixOf` path = readCabalFile (drop 7 path)
  | otherwise                    = readFile path

data CliOption = PrintHelp | SHA256 String | Maintainer String | Platform String
  deriving (Eq)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  let options :: [OptDescr CliOption]
      options =
        [ Option ['h'] ["help"]       (NoArg PrintHelp)                 "show this help text"
        , Option []    ["sha256"]     (ReqArg SHA256 "HASH")            "sha256 hash of source tarball"
        , Option []    ["maintainer"] (ReqArg Maintainer "MAINTAINER")  "maintainer of this package (may be specified multiple times)"
        , Option []    ["platform"]   (ReqArg Platform "PLATFORM")      "supported build platforms (may be specified multiple times)"
        ]

      usage :: String
      usage = usageInfo "Usage: cabal2nix [options] url-to-cabal-file" options ++
             "\n\
             \Recognized URI schemes:\n\
             \\n\
             \  cabal://pkgname-pkgversion       download the specified package from Hackage\n\
             \  http://host/path                 fetch the Cabel file via HTTP\n\
             \  file:///local/path               load the Cabal file from the local disk\n\
             \  /local/path                      abbreviated version of file URI\n"

      cmdlineError :: String -> IO a
      cmdlineError ""     = hPutStrLn stderr usage >> exitFailure
      cmdlineError errMsg = hPutStrLn stderr errMsg >> cmdlineError ""

  args' <- getArgs
  (opts,args) <- case getOpt Permute options args' of
     (o,n,[]  ) -> return (o,n)
     (_,_,errs) -> cmdlineError (concatMap (\e -> '*':'*':'*':' ':e) errs)

  when (PrintHelp `elem` opts) (cmdlineError "")

  let uri         = args
      hash        = [ h | SHA256 h <- opts ]
      maintainers = [ m | Maintainer m <- opts ]
      platforms'  = [ p | Platform p <- opts ]
      platforms
        | null platforms' = if not (null maintainers) then ["haskellPlatforms"] else []
        | otherwise       = platforms'

  when (length uri /= 1) (cmdlineError "*** exactly one URI must be specified")
  when (length hash > 1) (cmdlineError "*** the --sha256 option may be specified only once")

  cabal' <- fmap parsePackageDescription (readCabalFile (head uri))
  cabal <- case cabal' of
             ParseOk _ a -> return a
             ParseFailed err -> do
               hPutStrLn stderr ("*** cannot parse cabal file: " ++ show err)
               exitFailure

  let packageId = package (packageDescription cabal)

  sha256 <- if null hash then hashPackage packageId else return (head hash)

  putStr (showNixPkg (cabal2nix cabal sha256 platforms maintainers))
