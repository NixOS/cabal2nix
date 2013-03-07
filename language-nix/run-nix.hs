-- run-nix.hs

module Main ( main ) where

import Prelude ( fmap, either, (.) )
import Language.Nix ( run )
import System.Environment ( getArgs )
import System.IO ( IO, getContents, hPrint, print, stderr, readFile )
import Control.Monad ( mapM_, (>>=), (>=>) )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> fmap run getContents >>= either (hPrint stderr) print
    _  -> mapM_ (readFile >=> (either (hPrint stderr) print) . run) args
