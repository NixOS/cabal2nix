-- self-test.hs

module Main ( main ) where

import Prelude ( fmap, either )
import Language.Nix ( parseNixFile, parseNix )
import System.Environment ( getArgs )
import System.IO ( IO, getContents, hPrint, print, stderr )
import Control.Monad ( mapM_, (>>=), (>=>) )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> fmap parseNix getContents >>= either (hPrint stderr) print
    _  -> mapM_ (parseNixFile >=> either (hPrint stderr) print) args
