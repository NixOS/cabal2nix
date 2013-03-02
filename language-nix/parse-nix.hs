-- self-test.hs

module Main ( main ) where

import Language.Nix ( parseNixFile )
import System.Environment ( getArgs )
import System.IO ( hPrint, stderr )
import Control.Monad ( (>=>) )

main :: IO ()
main = getArgs >>= mapM_ (parseNixFile >=> either (hPrint stderr) print)
