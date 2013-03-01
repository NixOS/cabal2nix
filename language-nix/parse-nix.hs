-- self-test.hs

module Main ( main ) where

import Language.Nix ( parseNixFile )
import System.Environment ( getArgs )
import System.IO ( hPrint, stderr )
import System.Exit ( exitFailure )

main :: IO ()
main = getArgs >>= mapM_ parseAndPrint
  where
    parseAndPrint file = do r <- parseNixFile file
                            case r of
                              Left err   -> hPrint stderr err
                              Right expr -> print expr
