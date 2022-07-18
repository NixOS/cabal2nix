module Main ( main ) where

import Distribution.Hackage.DB.Path
import Distribution.Hackage.DB.Unparsed
import Distribution.Hackage.DB.Utility

import Control.Monad
import Data.ByteString.UTF8 as BS
import Data.Map as Map
import Distribution.Package
import System.Environment

main :: IO ()
main = do
  packageIds <- getArgs
  db <- hackageTarball >>= readTarball Nothing
  forM_ packageIds $ \pid -> do
    let PackageIdentifier pn v = parseText "PackageIdentifier" pid
    putStrLn $ maybe ("*** unknown package identifier on Hackage: " ++ show pid)
                     (toString . metaFile)
                     (Map.lookup pn db >>= Map.lookup v . versions)
