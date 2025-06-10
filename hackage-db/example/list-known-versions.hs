module Main ( main ) where

import Distribution.Hackage.DB.Path
import Distribution.Hackage.DB.Unparsed

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Map as Map
import Distribution.Text

main :: IO ()
main = do
  db <- hackageTarball >>= readTarball Nothing
  forM_ (toList db) $ \(pn, PackageData vr vs) -> do
    let pref = if BS.null vr then "" else " (preferred: " ++ show vr ++ ")"
    putStrLn $ display pn ++ ": " ++ List.intercalate ", " (fmap display (keys vs)) ++ pref
