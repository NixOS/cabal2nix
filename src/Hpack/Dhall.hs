{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
module Hpack.Dhall where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Aeson
import qualified Data.Text.IO as T
import qualified Dhall.Parser
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Dhall.JSON

-- SEE: https://github.com/sol/hpack-dhall/blob/master/src/Hpack/Dhall.hs
decodeDhall :: FilePath -> IO (Either String ([String], Value))
decodeDhall file = runExceptT $ do
  expr <- readInput >>= parseExpr >>= liftIO . Dhall.Import.load
  _ <- liftResult $ Dhall.TypeCheck.typeOf expr
  -- NOTE: dhallToJSON returns no warnings hence the empty list of warnings.
  liftResult $ ([],) <$> Dhall.JSON.dhallToJSON expr
  where
    readInput = liftIO (T.readFile file)
    parseExpr = liftResult . Dhall.Parser.exprFromText file
    liftResult = ExceptT . return . first show
