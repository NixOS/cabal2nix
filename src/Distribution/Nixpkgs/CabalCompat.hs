{-# LANGUAGE CPP #-}

module Distribution.Nixpkgs.CabalCompat
  ( unFlagAssignment
  , mkFlagAssignment
  , parseGenericPackageDescription
  ) where

-- Parser
import qualified Data.ByteString.Char8 as BS
#if MIN_VERSION_Cabal(2,2,0)
import qualified Distribution.PackageDescription.Parsec as Cabal
#else
import qualified Distribution.PackageDescription.Parser as Cabal
import Data.String.UTF8 ( toString, fromRep )
#endif

-- FlagAssignment
import Distribution.PackageDescription ( GenericPackageDescription )
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription ( mkFlagAssignment, unFlagAssignment )
#else
import Distribution.PackageDescription ( FlagAssignment )
#endif

#if !MIN_VERSION_Cabal(2,2,0)
unFlagAssignment :: FlagAssignment -> [(FlagName, Bool)]
unFlagAssignment = id

mkFlagAssignment :: [(FlagName, Bool)] -> FlagAssignment
mkFlagAssignment = id
#endif

parseGenericPackageDescription :: BS.ByteString -> Either String GenericPackageDescription
#if MIN_VERSION_Cabal(2,2,0)
parseGenericPackageDescription bs =
    case Cabal.runParseResult $ Cabal.parseGenericPackageDescription bs of
      (_, Left (_, errs)) -> Left $ show errs
      (_, Right x)        -> Right x
#else
parseGenericPackageDescription bs =
    case Cabal.parseGenericPackageDescription (decodeUTF8 bs) of
      (ParseFailed err) -> Left err
      (ParseOk _ x)     -> Right x
  where
    decodeUTF8 :: ByteString -> String
    decodeUTF8 = toString . fromRep
#endif
