{- |
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable
 -}

module Distribution.Hackage.DB.Utility
  ( parseText
  , parseBS
  , fromEpochTime
  , toEpochTime
  , parseIso8601
  ) where

import Distribution.Hackage.DB.Errors

import Codec.Archive.Tar.Entry as Tar
import Control.Exception
import qualified Data.ByteString as BSS
import Data.Char
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Distribution.Parsec

parseText :: Parsec a => String -> String -> a
parseText t x = fromMaybe (throw (InvalidRepresentationOfType t x)) (simpleParsec x)

parseBS :: Parsec a => String -> BSS.ByteString -> a
parseBS t x = fromMaybe (throw (InvalidRepresentationOfType t (bsToStringLossy x))) (simpleParsecBS x)

bsToStringLossy :: BSS.ByteString -> String
bsToStringLossy = map asciiToChar . BSS.unpack
  where asciiToChar w =
          case fromIntegral w of
            i | i >= 0 && i < 128 -> chr i
            _ -> '\xfffd' -- REPLACEMENT CHARACTER: �

-- | Convert the the 'EpochTime' used by the @tar@ library into a standard
-- 'UTCTime' type.

fromEpochTime :: EpochTime -> UTCTime
fromEpochTime et = posixSecondsToUTCTime (realToFrac et)

-- | Convert the standard 'UTCTime' type into the 'EpochTime' used by the @tar@
-- library.

toEpochTime :: UTCTime -> EpochTime
toEpochTime = floor . utcTimeToPOSIXSeconds

-- | Parse an UTC timestamp in extended ISO8601 format a standard 'UTCTime'
-- type. This function is useful to parse the "snapshot" identifier printed by
-- @cabal-install@ after a database update into a useable type. Combine with
-- 'toEpochTime' to obtain an 'EpochTime' that can be passed to the Hackage DB
-- reading code from this library.
--
-- >>> parseIso8601 "2018-12-21T13:17:40Z"
-- 2018-12-21 13:17:40 UTC

parseIso8601 :: MonadFail m => String -> m UTCTime
parseIso8601 = iso8601ParseM
