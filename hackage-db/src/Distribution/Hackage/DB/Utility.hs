{- |
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable
 -}

module Distribution.Hackage.DB.Utility where

import Distribution.Hackage.DB.Errors

import Control.Exception
import Codec.Archive.Tar.Entry as Tar
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Distribution.Parsec

parseText :: Parsec a => String -> String -> a
parseText t x = fromMaybe (throw (InvalidRepresentationOfType t x)) (simpleParsec x)

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

parseIso8601 :: Monad m => String -> m UTCTime
parseIso8601 = parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
