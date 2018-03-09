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
import Distribution.Text

parseText :: Text a => String -> String -> a
parseText t x = fromMaybe (throw (InvalidRepresentationOfType t x)) (simpleParse x)

-- | Convert the the 'EpochTime' used by the @tar@ library into a standard
-- 'UTCTime' type.

fromEpochTime :: EpochTime -> UTCTime
fromEpochTime et = posixSecondsToUTCTime (realToFrac et)

-- | Convert the standard 'UTCTime' type into the 'EpochTime' used by the @tar@
-- library.

toEpochTime :: UTCTime -> EpochTime
toEpochTime = floor . utcTimeToPOSIXSeconds
