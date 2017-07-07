{- |
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable
 -}

module Distribution.Hackage.DB.Utility where

import Codec.Archive.Tar.Entry as Tar
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Distribution.Text

parseText :: Text a => String -> String -> a
parseText tname x = fromMaybe (error ("invalid " ++ tname ++ ": " ++ show x )) (simpleParse x)

-- prop_TimeConversionWorks :: EpochTime -> Bool
-- prop_TimeConversionWorks et = toEpochTime (fromEpochTime et) == et

-- parseUTCTime :: String -> UTCTime
-- parseUTCTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

fromEpochTime :: EpochTime -> UTCTime
fromEpochTime et = posixSecondsToUTCTime (realToFrac et)

toEpochTime :: UTCTime -> EpochTime
toEpochTime = floor . utcTimeToPOSIXSeconds
