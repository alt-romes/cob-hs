{-|
   'DateTime' is the data type to use to model RecordM's datetime field type.
   For more utilities check the @time@ package's 'Data.Time'
 -}
module Cob.RecordM.DateTime
  (
    DateTime

  , millisSinceUnixEpoch
  , dateTimeFromMillis

  , dateTimeFromDay

  -- * Re-exports

  , getCurrentTime

  , parseTimeM
  , formatTime

  , addDays

  , UTCTime(..)

  ) where

import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

-- | Models a @RecordM@ field of type @$datetime@
type DateTime = UTCTime

-- | Create a 'DateTime' from a 'Day'.
--
-- === Example
--
-- @
-- let
--     parseDay d = parseTimeM True undefined "%d/%m/%Y" d :: Maybe Day
--     dateTime   = fromDay \<$\> mday :: Maybe DateTime
-- @
dateTimeFromDay :: Day -> DateTime
dateTimeFromDay d = UTCTime d 0

-- | Convert 'DateTime' to whole milliseconds since the Unix epoch.
--
-- This is how @RecordM@ represents a @$datetime@ field, and therefore what
-- 'Cob.RecordM.TH.mkRecord' writes and reads, and what a 'Query' on such a
-- field must compare against.
millisSinceUnixEpoch :: DateTime -> Integer
millisSinceUnixEpoch = truncate . (* 1000) . utcTimeToPOSIXSeconds

-- | The 'DateTime' of a number of milliseconds since the Unix epoch, the
-- inverse of 'millisSinceUnixEpoch'.
dateTimeFromMillis :: Integer -> DateTime
dateTimeFromMillis = posixSecondsToUTCTime . (/ 1000) . fromInteger
