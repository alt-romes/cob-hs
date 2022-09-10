{-|
   'DateTime' is the data type to use to model RecordM's datetime field type.
   For more utilities check the @time@ package's 'Data.Time'
 -}
module Cob.RecordM.DateTime
  (
    DateTime

  , secondsSinceUnixEpoch

  , dateTimeFromDay

  -- * Re-exports

  , getCurrentTime

  , parseTimeM
  , formatTime

  , addDays

  , UTCTime(..)

  ) where

import Data.Time

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

-- | Convert 'DateTime' to number of whole seconds since the Unix epoch. For
-- times before the Unix epoch, this is a negative number.
secondsSinceUnixEpoch :: DateTime -> Integer
secondsSinceUnixEpoch = read . formatTime undefined "%s"
