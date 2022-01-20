{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson
import Control.Monad
import Data.Text
import Cob
import Cob.RecordM
import Cob.RecordM.TH


type DogName = String
type Name    = String

newtype Owner = Owner Name
data    Dog   = Dog (Ref Owner) DogName deriving (Show)

mkRecord ''Owner "Owners" ["Owner"]
mkRecord ''Dog   "Dogs"   ["Owner", "Dog"]

logic :: Cob [(Ref Dog, Dog)]
logic = do
    dogs <- rmDefinitionSearch "bobby"
    id <- rmAddInstance (Owner "David")
    rmAddInstance (Dog id "deus")
    return dogs



main :: IO ()
main = do
    session <- makeSession "mimes8.cultofbits.com" "0UAU9pEG7DASG3w41f8/6/Ez8oiG2GSIhBiHErhb5CGDBMuCf74uDnVZe/ACZyLL/eZKYIuK3x7FIbX6zlzXaNlD8kDkb4tkV6Ucf8mEZrpMGBmuz5whCobgr4rOwS+w/dOHqxe8AvObZMH2DvdUYA=="
    dogs <- runCob session logic
    print dogs
    forM_ dogs print















-- data Dog = Dog String String

-- $(mkRecord ''Dog "Dogs" ["Owner Name", "Dog Name"])

-- type Amount = Int
-- data Exercise = Pushups | Abs | Squats | Kilometers | Unknown Text deriving (Show)
-- type Owner = Text
-- type ActivationCode = Text
-- type MasterUsername = Text
-- type ServerIdentifier = Text
-- type ServerUsername = Text

-- newtype UsersRecord = UsersRecord { _masterUsername :: MasterUsername }

-- data ServersRecord = ServersRecord { _serverIdentifier :: ServerIdentifier
--                                    , _activationCode   :: ActivationCode
--                                    , _owner            :: Owner }
--                                    deriving (Show)

-- data ServerUsersRecord = ServerUsersRecord { _userId         :: Ref UsersRecord
--                                            , _serverId       :: Ref ServersRecord
--                                            , _serverUsername :: ServerUsername }
--                                            deriving (Show)

-- data ExercisesRecord = ExercisesRecord { _serverUserId :: Ref ServerUsersRecord
--                                        , _amount       :: Amount
--                                        , _exercise     :: Exercise }


-- main :: IO ()
-- main = putStrLn "Hi2"

-- instance ToJSON Exercise where
--     toJSON = toJSON . toLower . pack . show
-- instance FromJSON Exercise where
--     parseJSON = withText "Exercise" $ \case 
--           "pushups" -> return Pushups
--           "abs" -> return Abs
--           "squats" -> return Squats
--           "kilometers" -> return Kilometers
--           _ -> fail "Error parsing exercise from JSON"


-- mkRecord ''UsersRecord       "ROMES Pushups Users"        ["Master Username"]
-- mkRecord ''ServersRecord     "ROMES Pushups Servers"      ["Server"         ,  "Activation Code",  "Owner"]
-- mkRecord ''ServerUsersRecord "ROMES Pushups Server Users" ["Master Username",  "Server"         ,  "Server Username"]
-- mkRecord ''ExercisesRecord   "ROMES Pushups Exercises"    ["Master Username",  "Amount"         ,  "Exercise Type"]
--
