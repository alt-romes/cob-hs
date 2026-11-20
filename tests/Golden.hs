{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Golden tests pinning the JSON we send to CoB.
--
-- Each request is encoded and compared against @tests/golden/requests/<name>.json@.
-- Missing golden files are created on the first run; @--accept@ overwrites them.
module Main (main) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty')
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.Text (Text)
import System.FilePath ((</>), (<.>))

import Test.Tasty
import Test.Tasty.Golden

import Cob.Ref
import Cob.RecordM.DateTime
import Cob.RecordM.Definition
import Cob.RecordM.Query (Query)
import Cob.RecordM.Record
import Cob.RecordM.Servant (AddSpec(..), DeleteSpec(..), UpdateSpec(..))
import Cob.RecordM.TH
import Cob.UserM.Entities
import Cob.UserM.Servant (LoginData(..))

--------------------------------------------------------------------------------
-- Codecs under test
--
-- These are the only definitions that should change when the instances move.
--------------------------------------------------------------------------------

encRecord :: Record a => a -> BSL.ByteString
encRecord = encodeRecord

encAddSpec :: Record a => AddSpec a -> BSL.ByteString
encAddSpec = encode

encUpdateSpec :: Record a => UpdateSpec a -> BSL.ByteString
encUpdateSpec = encode

encDeleteSpec :: DeleteSpec a -> BSL.ByteString
encDeleteSpec = encode

encDefinition :: Definition -> BSL.ByteString
encDefinition = encode . definitionValue

encUser :: User -> BSL.ByteString
encUser = encode

encGroupUsers :: [Integer] -> BSL.ByteString
encGroupUsers = encode

encLogin :: LoginData -> BSL.ByteString
encLogin = encode

--------------------------------------------------------------------------------
-- Schema
--------------------------------------------------------------------------------

type Note = Text

data Owner

data Status = Pending | Confirmed
  deriving Show
mkRecordEnum ''Status ["Pending Status", "Confirmed Status"]

data Kitchen = Kitchen
  { kId       :: Ref Kitchen
  , kName     :: Text
  , kCode     :: String
  , kNote     :: Note
  , kOwner    :: Ref Owner
  , kCount    :: Int
  , kPrice    :: Double
  , kWhen     :: DateTime
  , kStatus   :: Status
  , kNickname :: Maybe Text
  , kAge      :: Maybe Int
  , kBuddy    :: Maybe (Ref Owner)
  , kMood     :: Maybe Status
  } deriving Show
mkRecord ''Kitchen "GOLDEN Kitchen"
  ["ID", "Name", "Code", "Note", "Owner", "Count", "Price", "When", "Status", "Nickname", "Age", "Buddy", "Mood"]

kitchen :: Kitchen
kitchen = Kitchen
  { kId       = Ref 42
  , kName     = "Kitchen"
  , kCode     = "K-1"
  , kNote     = "Some \"quoted\" note, with ünïcode"
  , kOwner    = 7
  , kCount    = 3
  , kPrice    = 12.5
  , kWhen     = dateTimeFromMillis 1700000000123
  , kStatus   = Pending
  , kNickname = Just "Nick"
  , kAge      = Just 9
  , kBuddy    = Just 8
  , kMood     = Just Confirmed
  }

kitchenNoOptionals :: Kitchen
kitchenNoOptionals = kitchen
  { kPrice    = 3
  , kNickname = Nothing
  , kAge      = Nothing
  , kBuddy    = Nothing
  , kMood     = Nothing
  }

kitchenDef :: Definition
kitchenDef = fromDSL "GOLDEN Kitchen" "A definition" $ do
  name <- "Name" |=! instanceLabel
  _ <- "Tags" |=* list ["A", "B"]
  _ <- "Owner" |= dollarRef "GOLDEN Owners" ("name:x" :: Query ())
  _ <- "Both" |=!* (number <> readOnly <> "Some text")
  _ <- "Details" |= "Some details" |+ do
    _ <- "Note" |= text
    _ <- "Score" |= number
    pure []
  _ <- name === "Yes" ? do
    _ <- "Shown if yes" |= ""
    pure []
  pure ()

-- | 'kitchenDef' as if it already existed upstream, with its top-level fields reversed.
kitchenDefExisting :: Definition
kitchenDefExisting = kitchenDef
  { defId               = Just (DefId 77)
  , defVersion          = Just 5
  , defFieldDefinitions = M.map (\f -> f { fieldOrder = Just (100 - getFieldOrder f) }) (defFieldDefinitions kitchenDef)
  }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "CoB JSON golden tests"
    [ request "record-values"                 (encRecord kitchen)
    , request "record-values-no-optionals"    (encRecord kitchenNoOptionals)
    , request "dynrecord-values"              (withDynRecord (DynRecord "GOLDEN Kitchen") (encRecord (DynRecord "GOLDEN Kitchen")))
    , request "integration-add"               (encAddSpec (AddSpec "GOLDEN Kitchen" kitchen True))
    , request "integration-update"            (encUpdateSpec (UpdateSpec (Ref 42) (Versioned kitchenNoOptionals 3)))
    , request "integration-delete"            (encDeleteSpec (DeleteSpec @Kitchen "GOLDEN Kitchen" "id:42" True))
    , request "definition-new"                (encDefinition kitchenDef)
    , request "definition-update"             (encDefinition kitchenDefExisting)
    , request "userm-create-user"             (encUser (User "jdoe" (Just "secret") "John Doe" "jdoe@example.invalid" Nothing (Just "JDOE")))
    , request "userm-group-users"             (encGroupUsers [1, 2])
    , request "userm-login"                   (encLogin (LoginData "jdoe" "secret"))
    ]

requests :: FilePath
requests = "tests/golden/requests"

request :: String -> BSL.ByteString -> TestTree
request name enc = goldenVsString name (requests </> name <.> "json") (pure (normalise enc))
  where
    normalise bs = maybe bs ((<> "\n") . encodePretty' defConfig { confCompare = compare }) (decode @Value bs)
