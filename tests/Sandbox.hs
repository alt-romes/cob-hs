{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The sandbox definition the integration tests run against, and its configuration
module Sandbox
  ( TestConfig(..), configPath, readConfig
  , sbClientDef, sbClientFields, setupDefinition
  , describeFields, fieldNames, fieldIds
  ) where

import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

import Cob
import Cob.RecordM (updDefinition)
import Cob.RecordM.Definition

--------------------------------------------------------------------------------
-- Test configuration
--
-- JSON file with the following shape:
--
--   {
--     "host":         "test.cultofbits.com",
--     "token":        "…",
--     "definitionId": 123
--   }
--
-- The definition is overwritten with 'sbClientDef' before the tests run, and
-- the definition tests modify it further (restoring it afterwards), so the
-- token only needs permission to modify (and add instances to) it.
--
-- In fact, the token should ONLY have permissions for that definition
-- including `definition:update:<id>` to modify it.
--
-- Path: COB_TEST_CONFIG env var, defaulting to `tests/cob-test.json`.
--------------------------------------------------------------------------------

data TestConfig = TestConfig
  { host         :: String
  , token        :: String
  , definitionId :: Int
  } deriving (Generic)

instance FromJSON TestConfig

configPath :: IO FilePath
configPath = fromMaybe "tests/cob-test.json" <$> lookupEnv "COB_TEST_CONFIG"

readConfig :: FilePath -> IO (Either String TestConfig)
readConfig fp = do
  exists <- doesFileExist fp
  if not exists
    then return (Left ("file not found: " <> fp))
    else eitherDecodeFileStrict' fp

--------------------------------------------------------------------------------
-- Test schema
--------------------------------------------------------------------------------

sbClientDef :: Definition
sbClientDef =
  simpleDefinition "TEST Sandbox Clients" "Cob tests — clients"
    & field "Email"       . spec <>~ instanceLabel <> mandatory
    & field "Name"        . spec <>~ ""
    & field "Kickstarter" . spec <>~ list ["Yes", "No"]

-- | 'describeFields' of 'sbClientDef'
sbClientFields :: [(Text, Text, Bool)]
sbClientFields =
  [ ("Email",       "$instanceLabel", True)
  , ("Name",        "",               False)
  , ("Kickstarter", "$[Yes,No]",      False)
  ]

-- | Overwrite the definition with 'sbClientDef'
setupDefinition :: CobSession -> DefinitionId -> IO ()
setupDefinition s did = void (runReaderT (updDefinition did setFields) s)
  where
    setFields d = d
      { defName             = defName sbClientDef
      , defDescription      = defDescription sbClientDef
      , defFieldDefinitions = defFieldDefinitions sbClientDef
      }

--------------------------------------------------------------------------------
-- Inspecting definitions
--------------------------------------------------------------------------------

-- | Name, description and mandatoriness of top-level fields, by order.
describeFields :: Definition -> [(Text, Text, Bool)]
describeFields = map describeField . sortOn fieldOrder . M.elems . defFieldDefinitions
  where
    describeField f = (fieldName f, descText (f ^. spec . description), f ^. spec . isMandatory)

fieldNames :: Definition -> [Text]
fieldNames = map (\(n, _, _) -> n) . describeFields

fieldIds :: Definition -> [(Text, FieldName)]
fieldIds d = [ (fieldName f, fieldId f) | f <- M.elems (defFieldDefinitions d) ]
