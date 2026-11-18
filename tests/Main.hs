{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, finally, handle)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Log.FastLogger (fromLogStr)

import qualified Streamly.Data.Stream as Streamly
import qualified Streamly.Data.Fold as Fold

import Test.Tasty
import Test.Tasty.HUnit

import Cob
import Cob.RecordM (getDefinition, updDefinition)
import Cob.RecordM.Definition
import Cob.RecordM.TH

--------------------------------------------------------------------------------
-- Test schema
--------------------------------------------------------------------------------

data KickstarterPerm = KickstarterYes | KickstarterNo
  deriving (Show, Eq)
mkRecordEnum ''KickstarterPerm ["Yes", "No"]

data SBClient = SBClient
  { sbcEmail       :: Text
  , sbcName        :: Maybe Text
  , sbcKickstarter :: Maybe KickstarterPerm
  } deriving Show
mkRecord ''SBClient "TEST Sandbox Clients" ["Email", "Name", "Kickstarter"]

sbClientDef :: Definition
sbClientDef = fromDSL "TEST Sandbox Clients" "Cob tests — clients" $ do
  "Email"       |=! instanceLabel
  "Name"        |=  ""
  "Kickstarter" |=  list ["Yes", "No"]
  pure ()

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
-- Path: COB_TEST_CONFIG env var, defaulting to `tests/cob-test.json`.
--------------------------------------------------------------------------------

data TestConfig = TestConfig
  { host         :: String
  , token        :: String
  , definitionId :: Int
  } deriving (Generic)

instance FromJSON TestConfig

readConfig :: FilePath -> IO (Either String TestConfig)
readConfig fp = do
  exists <- doesFileExist fp
  if not exists
    then return (Left ("file not found: " <> fp))
    else eitherDecodeFileStrict' fp

setupDefinition :: CobSession -> DefinitionId -> IO ()
setupDefinition s did = void (runReaderT (updDefinition did setFields) s)
  where
    setFields d = d
      { defName             = defName sbClientDef
      , defDescription      = defDescription sbClientDef
      , defFieldDefinitions = defFieldDefinitions sbClientDef
      }

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  path <- fromMaybe "tests/cob-test.json" <$> lookupEnv "COB_TEST_CONFIG"
  ecfg <- readConfig path
  case ecfg of
    Left err -> do
      hPutStrLn stderr $
        "[cob-tests] No usable config at " <> path <> ": " <> err <>
        " (set COB_TEST_CONFIG or create the file). Skipping integration tests."
      defaultMain (testGroup "Tests (skipped: no config)" [])
    Right cfg -> do
      counter <- newIORef 0
      withSession (host cfg) (token cfg) $ \s -> do
        let session = s { logger = \_ -> pure () }
        setupDefinition session (DefId (definitionId cfg))
        defaultMain (allTests (TestEnv session (DefId (definitionId cfg)) counter))

-- | Shared environment for every test.
data TestEnv = TestEnv
  { envSession :: CobSession
  , envDefId   :: DefinitionId
  , envCounter :: IORef Int
  }

-- | Run an action with a session whose logs are shown only if it fails.
withLogs :: TestEnv -> (CobSession -> IO a) -> IO a
withLogs env k = do
  logs <- newIORef mempty
  let session = (envSession env) { logger = \l -> modifyIORef' logs (<> l) }
  handle (\(e :: SomeException) -> do
    out <- readIORef logs
    assertFailure (displayException e <> "\n--- logs ---\n" <> T.unpack (decodeUtf8 (fromLogStr out)))) $
    k session

-- | Run a Cob action with mockCob, so additions are rolled back.
runMock :: TestEnv -> Cob a -> IO a
runMock env c = withLogs env $ \s -> mockCob 1{-seconds-} s c

-- | Run an action on the test definition, restoring it to 'sbClientDef' afterwards.
withDef :: TestEnv -> (DefinitionId -> ReaderT CobSession IO a) -> IO a
withDef env act = withLogs env $ \s ->
  runReaderT (act (envDefId env)) s `finally` setupDefinition s (envDefId env)

allTests :: TestEnv -> TestTree
allTests env = dependentTestGroup "Cob integration tests" AllFinish
  [ dependentTestGroup "Definitions" AllFinish
    [ testCase "getDefinition returns the test definition" $ testGetDefinition    env
    , testCase "extendFromDSL appends fields, keeping ids" $ testExtendDefinition env
    , testCase "Updating descriptions"                     $ testUpdDescriptions  env
    , testCase "Removing a field"                          $ testRemoveField      env
    , testCase "Adding a field with subfields"             $ testSubfields        env
    ]
  , testGroup "Records (mockCob)" (recordTests env)
  ]

recordTests :: TestEnv -> [TestTree]
recordTests env =
  [ testCase "Add + Get roundtrip (SBClient)"                $ testAddGet             env
  , testCase "AddSync makes the record immediately findable" $ testAddSync            env
  , testCase "Search finds added record"                     $ testSearch             env
  , testCase "Count returns >= 1 after add"                  $ testCount              env
  , testCase "StreamSearch streams added record"             $ testStreamSearch       env
  , testCase "Delete removes the record"                     $ testDelete             env
  , testCase "UpdateInstances applies the update"            $ testUpdateInstances    env
  , testCase "Enum and Maybe serialization roundtrip"        $ testEnumMaybeRoundtrip env
  ]

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

uniqueEmail :: TestEnv -> String -> IO Text
uniqueEmail env tag = do
  n <- atomicModifyIORef' (envCounter env) (\i -> (i + 1, i + 1))
  now <- getPOSIXTime
  let stamp = show (truncate now :: Integer)
  return $ T.pack ("cob-tests+" <> tag <> "-" <> stamp <> "-" <> show n <> "@example.invalid")

-- | Name, description and requiredness of top-level fields, by order.
describeFields :: Definition -> [(Text, Text, FieldRequired)]
describeFields = map describeField . sortOn getFieldOrder . M.elems . defFieldDefinitions
  where
    describeField f = (fieldName f, descText (fieldDescription f), fieldRequired f)

fieldIds :: Definition -> [(Text, FieldName)]
fieldIds d = [ (fieldName f, fieldId f) | f <- M.elems (defFieldDefinitions d) ]

sbClientFields :: [(Text, Text, FieldRequired)]
sbClientFields =
  [ ("Email",       "$instanceLabel", MandatoryField)
  , ("Name",        "",               FieldNotRequired)
  , ("Kickstarter", "$[Yes,No]",      FieldNotRequired)
  ]

sampleClient :: Text -> SBClient
sampleClient e = SBClient
  { sbcEmail       = e
  , sbcName        = Just "Test Sandbox Client"
  , sbcKickstarter = Just KickstarterYes
  }

--------------------------------------------------------------------------------
-- Definition tests
--------------------------------------------------------------------------------

testGetDefinition :: TestEnv -> Assertion
testGetDefinition env = withDef env $ \did -> do
  d <- getDefinition did
  liftIO $ do
    defName d        @?= defName sbClientDef
    describeFields d @?= sbClientFields

testExtendDefinition :: TestEnv -> Assertion
testExtendDefinition env = withDef env $ \did -> do
  before <- getDefinition did
  _ <- updDefinition did (`extendFromDSL` do
    "Extra" |= text
    pure ())
  after <- getDefinition did
  liftIO $ do
    describeFields after @?= sbClientFields ++ [("Extra", "$text", FieldNotRequired)]
    assertBool "existing fields must keep their ids" $
      all (`elem` fieldIds after) (fieldIds before)

testUpdDescriptions :: TestEnv -> Assertion
testUpdDescriptions env = withDef env $ \did -> do
  let setName f = if fieldName f == "Name" then f { fieldDescription = text } else f
  _ <- updDefinition did $ \d -> d
    { defDescription      = "Updated description"
    , defFieldDefinitions = M.map setName (defFieldDefinitions d)
    }
  d <- getDefinition did
  liftIO $ do
    defDescription d @?= "Updated description"
    describeFields d @?=
      [ ("Email",       "$instanceLabel", MandatoryField)
      , ("Name",        "$text",          FieldNotRequired)
      , ("Kickstarter", "$[Yes,No]",      FieldNotRequired)
      ]

testRemoveField :: TestEnv -> Assertion
testRemoveField env = withDef env $ \did -> do
  _ <- updDefinition did (`extendFromDSL` do
    "Extra" |= text
    pure ())
  _ <- updDefinition did $ \d ->
    d { defFieldDefinitions = M.filter ((/= "Extra") . fieldName) (defFieldDefinitions d) }
  d <- getDefinition did
  liftIO $ describeFields d @?= sbClientFields

testSubfields :: TestEnv -> Assertion
testSubfields env = withDef env $ \did -> do
  _ <- updDefinition did (`extendFromDSL` do
    _ <- "Details" |= "" |+ do
      "Note"  |= text
      "Score" |= number
      pure []
    pure ())
  d <- getDefinition did
  liftIO $ do
    map (\(n, _, _) -> n) (describeFields d) @?= ["Email", "Name", "Kickstarter", "Details"]
    case filter ((== "Details") . fieldName) (M.elems (defFieldDefinitions d)) of
      [details] ->
        describeFields d { defFieldDefinitions = fieldFields details } @?=
          [ ("Note",  "$text",   FieldNotRequired)
          , ("Score", "$number", FieldNotRequired)
          ]
      _ -> assertFailure "expected exactly one Details field"

--------------------------------------------------------------------------------
-- Record tests
--------------------------------------------------------------------------------

testAddGet :: TestEnv -> Assertion
testAddGet env = runMock env $ do
  email <- liftCob (uniqueEmail env "addget")
  ref <- add (sampleClient email)
  liftCob (threadDelay 2000000)
  got <- get ref
  liftCob $ do
    sbcEmail got       @?= email
    sbcName got        @?= Just "Test Sandbox Client"
    sbcKickstarter got @?= Just KickstarterYes

testAddSync :: TestEnv -> Assertion
testAddSync env = runMock env $ do
  email <- liftCob (uniqueEmail env "addsync")
  ref <- addSync (sampleClient email)
  hits <- search ("email" =: email :: Query SBClient)
  liftCob $
    assertBool "addSync result must appear in search"
      (any (\(r, _) -> ref_id r == ref_id ref) hits)

testSearch :: TestEnv -> Assertion
testSearch env = runMock env $ do
  email <- liftCob (uniqueEmail env "search")
  _ref  <- addSync (sampleClient email)
  hits  <- search ("email" =: email :: Query SBClient)
  liftCob $ do
    assertBool "search returns at least one hit" (not (null hits))
    let emails = map (sbcEmail . snd) hits
    assertBool ("search returned wrong emails: " <> show emails)
      (email `elem` emails)

testCount :: TestEnv -> Assertion
testCount env = runMock env $ do
  email <- liftCob (uniqueEmail env "count")
  _ <- addSync (sampleClient email)
  n <- count ("email" =: email :: Query SBClient)
  liftCob $ assertBool ("count must be >= 1, got " <> show n) (n >= 1)

testStreamSearch :: TestEnv -> Assertion
testStreamSearch env = runMock env $ do
  email <- liftCob (uniqueEmail env "stream")
  _ <- addSync (sampleClient email)
  collected <- streamSearch
                  ("email" =: email :: Query SBClient)
                  (Streamly.fold Fold.toList)
  liftCob $ do
    assertBool "streamSearch yields at least one element" (not (null collected))
    let emails = map (sbcEmail . snd) collected
    assertBool ("streamSearch yielded wrong emails: " <> show emails)
      (email `elem` emails)

testDelete :: TestEnv -> Assertion
testDelete env = runMock env $ do
  email <- liftCob (uniqueEmail env "delete")
  ref <- addSync (sampleClient email)
  delete ref
  liftCob (threadDelay 2000000)
  hits <- search (byRef ref :: Query SBClient)
  liftCob $
    assertBool "deleted record must not be found"
      (not (any (\(r, _) -> ref_id r == ref_id ref) hits))

testUpdateInstances :: TestEnv -> Assertion
testUpdateInstances env = runMock env $ do
  email <- liftCob (uniqueEmail env "update")
  _ <- addSync (sampleClient email)
  let bumpName c = c { sbcName = Just "Updated Name" }
  updated <- updateInstances ("email" =: email :: Query SBClient) bumpName
  liftCob $ do
    assertBool "updateInstances returned no rows" (not (null updated))
    mapM_ (\(_, c) -> sbcName c @?= Just "Updated Name") updated


testEnumMaybeRoundtrip :: TestEnv -> Assertion
testEnumMaybeRoundtrip env = runMock env $ do
  email1 <- liftCob (uniqueEmail env "enum-nothing")
  let c1 = SBClient { sbcEmail = email1, sbcName = Nothing, sbcKickstarter = Nothing }
  r1 <- addSync c1
  g1 <- get r1
  liftCob $ do
    sbcEmail g1       @?= email1
    sbcName g1        @?= Nothing
    sbcKickstarter g1 @?= Nothing

  email2 <- liftCob (uniqueEmail env "enum-no")
  let c2 = SBClient { sbcEmail = email2, sbcName = Just "Anon", sbcKickstarter = Just KickstarterNo }
  r2 <- addSync c2
  g2 <- get r2
  liftCob $ sbcKickstarter g2 @?= Just KickstarterNo
