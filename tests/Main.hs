{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, finally, handle)
import Control.Applicative ((<|>))
import Control.Monad (forM_, void, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, Key, Object, Value(..), eitherDecodeFileStrict', withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
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


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (expectFail)

import Cob
import Cob.RecordM (getDefinition, updDefinition)
import Cob.RecordM.Definition
import qualified Cob.RecordM.Servant as Servant
import Cob.Utils (performReq)
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
sbClientDef =
  simpleDefinition "TEST Sandbox Clients" "Cob tests — clients"
    & field "Email"       . spec <>~ instanceLabel <> mandatory
    & field "Name"        . spec <>~ ""
    & field "Kickstarter" . spec <>~ list ["Yes", "No"]

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
    , testCase "Reordering fields and back"                $ testReorder          env
    , testCase "RecordM reads conditions as we do"         $ testConditions       env
    , testCase "RecordM derives the keys we don't send"    $ testDerivedKeys      env
    , expectFail $
      testCase "RecordM doesn't derive required"           $ testRequiredNotDerived env
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

-- | Name, description and mandatoriness of top-level fields, by order.
describeFields :: Definition -> [(Text, Text, Bool)]
describeFields = map describeField . sortOn fieldOrder . M.elems . defFieldDefinitions
  where
    describeField f = (fieldName f, descText (f ^. spec . description), f ^. spec . isMandatory)

fieldNames :: Definition -> [Text]
fieldNames = map (\(n, _, _) -> n) . describeFields

fieldIds :: Definition -> [(Text, FieldName)]
fieldIds d = [ (fieldName f, fieldId f) | f <- M.elems (defFieldDefinitions d) ]

sbClientFields :: [(Text, Text, Bool)]
sbClientFields =
  [ ("Email",       "$instanceLabel", True)
  , ("Name",        "",               False)
  , ("Kickstarter", "$[Yes,No]",      False)
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
  _ <- updDefinition did (& field "Extra" . spec <>~ text)
  after <- getDefinition did
  liftIO $ do
    describeFields after @?= sbClientFields ++ [("Extra", "$text", False)]
    assertBool "existing fields must keep their ids" $
      all (`elem` fieldIds after) (fieldIds before)

testUpdDescriptions :: TestEnv -> Assertion
testUpdDescriptions env = withDef env $ \did -> do
  _ <- updDefinition did $ \d -> d { defDescription = "Updated description" }
    & field "Name" . spec . description .~ "$text"
  d <- getDefinition did
  liftIO $ do
    defDescription d @?= "Updated description"
    describeFields d @?=
      [ ("Email",       "$instanceLabel", True)
      , ("Name",        "$text",          False)
      , ("Kickstarter", "$[Yes,No]",      False)
      ]

testRemoveField :: TestEnv -> Assertion
testRemoveField env = withDef env $ \did -> do
  _ <- updDefinition did (& field "Extra" . spec <>~ text)
  _ <- updDefinition did $ \d -> d & fieldById (fieldId (d ^. field "Extra")) .~ Nothing
  d <- getDefinition did
  liftIO $ describeFields d @?= sbClientFields

testSubfields :: TestEnv -> Assertion
testSubfields env = withDef env $ \did -> do
  _ <- updDefinition did $ \d -> d
    & field "Details" . field "Note"  . spec <>~ text
    & field "Details" . field "Score" . spec <>~ number
  d <- getDefinition did
  liftIO $ do
    fieldNames d @?= ["Email", "Name", "Kickstarter", "Details"]
    describeFields d { defFieldDefinitions = fieldFields (d ^. field "Details") } @?=
      [ ("Note",  "$text",   False)
      , ("Score", "$number", False)
      ]

testReorder :: TestEnv -> Assertion
testReorder env = withDef env $ \did -> do
  let original = ["Email", "Name", "Kickstarter"]
      reorder names = void $ updDefinition did $ \d -> d
        { defFieldDefinitions = M.map (\f -> f { fieldOrder = fromMaybe (fieldOrder f) (lookup (fieldName f) (zip names [0..])) }) (defFieldDefinitions d) }
  before <- getDefinition did
  reorder (reverse original)
  reversed <- getDefinition did
  reorder original
  after <- getDefinition did
  liftIO $ do
    fieldNames reversed @?= reverse original
    fieldNames after    @?= original
    fieldIds after      @?= fieldIds before

testConditions :: TestEnv -> Assertion
testConditions env = withDef env $ \did -> do
  _ <- updDefinition did $ \d -> foldl
    (\d' (i, c) -> d' & field "Name" . field (condField i) . spec <>~ ifCondition c) d (zip [0 ..] cases)
  v <- performReq (Servant.getDefinition did)
  liftIO $ forM_ (zip [0 ..] cases) $ \(i, c) ->
    assertEqual (T.unpack c) (Right (condExpr <$> parseCondText c)) (Just <$> visibilityOf (condField i) v)
  where
    condField i = "Cond " <> T.pack (show (i :: Int))
    cases =
      [ "Name=1 && Name=2 || Name=3", "Name=1 || Name=2 && Name=3"
      , "(Name=1 || Name=2) && Name=3", "Name=1 && (Name=2 || Name=3)"
      , "Name=1 && Name=2 && Name=3", "Name=1&&Name=2", "=Yes && Name!", "=a,b"
      , "Name>1", "Name>=1", "Name<1", "Name<=1" ]

-- | The @visibilityCondition@ RecordM derived for the field with this name
visibilityOf :: Text -> Value -> Either String CondExpr
visibilityOf n v = case [ o | o <- objects v, KM.lookup "name" o == Just (String n) ] of
  o : _ -> parseEither (visibility <=< (.: "visibilityCondition")) o
  []    -> Left ("no field " <> show n)
  where
    objects = \case
      Object o -> o : concatMap objects (KM.elems o)
      Array a  -> concatMap objects a
      _        -> []

    visibility :: Value -> Parser CondExpr
    visibility = withObject "visibilityCondition" $ \o -> o .: "type" >>= \case
      "And" -> And <$> (mapM visibility =<< o .: "conditions")
      "Or"  -> Or  <$> (mapM visibility =<< o .: "conditions")
      t     -> Compare <$> (matcher =<< o .: "matcher") <*> maybe (fail (T.unpack t)) pure (lookup t ops)
                       <*> (o .: "value" <|> (pure <$> o .: "value"))

    matcher :: Object -> Parser (Maybe Text)
    matcher m = m .: "type" >>= \case
      ("Parent" :: Text) -> pure Nothing
      _ -> Just <$> m .: "name"

    ops = [ ("Equal", Equal), ("NotEqual", NotEqual), ("HigherThan", Greater)
          , ("HigherThanOrEqual", GreaterEq), ("LowerThan", Lower), ("LowerThanOrEqual", LowerEq) ]

-- | Sending a definition without its 'derivedKeys' leaves RecordM in the same
-- state as sending it back whole, as RecordM's own UI does.
testDerivedKeys :: TestEnv -> Assertion
testDerivedKeys = sentAsWhole id

-- | 'required' isn't derived, so sending it without it must not be the same.
testRequiredNotDerived :: TestEnv -> Assertion
testRequiredNotDerived = sentAsWhole (withoutKey "required")

-- | Sending the definition as we encode it, after the given change, leaves
-- RecordM in the same state as sending it back whole.
sentAsWhole :: (Value -> Value) -> TestEnv -> Assertion
sentAsWhole change env = withDef env $ \did -> do
  _ <- updDefinition did derivedDef
  whole <- performReq (Servant.getDefinition did)
  _ <- performReq (Servant.updDefinition did whole)
  viaWhole <- performReq (Servant.getDefinition did)
  ours <- definitionValue <$> getDefinition did
  _ <- performReq (Servant.updDefinition did (change ours))
  viaOurs <- performReq (Servant.getDefinition did)
  liftIO $ do
    assertBool "every derived key must have a non-trivial value in some field" $
      all (\k -> any (`notElem` [Null, Bool False, Array mempty]) (valuesAt k viaOurs)) (filter (/= "_links") derivedKeys)
    withoutKey "version" viaOurs @?= withoutKey "version" viaWhole

-- | Remove the given key, at any depth
withoutKey :: Key -> Value -> Value
withoutKey k = \case
  Object o -> Object (KM.map (withoutKey k) (KM.delete k o))
  Array a -> Array (fmap (withoutKey k) a)
  v -> v

-- | Exercises every one of the 'derivedKeys'
derivedDef :: Definition -> Definition
derivedDef d = d
  & field "Group" . spec <>~ "$group"
  & field "Group" . field "Kind" %~ (\f -> f
      & spec <>~ listDefault ["A", "B"] "A" <> "$radio"
      & field "Only A" . spec <>~ ifCondition "=A"
      & field "Not B" . spec <>~ ifCondition "!B")
  & field "Group" . field "Other" . spec <>~ ifCondition "Kind=B"
  & field "Group" . field "Either" . spec <>~ ifCondition "(Kind=A || Kind=B) && Secret!"
  & field "Secret" . spec <>~ "$restricted(cob-tests)" <> mandatory
  & field "Items" %~ (\f -> f & spec <>~ duplicable & field "Item" . spec <>~ text)

-- | Every value of the given key, at any depth
valuesAt :: Key -> Value -> [Value]
valuesAt k = \case
  Object o -> maybe id (:) (KM.lookup k o) (concatMap (valuesAt k) (KM.elems o))
  Array a -> concatMap (valuesAt k) a
  _ -> []

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
                  runSourceToList
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
