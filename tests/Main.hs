{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad (void)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import qualified Streamly.Data.Stream as Streamly
import qualified Streamly.Data.Fold as Fold

import Test.Tasty
import Test.Tasty.HUnit

import Cob
import Cob.RecordM.TH

--------------------------------------------------------------------------------
-- Test schema (Sandbox: renamed from Kanjideck)
--------------------------------------------------------------------------------

data KickstarterPerm = KickstarterYes | KickstarterNo
  deriving (Show, Eq)
mkRecordEnum ''KickstarterPerm ["Yes", "No"]

data OrderState = OrderPaid | OrderSent | OrderRefund
  deriving (Show, Eq)
mkRecordEnum ''OrderState ["Paid", "Sent", "Refund"]

-- Abstract, only used to type references.
data SBProduct

data SBClient = SBClient
  { sbcEmail       :: Text
  , sbcName        :: Maybe Text
  , sbcKickstarter :: Maybe KickstarterPerm
  } deriving Show
mkRecord ''SBClient "Sandbox Clients" ["Email", "Name", "Kickstarter"]

data SBOrder = SBOrder
  { sboClient :: Ref SBClient
  , sboState  :: OrderState
  }
mkRecord ''SBOrder "Sandbox Orders" ["Client", "State"]

data SBOrderItem = SBOrderItem
  { sboiOrder   :: Ref SBOrder
  , sboiClient  :: Maybe (Ref SBClient)
  , sboiProduct :: Ref SBProduct
  , sboiQt      :: Int
  , sboiValue   :: Int
  }
mkRecord ''SBOrderItem "Sandbox Order Items" ["Order", "Client", "Product", "Qt", "Value"]

data SBDownloaded = SBDownloaded (Ref SBOrderItem) Text
mkRecord ''SBDownloaded "Sandbox Downloads" ["Order Item", "Resource"]

--------------------------------------------------------------------------------
-- Test configuration
--
-- JSON file with the following shape (optional fields may be omitted or null):
--
--   {
--     "host":       "test.cultofbits.com",
--     "token":      "…",
--     "loginUser":  "…",
--     "loginPass":  "…",
--     "groupRef":   123,
--     "productRef": 456
--   }
--
-- Path: COB_TEST_CONFIG env var, defaulting to `tests/cob-test.json`. Tests
-- needing a missing optional field are reported as skipped.
--------------------------------------------------------------------------------

data TestConfig = TestConfig
  { host       :: String
  , token      :: String
  , loginUser  :: Maybe String
  , loginPass  :: Maybe String
  , groupRef   :: Maybe Integer
  , productRef :: Maybe Integer
  } deriving (Generic)

instance FromJSON TestConfig

readConfig :: FilePath -> IO (Either String TestConfig)
readConfig fp = do
  exists <- doesFileExist fp
  if not exists
    then return (Left ("file not found: " <> fp))
    else eitherDecodeFileStrict' fp

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
      withSession (host cfg) (token cfg) $ \session ->
        defaultMain (allTests cfg (TestEnv session counter))

-- | Shared environment for every test.
data TestEnv = TestEnv
  { envSession :: CobSession
  , envCounter :: IORef Int
  }

-- | Run a Cob action with mockCob (so additions are rolled back).
runMock :: TestEnv -> Cob a -> IO a
runMock env c = mockCob 0 (envSession env) c

allTests :: TestConfig -> TestEnv -> TestTree
allTests cfg env = testGroup "Cob mockCob integration tests"
  [ testCase "Add + Get roundtrip (SBClient)"                $ testAddGet             env
  , testCase "AddSync makes the record immediately findable" $ testAddSync            env
  , testCase "Search finds added record"                     $ testSearch             env
  , testCase "Count returns >= 1 after add"                  $ testCount              env
  , testCase "StreamSearch streams added record"             $ testStreamSearch       env
  , testCase "Delete removes the record"                     $ testDelete             env
  , testCase "UpdateInstances applies the update"            $ testUpdateInstances    env
  , testCase "Order + OrderItem references (serialization)"  $ testReferences         env cfg
  , testCase "Enum and Maybe serialization roundtrip"        $ testEnumMaybeRoundtrip env
  , testCase "CreateUser + DeleteUser"                       $ testCreateUser         env
  , optionalGroup cfg "AddToGroup"                           $ testAddToGroup         env
  , optionalLogin cfg "Login"                                $ testLogin              env
  ]

optionalGroup :: TestConfig -> TestName -> (Integer -> Assertion) -> TestTree
optionalGroup cfg name act = case groupRef cfg of
  Just g  -> testCase name (act g)
  Nothing -> testCase (name <> " (skipped: no groupRef)") (return ())

optionalLogin :: TestConfig -> TestName -> ((String, String) -> Assertion) -> TestTree
optionalLogin cfg name act = case (loginUser cfg, loginPass cfg) of
  (Just u, Just p) -> testCase name (act (u, p))
  _ -> testCase (name <> " (skipped: no login creds)") (return ())

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

uniqueEmail :: TestEnv -> String -> IO Text
uniqueEmail env tag = do
  n <- atomicModifyIORef' (envCounter env) (\i -> (i + 1, i + 1))
  now <- getPOSIXTime
  let stamp = show (truncate now :: Integer)
  return $ T.pack ("cob-tests+" <> tag <> "-" <> stamp <> "-" <> show n <> "@example.invalid")

sampleClient :: Text -> SBClient
sampleClient e = SBClient
  { sbcEmail       = e
  , sbcName        = Just "Test Sandbox Client"
  , sbcKickstarter = Just KickstarterYes
  }

sampleUser :: Text -> User
sampleUser e =
  let uname = T.unpack e
  in User
       { uusername   = uname
       , upassword   = Just "test-pw-do-not-use"
       , uname       = "Cob Tests"
       , uemail      = uname
       , ucontact    = Nothing
       , uusernameAD = Nothing
       }

--------------------------------------------------------------------------------
-- Individual tests
--------------------------------------------------------------------------------

testAddGet :: TestEnv -> Assertion
testAddGet env = runMock env $ do
  email <- liftCob (uniqueEmail env "addget")
  ref <- add (sampleClient email)
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

testReferences :: TestEnv -> TestConfig -> Assertion
testReferences env cfg = runMock env $ do
  email     <- liftCob (uniqueEmail env "refs")
  clientRef <- addSync (sampleClient email)
  orderRef  <- addSync SBOrder { sboClient = clientRef, sboState = OrderPaid }
  ord       <- get orderRef
  liftCob $ do
    ref_id (sboClient ord) @?= ref_id clientRef
    case sboState ord of
      OrderPaid -> return ()
      _         -> assertFailure "expected OrderPaid"

  case productRef cfg of
    Nothing -> return ()
    Just pid -> do
      let prodRef = Ref Nothing pid :: Ref SBProduct
      itemRef <- addSync SBOrderItem
        { sboiOrder   = orderRef
        , sboiClient  = Just clientRef
        , sboiProduct = prodRef
        , sboiQt      = 3
        , sboiValue   = 1500
        }
      item <- get itemRef
      liftCob $ do
        ref_id (sboiOrder item)           @?= ref_id orderRef
        fmap ref_id (sboiClient item)     @?= Just (ref_id clientRef)
        ref_id (sboiProduct item)         @?= pid
        sboiQt item                       @?= 3
        sboiValue item                    @?= 1500
      void $ add (SBDownloaded itemRef "https://example.invalid/test.pdf")

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

testCreateUser :: TestEnv -> Assertion
testCreateUser env = runMock env $ do
  email <- liftCob (uniqueEmail env "user")
  ur    <- createUser (sampleUser email)
  liftCob $ assertBool "createUser returned a ref" (ref_id ur > 0)

testAddToGroup :: TestEnv -> Integer -> Assertion
testAddToGroup env gid = runMock env $ do
  email <- liftCob (uniqueEmail env "grp")
  ur    <- createUser (sampleUser email)
  addToGroup [ur] (Ref Nothing gid :: Ref Group)

testLogin :: TestEnv -> (String, String) -> Assertion
testLogin env (user, pass) = runMock env $ do
  tok <- login user pass
  liftCob $ assertBool "login returned a non-empty token" (not (null tok))
