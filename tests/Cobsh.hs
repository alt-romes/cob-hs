{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Drive the @cobsh@ executable against the sandbox definition
module Main (main) where

import Control.Exception (finally)
import Control.Monad.Reader (runReaderT)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import Test.Tasty hiding (after)
import Test.Tasty.HUnit

import Cob
import Cob.RecordM (getDefinition)
import Cob.RecordM.Definition

import Sandbox

data Env = Env
  { envConfig  :: FilePath
  , envSession :: CobSession
  , envDefId   :: DefinitionId
  }

main :: IO ()
main = do
  path <- configPath
  readConfig path >>= \case
    Left err -> do
      hPutStrLn stderr $ "[cobsh-tests] No usable config at " <> path <> ": " <> err <> ". Skipping."
      defaultMain (testGroup "cobsh (skipped: no config)" [])
    Right cfg -> withSession (host cfg) (token cfg) $ \s -> do
      let env = Env path s{logger = \_ -> pure ()} (DefId (definitionId cfg))
      setupDefinition (envSession env) (envDefId env)
      defaultMain (tests env)

tests :: Env -> TestTree
tests env = dependentTestGroup "cobsh" AllFinish $ map (\(n, t) -> testCase n (restoring env (t env)))
  [ ("info lists the fields",                    testInfo)
  , ("field shows a field",                      testShowField)
  , ("unknown fields are an error",              testUnknownField)
  , ("name and desc",                            testNameDesc)
  , ("new adds a field",                         testNew)
  , ("new refuses an existing name",             testNewExisting)
  , ("field modifies keywords and flags",        testModifyField)
  , ("field --desc keeps the keywords",          testFieldDesc)
  , ("a no-op changes nothing",                  testNoop)
  , ("delete asks for confirmation",             testDeleteAborted)
  , ("delete removes the field",                 testDelete)
  , ("undo reverts changes in reverse order",    testUndo)
  , ("undo restores a deleted field",            testUndoDelete)
  , ("history lists the changes",                testHistory)
  , ("lines lists the fields with their ids",    testLines)
  , ("apply of the same lines changes nothing",  testApplyUnchanged)
  , ("apply --dry-run changes nothing",          testApplyDryRun)
  , ("apply edits, adds and deletes fields",     testApplyEdits)
  , ("apply reorders fields",                    testApplyReorder)
  , ("apply refuses moving to another parent",   testApplyMove)
  ]

--------------------------------------------------------------------------------
-- Running cobsh
--------------------------------------------------------------------------------

-- | Run one command on the test definition, answering prompts with the input
cobsh :: Env -> [String] -> String -> IO (ExitCode, String, String)
cobsh env args = readProcessWithExitCode "cobsh" (["-c", envConfig env, "-d", show (envDefId env)] ++ args)

-- | Run one command which must succeed, returning its output
cobsh_ :: Env -> [String] -> IO String
cobsh_ env args = do
  (code, out, err) <- cobsh env args "yes\n"
  assertEqual ("cobsh " ++ unwords args ++ " failed: " ++ err) ExitSuccess code
  pure out

-- | Run these lines in the interactive shell, on the test definition
repl :: Env -> [String] -> IO String
repl env ls = do
  (code, out, err) <- readProcessWithExitCode "cobsh" ["-c", envConfig env]
    (unlines (("-d " ++ show (envDefId env)) : ls))
  assertEqual "cobsh shell failed" ExitSuccess code
  assertBool ("cobsh shell errors: " ++ err) (null err)
  pure out

-- | The definition's fields as lines to edit
fieldLines :: Env -> IO [String]
fieldLines env = lines <$> cobsh_ env ["lines"]

-- | Make the fields those of the lines, as the vim plugin does
apply :: Env -> [String] -> [String] -> IO (ExitCode, String, String)
apply env args ls = cobsh env ("apply" : args) (unlines ls)

-- | The line without its id
withoutId :: String -> String
withoutId = drop 1 . dropWhile (/= ' ')

current :: Env -> IO Definition
current env = runReaderT (getDefinition (envDefId env)) (envSession env)

restoring :: Env -> IO a -> IO a
restoring env k = k `finally` setupDefinition (envSession env) (envDefId env)

-- | Run these commands, which must have these resulting fields
fieldsAfter :: Env -> [[String]] -> [(Text, Text, Bool)] -> Assertion
fieldsAfter env cmds expected = do
  mapM_ (cobsh_ env) cmds
  fs <- describeFields <$> current env
  fs @?= expected

contains :: String -> String -> Assertion
contains out s = assertBool (show s ++ " not in:\n" ++ out) (s `isInfixOf` out)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testInfo :: Env -> Assertion
testInfo env = do
  out <- cobsh_ env ["info"]
  mapM_ (contains out) ["TEST Sandbox Clients", "Cob tests — clients", "Email", "Name", "Kickstarter", "$instanceLabel", "$[Yes,No]"]

testShowField :: Env -> Assertion
testShowField env = do
  out <- cobsh_ env ["field", "email"]
  mapM_ (contains out) ["Email", "$instanceLabel", "Mandatory    yes", "Duplicable   no"]

testUnknownField :: Env -> Assertion
testUnknownField env = do
  (code, _, err) <- cobsh env ["field", "Nope"] ""
  code @?= ExitFailure 1
  contains err "No field \"Nope\""

testNameDesc :: Env -> Assertion
testNameDesc env = do
  _ <- cobsh_ env ["name", "Renamed"]
  _ <- cobsh_ env ["desc", "Another description"]
  d <- current env
  defName d @?= "Renamed"
  defDescription d @?= "Another description"

testNew :: Env -> Assertion
testNew env = do
  before <- fieldIds <$> current env
  fieldsAfter env [["new", "Extra", "text", "$readonly"]]
    (sbClientFields ++ [("Extra", "$text $readonly", False)])
  after <- fieldIds <$> current env
  assertBool "existing fields must keep their ids" (all (`elem` after) before)

testNewExisting :: Env -> Assertion
testNewExisting env = do
  (code, _, err) <- cobsh env ["new", "Name"] ""
  code @?= ExitFailure 1
  contains err "already exists"
  fieldsAfter env [] sbClientFields

testModifyField :: Env -> Assertion
testModifyField env = do
  fieldsAfter env
    [ ["field", "Email", "--remove", "instanceLabel", "--optional"]
    , ["field", "Name", "--add", "number", "--add", "$readonly", "--mandatory"]
    , ["field", "Kickstarter", "--name", "Backer"]
    ]
    [ ("Email", "", False)
    , ("Name", "$number $readonly", True)
    , ("Backer", "$[Yes,No]", False)
    ]

testFieldDesc :: Env -> Assertion
testFieldDesc env = do
  fieldsAfter env [["field", "Email", "--desc", "The client's email"]]
    (("Email", "$instanceLabel The client's email", True) : drop 1 sbClientFields)

testNoop :: Env -> Assertion
testNoop env = do
  before <- current env
  out <- cobsh_ env ["field", "Email", "--add", "instanceLabel", "--mandatory"]
  contains out "Nothing to change"
  after <- current env
  defVersion after @?= defVersion before

testDeleteAborted :: Env -> Assertion
testDeleteAborted env = do
  (code, out, _) <- cobsh env ["delete", "Kickstarter"] "no\n"
  code @?= ExitSuccess
  contains out "Aborted"
  fieldsAfter env [] sbClientFields

testDelete :: Env -> Assertion
testDelete env = do
  fieldsAfter env [["delete", "Kickstarter"]]
    (take 2 sbClientFields)

testUndo :: Env -> Assertion
testUndo env = do
  before <- current env
  _ <- repl env
    [ "new Extra text", "field Name --add number", "name Renamed"
    , "undo", "undo", "undo" ]
  after <- current env
  defName after @?= defName before
  describeFields after @?= describeFields before
  fieldIds after @?= fieldIds before

testUndoDelete :: Env -> Assertion
testUndoDelete env = do
  _ <- repl env ["delete Kickstarter", "yes", "undo"]
  fieldsAfter env [] sbClientFields

testHistory :: Env -> Assertion
testHistory env = do
  out <- repl env ["new Extra", "name Renamed", "history"]
  mapM_ (contains out) ["1. definition", "2. definition", "+ field", "\"Extra\"", "name: \"TEST Sandbox Clients\" -> \"Renamed\""]

testLines :: Env -> Assertion
testLines env = do
  ls <- fieldLines env
  assertBool ("lines must start with their ids: " ++ show ls) (all ("/" `isPrefixOf`) ls)
  map withoutId ls @?= ["Email*: $instanceLabel", "Name:", "Kickstarter: $[Yes,No]"]

testApplyUnchanged :: Env -> Assertion
testApplyUnchanged env = do
  before <- current env
  (code, out, _) <- apply env [] =<< fieldLines env
  code @?= ExitSuccess
  contains out "Nothing to change"
  after <- current env
  defVersion after @?= defVersion before

testApplyDryRun :: Env -> Assertion
testApplyDryRun env = do
  before <- current env
  ls <- fieldLines env
  (code, out, _) <- apply env ["--dry-run"] (ls ++ ["Notes: $text"])
  code @?= ExitSuccess
  contains out "+ field new \"Notes\" $text"
  after <- current env
  defVersion after @?= defVersion before

testApplyEdits :: Env -> Assertion
testApplyEdits env = do
  [email, name, _kickstarter] <- fieldLines env
  (code, _, err) <- apply env []
    [ email, "  Confirmed*: $[Yes,No]"
    , takeWhile (/= ' ') name ++ " Full name+: some text"
    , "", "Notes: $text" ]
  assertEqual err ExitSuccess code
  ls <- fieldLines env
  map withoutId ls @?= ["Email*: $instanceLabel", "  Confirmed*: $[Yes,No]", "Full name+: some text", "Notes: $text"]
  take 1 ls @?= [email]

testApplyReorder :: Env -> Assertion
testApplyReorder env = do
  ls <- fieldLines env
  (code, _, err) <- apply env [] (reverse ls)
  assertEqual err ExitSuccess code
  ls' <- fieldLines env
  ls' @?= reverse ls

testApplyMove :: Env -> Assertion
testApplyMove env = do
  [email, name, kickstarter] <- fieldLines env
  let (i, rest) = break (== ' ') name
  (code, _, err) <- apply env [] [email, i ++ "   " ++ drop 1 rest, kickstarter]
  code @?= ExitFailure 1
  contains err "fields can't be moved to another parent"
  fieldsAfter env [] sbClientFields
