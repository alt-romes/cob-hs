{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Bifunctor
import Data.Group
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Options.Applicative
import Prettyprinter (pretty)
import qualified ShellWords
import System.Console.Haskeline
import Text.Megaparsec (eof, errorBundlePretty, parse)

import Cob.Session
import Cob.RecordM (getDefinition, updDefinition)
import Cob.RecordM.Definition

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

data Cmd = Modify [DefModification] | Undo | Fields

type FieldId = Int

data DefModification
  = SetName Text
  | SetDesc Text
  | AddField Text
  | AddFieldKw FieldId Keyword
  | RemoveFieldKw FieldId Keyword

-- | A self-contained, invertible edit to a 'Definition'
data Edit
  = Rename Text Text
  | Redescribe Text Text
  | NewField Text
  | DropField Text
  | AddKw FieldId Keyword
  | DelKw FieldId Keyword

applyEdit :: Edit -> Definition -> Definition
applyEdit = \case
  Rename _ n -> \d -> d{defName = n}
  Redescribe _ t -> \d -> d{defDescription = t}
  NewField n -> \d -> extendFromDSL d (void (n |= mempty))
  DropField n -> \d ->
    let fs = defFieldDefinitions d
     in d{defFieldDefinitions = maybe fs ((`M.delete` fs) . fst) (M.lookupMax (M.filter ((== n) . fieldName) fs))}
  AddKw n kw -> onFieldDesc n \fd -> fd{knownKws = knownKws fd ++ [kw]}
  DelKw n kw -> onFieldDesc n \fd -> fd{knownKws = filter (not . sameKw kw) (knownKws fd)}

invEdit :: Edit -> Edit
invEdit = \case
  Rename o n -> Rename n o
  Redescribe o n -> Redescribe n o
  NewField n -> DropField n
  DropField n -> NewField n
  AddKw n kw -> DelKw n kw
  DelKw n kw -> AddKw n kw

-- | Fill in the old values of the modifications, dropping no-ops
resolve :: Definition -> [DefModification] -> [Edit]
resolve _ [] = []
resolve d (m:ms) = es ++ resolve (act (Change es) d) ms
  where
    es = case m of
      SetName n -> [Rename (defName d) n]
      SetDesc t -> [Redescribe (defDescription d) t]
      AddField n -> [NewField n]
      AddFieldKw n kw -> [AddKw n kw | not (hasKw n kw d)]
      RemoveFieldKw n kw -> [DelKw n kw | hasKw n kw d]

sameKw :: Keyword -> Keyword -> Bool
sameKw a b = show a == show b

hasKw :: FieldId -> Keyword -> Definition -> Bool
hasKw i kw = any go . defFieldDefinitions
  where go f = (isField i f && any (sameKw kw) (knownKws (fieldDescription f))) || any go (fieldFields f)

isField :: FieldId -> Field -> Bool
isField i f = getFieldId (fieldId f) == show i

-- | Modify the description of the (sub)field with the given id
onFieldDesc :: FieldId -> (FieldDescription -> FieldDescription) -> Definition -> Definition
onFieldDesc i g d = d{defFieldDefinitions = go (defFieldDefinitions d)}
  where
    go = fmap \f ->
      (if isField i f then f{fieldDescription = g (fieldDescription f)} else f)
        {fieldFields = go (fieldFields f)}

-- | Every field as @id: name (order n)@, subfields indented under their parent
fieldsList :: Definition -> [String]
fieldsList = go "" . defFieldDefinitions
  where
    go ind fs = concat
      [ (ind ++ getFieldId (fieldId f) ++ ": " ++ T.unpack (fieldName f) ++ " (order " ++ show (getFieldOrder f) ++ ")")
          : go ("  " ++ ind) (fieldFields f)
      | f <- sortOn getFieldOrder (M.elems fs) ]

--------------------------------------------------------------------------------
-- Changes: the group acting on definitions
--------------------------------------------------------------------------------

-- | Edits applied in order
newtype Change = Change [Edit]

instance Semigroup Change where
  Change a <> Change b = Change (b ++ a)
instance Monoid Change where
  mempty = Change []
instance Group Change where
  invert (Change es) = Change (reverse (map invEdit es))

-- | @act (s <> t) = act s . act t@ and @act (invert c) . act c = id@,
-- the latter for definitions the change was resolved against.
act :: Change -> Definition -> Definition
act (Change es) d = foldl' (flip applyEdit) d es

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- | A keyword, with or without the leading @$@
kwReader :: ReadM Keyword
kwReader = eitherReader \s -> first errorBundlePretty (parse (parseKeyword <* eof) "" (T.pack ('$' : dropWhile (== '$') s)))

-- | A line optionally sets the active definition, then runs a command on it
lineP :: Parser (Maybe DefinitionId, Maybe Cmd)
lineP = (,)
  <$> optional (option (DefId <$> auto) (long "def" <> short 'd' <> metavar "ID" <> help "Set the active definition"))
  <*> optional (hsubparser (mconcat
    [ cmd "name" "Rename the definition" (Modify . pure . SetName <$> strArgument (metavar "NAME"))
    , cmd "desc" "Set the definition's description" (Modify . pure . SetDesc <$> strArgument (metavar "TEXT"))
    , cmd "new" "Add a field" (Modify . pure . AddField <$> strArgument (metavar "NAME"))
    , cmd "field" "Add or remove keywords of a field" (fieldMods <$> argument auto (metavar "ID") <*> some kwMod)
    , cmd "undo" "Revert the last change" (pure Undo)
    , cmd "fields" "List the fields" (pure Fields)
    ]))
  where
    cmd n d p = command n (info p (progDesc d))
    fieldMods i = Modify . map ($ i)
    kwMod = flip AddFieldKw <$> option kwReader (long "add" <> short 'a' <> metavar "KEYWORD" <> help "Add a keyword, such as $number")
        <|> flip RemoveFieldKw <$> option kwReader (long "remove" <> short 'r' <> metavar "KEYWORD" <> help "Remove a keyword")

replInfo :: ParserInfo (Maybe DefinitionId, Maybe Cmd)
replInfo = info (lineP <**> helper) (progDesc "Edit RecordM definitions")

--------------------------------------------------------------------------------
-- REPL
--------------------------------------------------------------------------------

data Config = Config { host :: String, token :: String }
  deriving Generic
instance FromJSON Config

main :: IO ()
main = do
  path <- execParser $ info
    (strArgument (metavar "CONFIG" <> value ".cfg" <> showDefault <> help "Configuration with the host and token") <**> helper)
    (progDesc "Edit RecordM definitions interactively")
  Config{host, token} <- either fail pure =<< eitherDecodeFileStrict' path
  withSession host token \s -> runInputT defaultSettings (repl s{verbosity = WARN} Nothing [])

type History = [(DefinitionId, Change)]

-- | Run a command on the active definition, returning what to print and the new history
runCmd :: History -> Maybe DefinitionId -> Cmd -> ReaderT CobSession IO (String, History)
runCmd h active = \case
  Modify [] -> pure ("", h)
  Modify ms -> withActive \i -> do
    c <- Change . (`resolve` ms) <$> getDefinition i
    d <- updDefinition i (act c)
    pure (show (pretty d), (i, c):h)
  Undo -> case h of
    [] -> fail "Nothing to undo"
    (i, c):h' -> (, h') . show . pretty <$> updDefinition i (act (invert c))
  Fields -> withActive \i -> (, h) . unlines . fieldsList <$> getDefinition i
  where
    withActive k = maybe (fail "No active definition: use `def <id>` first") k active

repl :: CobSession -> Maybe DefinitionId -> History -> InputT IO ()
repl s active h = do
  ml <- getInputLine (maybe "" (\i -> "def " ++ show i) active ++ "> ")
  forM_ ml \l -> case execParserPure (prefs mempty) replInfo <$> ShellWords.parse l of
    Left e -> outputStrLn e >> repl s active h
    Right (Failure f) -> outputStrLn (fst (renderFailure f "cobsh")) >> repl s active h
    Right (CompletionInvoked _) -> repl s active h
    Right (Success (newActive, cmd)) -> do
      let active' = newActive <|> active
      r <- liftIO $ try @SomeException $ runReaderT (runCmd h active' (fromMaybe (Modify []) cmd)) s
      case r of
        Left e -> outputStrLn (displayException e) >> repl s active' h
        Right (out, h') -> unless (null out) (outputStrLn out) >> repl s active' h'
