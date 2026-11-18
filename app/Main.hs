{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Bifunctor
import Data.Char
import Data.Group
import Data.IORef
import Data.List (dropWhileEnd, sortOn)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Options.Applicative
import Prettyprinter (Doc, Pretty(..), (<+>), align, annotate, defaultLayoutOptions, hsep, layoutPretty, nest, parens, unAnnotate, viaShow, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), bold, color, renderStrict, underlined)
import qualified ShellWords
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Process (callCommand)
import Text.Megaparsec (eof, errorBundlePretty, parse)
import Text.Read (readMaybe)

import Cob.Session
import Cob.RecordM (getDefinition, updDefinition)
import Cob.RecordM.Definition

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

data Cmd = Modify DefModification | Undo | Info | ShowField FieldRef | Log

data FieldRef = ById Int | ByName Text

data DefModification
  = SetName Text
  | SetDesc Text
  | NewField Text [Keyword]
  | ModField FieldRef [FieldMod]
  | DeleteField FieldRef

data FieldMod = AddKw Keyword | DelKw Keyword | FName Text | FDesc Text | FMandatory Bool | FDuplicable Bool

-- | A self-contained, invertible edit to a 'Definition'
data Edit
  = Rename Text Text
  | Redescribe Text Text
  | AddNew Text FieldDescription
  | DropNew Text FieldDescription
  | SetField Field Field
  | DelField (Maybe FieldName) Field
  | PutField (Maybe FieldName) Field

applyEdit :: Edit -> Definition -> Definition
applyEdit = \case
  Rename _ n -> \d -> d{defName = n}
  Redescribe _ t -> \d -> d{defDescription = t}
  AddNew n fd -> \d -> extendFromDSL d (void (n |= fd))
  DropNew n _ -> \d ->
    let fs = defFieldDefinitions d
     in d{defFieldDefinitions = maybe fs ((`M.delete` fs) . fst) (M.lookupMax (M.filter ((== n) . fieldName) fs))}
  SetField _ new -> onField (fieldId new) \f -> new{fieldFields = fieldFields f}
  DelField _ f -> onFieldMaps (M.delete (fieldId f))
  PutField Nothing f -> \d -> d{defFieldDefinitions = M.insert (fieldId f) f (defFieldDefinitions d)}
  PutField (Just p) f -> onField p \pf -> pf{fieldFields = M.insert (fieldId f) f (fieldFields pf)}

invEdit :: Edit -> Edit
invEdit = \case
  Rename o n -> Rename n o
  Redescribe o n -> Redescribe n o
  AddNew n fd -> DropNew n fd
  DropNew n fd -> AddNew n fd
  SetField o n -> SetField n o
  DelField p f -> PutField p f
  PutField p f -> DelField p f

-- | Fill in the old values of the modification, dropping no-ops
resolve :: Definition -> DefModification -> Either String [Edit]
resolve d = \case
  SetName n -> pure [Rename (defName d) n | defName d /= n]
  SetDesc t -> pure [Redescribe (defDescription d) t | defDescription d /= t]
  NewField n kws -> pure [AddNew n (FieldDesc kws "")]
  ModField r fms -> do
    (_, f) <- lookupField r d
    let f' = foldl' (flip modField) f fms
    pure [SetField f f' | not (null (fieldDiff f f'))]
  DeleteField r -> pure . uncurry DelField <$> lookupField r d

modField :: FieldMod -> Field -> Field
modField = \case
  AddKw kw -> onDesc \fd -> fd{knownKws = knownKws fd ++ [kw | not (any (sameKw kw) (knownKws fd))]}
  DelKw kw -> onDesc \fd -> fd{knownKws = filter (not . sameKw kw) (knownKws fd)}
  FName n -> \f -> f{fieldName = n}
  FDesc t -> onDesc \fd -> fd{rawDesc = t}
  FMandatory b -> \f -> f{fieldRequired = if b then MandatoryField else FieldNotRequired}
  FDuplicable b -> \f -> f{fieldDuplicable = b}
  where onDesc g f = f{fieldDescription = g (fieldDescription f)}

sameKw :: Keyword -> Keyword -> Bool
sameKw a b = show a == show b

-- | Modify the maps of fields at every depth
onFieldMaps :: (M.Map FieldName Field -> M.Map FieldName Field) -> Definition -> Definition
onFieldMaps g d = d{defFieldDefinitions = go (defFieldDefinitions d)}
  where go = fmap (\f -> f{fieldFields = go (fieldFields f)}) . g

onField :: FieldName -> (Field -> Field) -> Definition -> Definition
onField i g = onFieldMaps (M.adjust g i)

-- | Every field with its depth and parent, subfields right after their parent
allFields :: Definition -> [(Int, Maybe FieldName, Field)]
allFields = go 0 Nothing . defFieldDefinitions
  where
    go n p fs = concat
      [ (n, p, f) : go (n + 1) (Just (fieldId f)) (fieldFields f)
      | f <- sortOn getFieldOrder (M.elems fs) ]

lookupField :: FieldRef -> Definition -> Either String (Maybe FieldName, Field)
lookupField r d = case [(p, f) | (_, p, f) <- allFields d, matches f] of
  [x] -> Right x
  [] -> Left ("No field " ++ name)
  xs -> Left ("Ambiguous field " ++ name ++ ", use one of the ids " ++ unwords [getFieldId (fieldId f) | (_, f) <- xs])
  where
    (matches, name) = case r of
      ById i -> ((== show i) . getFieldId . fieldId, show i)
      ByName n -> ((== T.toCaseFold n) . T.toCaseFold . fieldName, show n)

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
-- Command line
--------------------------------------------------------------------------------

-- | A command, optionally switching the active definition first
data Line = Line (Maybe DefinitionId) (Maybe Cmd)

lineP :: Parser Line
lineP = Line
  <$> optional (option (DefId <$> auto) (long "def" <> short 'd' <> metavar "ID" <> help "Switch to definition ID"))
  <*> optional (hsubparser (mconcat
    [ cmd "info" "Show the definition and its fields (flags: M mandatory, D duplicable)" (pure Info)
    , cmd "field" "Show or modify a field" (fieldCmd <$> refArg <*> many fieldModP)
    , cmd "name" "Rename the definition" (Modify . SetName <$> strArgument (metavar "NAME"))
    , cmd "desc" "Set the definition's description" (Modify . SetDesc <$> strArgument (metavar "TEXT"))
    , cmd "new" "Add a field" (fmap Modify . NewField <$> strArgument (metavar "NAME") <*> many (argument kwReader (metavar "KEYWORD..." <> completeWith keywords)))
    , cmd "delete" "Delete a field" (Modify . DeleteField <$> refArg)
    , cmd "undo" "Revert the last change" (pure Undo)
    , cmd "history" "List the changes made in this session" (pure Log)
    ]))
  where
    cmd n d p = command n (info p (progDesc d))
    refArg = argument (fieldRef <$> str) (metavar "FIELD" <> help "Field id or name")
    fieldRef s = maybe (ByName (T.pack s)) ById (readMaybe s)
    fieldCmd r [] = ShowField r
    fieldCmd r ms = Modify (ModField r ms)

fieldModP :: Parser FieldMod
fieldModP = asum
  [ AddKw <$> option kwReader (long "add" <> short 'a' <> metavar "KEYWORD" <> help "Add a keyword, such as $number" <> completeWith keywords)
  , DelKw <$> option kwReader (long "remove" <> short 'r' <> metavar "KEYWORD" <> help "Remove a keyword" <> completeWith keywords)
  , FName <$> strOption (long "name" <> metavar "NAME" <> help "Rename the field")
  , FDesc <$> strOption (long "desc" <> metavar "TEXT" <> help "Set the description following the keywords")
  , FMandatory <$> (flag' True (long "mandatory" <> help "Make the field mandatory") <|> flag' False (long "optional" <> help "Make the field optional"))
  , FDuplicable <$> (flag' True (long "duplicable" <> help "Make the field duplicable") <|> flag' False (long "single" <> help "Make the field not duplicable"))
  ]

-- | A keyword, with or without the leading @$@
kwReader :: ReadM Keyword
kwReader = eitherReader \s -> first errorBundlePretty (parse (parseKeyword <* eof) "" (T.pack ('$' : dropWhile (== '$') s)))

keywords :: [String]
keywords = map show [InstanceLabelKw, InstanceDescriptionKw, ReadOnlyKw, NumberKw, DateTimeKw, DateKw, TimeKw, TextKw]

parserPrefs :: ParserPrefs
parserPrefs = prefs disambiguate

replInfo :: ParserInfo Line
replInfo = info (lineP <**> helper) (progDesc "Edit RecordM definitions" <> footer "Also: !COMMAND runs a shell command, quit exits")

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

render :: Bool -> Doc AnsiStyle -> String
render useColor = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . if useColor then id else unAnnotate

added, removed, kwDoc :: Doc AnsiStyle -> Doc AnsiStyle
added = annotate (color Green)
removed = annotate (color Red)
kwDoc = annotate (color Cyan)

quote :: Text -> Doc ann
quote = viaShow

yesNo :: Bool -> Doc ann
yesNo b = if b then "yes" else "no"

fieldLabel :: Field -> Doc ann
fieldLabel f = pretty (getFieldId (fieldId f)) <+> quote (fieldName f)

-- | What changes from the first field to the second
fieldDiff :: Field -> Field -> [Doc AnsiStyle]
fieldDiff o n = concat
  [ ["name:" <+> quote (fieldName n) | fieldName o /= fieldName n]
  , [added ("+" <> viaShow k) | k <- kws n, not (any (sameKw k) (kws o))]
  , [removed ("-" <> viaShow k) | k <- kws o, not (any (sameKw k) (kws n))]
  , ["desc:" <+> quote (raw n) | raw o /= raw n]
  , ["mandatory:" <+> yesNo (mandatory' n) | mandatory' o /= mandatory' n]
  , ["duplicable:" <+> yesNo (fieldDuplicable n) | fieldDuplicable o /= fieldDuplicable n]
  ]
  where
    kws = knownKws . fieldDescription
    raw = rawDesc . fieldDescription

mandatory' :: Field -> Bool
mandatory' f = fieldRequired f == MandatoryField

prettyEdit :: Edit -> Doc AnsiStyle
prettyEdit = \case
  Rename o n -> "name:" <+> removed (quote o) <+> "->" <+> added (quote n)
  Redescribe o n -> "desc:" <+> removed (quote o) <+> "->" <+> added (quote n)
  AddNew n fd -> added ("+ field" <+> quote n <+> pretty fd)
  DropNew n _ -> removed ("- field" <+> quote n)
  SetField o n -> "field" <+> fieldLabel o <> ":" <+> hsep (fieldDiff o n)
  DelField _ f -> removed ("- field" <+> fieldLabel f)
  PutField _ f -> added ("+ field" <+> fieldLabel f)

fieldsTable :: Definition -> Doc AnsiStyle
fieldsTable d = vsep $ annotate underlined (row "ID" "Name" "Flags" <+> "Description")
  : [ row (getFieldId (fieldId f)) (replicate (2 * n) ' ' ++ T.unpack (fieldName f)) (flags f) <+> descDoc (fieldDescription f)
    | (n, _, f) <- fs ]
  where
    fs = allFields d
    row i nm fl = pretty (replicate (idW - length i) ' ' ++ i) <+> pretty (padR nameW nm) <+> pretty (padR 5 fl)
    idW = maximum (2 : [length (getFieldId (fieldId f)) | (_, _, f) <- fs])
    nameW = maximum (4 : [2 * n + T.length (fieldName f) | (n, _, f) <- fs])
    padR w s = s ++ replicate (w - length s) ' '
    flags f = [if mandatory' f then 'M' else ' ', if fieldDuplicable f then 'D' else ' ']

descDoc :: FieldDescription -> Doc AnsiStyle
descDoc fd = hsep (map (kwDoc . viaShow) (knownKws fd) ++ [pretty (rawDesc fd) | not (T.null (rawDesc fd))])

defInfo :: Definition -> Doc AnsiStyle
defInfo d = vsep
  [ annotate bold (pretty (defName d)) <+> parens ("definition" <+> maybe "?" viaShow (defId d) <> ", version" <+> maybe "?" pretty (defVersion d))
  , pretty (defDescription d)
  , ""
  , fieldsTable d
  ]

fieldInfo :: (Maybe FieldName, Field) -> Doc AnsiStyle
fieldInfo (p, f) = vsep [annotate bold (pretty (k ++ replicate (12 - length k) ' ')) <+> v | (k, v) <- rows]
  where
    fd = fieldDescription f
    rows =
      [ ("ID", pretty (getFieldId (fieldId f)))
      , ("Name", pretty (fieldName f))
      , ("Parent", maybe "-" (pretty . getFieldId) p)
      , ("Keywords", hsep (map (kwDoc . viaShow) (knownKws fd)))
      , ("Description", pretty (rawDesc fd))
      , ("Mandatory", yesNo (mandatory' f))
      , ("Duplicable", yesNo (fieldDuplicable f))
      , ("Condition", maybe "-" (\(Equals _ v) -> "=" <> pretty v) (fieldCondition f))
      , ("Order", pretty (getFieldOrder f))
      , ("Subfields", hsep (map fieldLabel (sortOn getFieldOrder (M.elems (fieldFields f)))))
      ]

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | Changes made, most recent first
type History = [(DefinitionId, Change)]

data Env = Env
  { session :: CobSession
  , useColor :: Bool
  , activeDef :: IORef (Maybe DefinitionId)
  , changes :: IORef History
  }

runCmd :: Env -> Cmd -> IO (Doc AnsiStyle)
runCmd Env{session, activeDef, changes} = \case
  Modify m -> withActive \i -> do
    es <- either fail pure . (`resolve` m) =<< cob (getDefinition i)
    let summary = vsep (map prettyEdit es)
    ok <- if any isDel es then confirm (nest 2 (vsep ["Delete from definition" <+> viaShow i <> ":", summary]) <> "\nProceed?") else pure True
    if null es || not ok then pure (if ok then "Nothing to change" else "Aborted") else do
      _ <- cob (updDefinition i (act (Change es)))
      modifyIORef changes ((i, Change es) :)
      pure summary
  Undo -> readIORef changes >>= \case
    [] -> fail "Nothing to undo"
    (i, c) : h -> do
      let Change es = invert c
      _ <- cob (updDefinition i (act (Change es)))
      writeIORef changes h
      pure (nest 2 (vsep (("Reverted in definition" <+> viaShow i <> ":") : map prettyEdit es)))
  Info -> withActive (fmap defInfo . cob . getDefinition)
  ShowField r -> withActive \i -> fieldInfo <$> (either fail pure . lookupField r =<< cob (getDefinition i))
  Log -> do
    h <- readIORef changes
    pure if null h then "No changes in this session" else vsep
      [ pretty n <> "." <+> "definition" <+> viaShow i <> ":" <+> align (vsep (map prettyEdit es))
      | (n, (i, Change es)) <- zip [1 :: Int ..] h ]
  where
    cob m = runReaderT m session
    withActive k = maybe (fail "No active definition: use `-d ID` first") k =<< readIORef activeDef
    isDel = \case DelField{} -> True; _ -> False

confirm :: Doc ann -> IO Bool
confirm q = do
  putStr (show (unAnnotate q) ++ " (yes/no) ") >> hFlush stdout
  (`elem` ["y", "yes"]) . map toLower <$> getLine

run :: Env -> Line -> IO ()
run env (Line i c) = do
  forM_ i (writeIORef (activeDef env) . Just)
  forM_ (c <|> Info <$ i) (putStrLn . render (useColor env) <=< runCmd env)

printErr :: Env -> SomeException -> IO ()
printErr env e = hPutStrLn stderr (render (useColor env) (removed (pretty (dropWhileEnd isSpace msg))))
  where msg = maybe (displayException e) ioeGetErrorString (fromException e)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

data Config = Config { host :: String, token :: String }
  deriving Generic
instance FromJSON Config

-- | @$COBSH_CONFIG@, else @.cfg@ if present, else @~/.config/cobsh/config.json@
configPath :: IO FilePath
configPath = do
  hasLocal <- doesFileExist ".cfg"
  global <- getXdgDirectory XdgConfig ("cobsh" </> "config.json")
  fromMaybe (if hasLocal then ".cfg" else global) <$> lookupEnv "COBSH_CONFIG"

main :: IO ()
main = do
  (cfg, l) <- customExecParser parserPrefs $ info
    ((,) <$> optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Configuration with the host and token")) <*> lineP <**> helper)
    (fullDesc <> progDesc "Edit RecordM definitions; without a command, start an interactive shell")
  Config{host, token} <- either fail pure =<< eitherDecodeFileStrict' =<< maybe configPath pure cfg
  useColor <- (&&) <$> hIsTerminalDevice stdout <*> (isNothing <$> lookupEnv "NO_COLOR")
  histFile <- (</> ".cobsh_history") <$> getHomeDirectory
  withSession host token \s -> do
    env <- Env s{verbosity = WARN} useColor <$> newIORef Nothing <*> newIORef []
    case l of
      Line Nothing Nothing -> runInputT (Settings completeLine (Just histFile) True) do
        outputStrLn "cobsh: type `help` for help"
        repl env
      _ -> run env l `catch` \e -> printErr env e >> exitFailure

repl :: Env -> InputT IO ()
repl env = do
  i <- liftIO (readIORef (activeDef env))
  ml <- handleInterrupt (pure (Just "")) $ withInterrupt $
    getInputLine (maybe "cobsh> " (\d -> "cobsh " ++ show d ++ "> ") i)
  forM_ ml \l -> unless (words l `elem` [["quit"], ["exit"]]) do
    liftIO (runLine env l `catch` printErr env)
    repl env

runLine :: Env -> String -> IO ()
runLine env l = case dropWhile isSpace l of
  '!' : c -> callCommand c
  _ -> do
    ws <- either fail pure (ShellWords.parse l)
    case execParserPure parserPrefs replInfo (if ws == ["help"] then ["--help"] else ws) of
      Success x -> run env x
      Failure f -> putStrLn (fst (renderFailure f "cobsh"))
      CompletionInvoked _ -> pure ()

-- | Complete with optparse's own completion
completeLine :: CompletionFunc IO
completeLine = completeWordWithPrev Nothing " \t" \prev w -> do
  let ws = "cobsh" : either (const (words (reverse prev))) id (ShellWords.parse (reverse prev)) ++ [w]
      args = ["--bash-completion-index", show (length ws - 1)] ++ concatMap (\x -> ["--bash-completion-word", x]) ws
  case execParserPure parserPrefs replInfo args of
    CompletionInvoked c -> map simpleCompletion . lines <$> execCompletion c "cobsh"
    _ -> pure []
