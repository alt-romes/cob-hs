{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Act
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Bifunctor
import Data.Char
import Data.Group
import Data.IORef
import Data.List (dropWhileEnd, sortOn)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Last(..))
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import Options.Applicative
import Prettyprinter (Doc, Pretty(..), (<+>), align, annotate, defaultLayoutOptions, hcat, hsep, layoutPretty, nest, parens, unAnnotate, viaShow, vsep)
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

data Cmd = Modify DefMod | Undo | Info | ShowField FieldRef | Log

data FieldRef = ById Int | ByName Text

data DefMod
  = Name Text
  | Desc Text
  | New Text [Keyword]
  | ModField FieldRef [FieldMod]
  | Delete FieldRef

data FieldMod = AddKw Keyword | DelKw Keyword | FName Text | FDesc FieldDescription | FMandatory Bool | FDuplicable Bool

-- | Sets a part of a 'Definition', remembering what was there before
data Edit
  = SetName Text Text
  | SetDesc Text Text
  | SetField [FieldName] FieldName (Maybe Field) (Maybe Field)
  -- ^ The field with this id under these parents

applyEdit :: Edit -> Definition -> Definition
applyEdit = \case
  SetName _ n -> \d -> d{defName = n}
  SetDesc _ t -> \d -> d{defDescription = t}
  SetField ps k _ new -> fieldAt ps k .~ new

invEdit :: Edit -> Edit
invEdit = \case
  SetName o n -> SetName n o
  SetDesc o n -> SetDesc n o
  SetField ps k o n -> SetField ps k n o

-- | The field with this id under this path of parent ids
fieldAt :: HasFields s => [FieldName] -> FieldName -> Traversal' s (Maybe Field)
fieldAt [] k = fieldById k
fieldAt (p : ps) k = fieldById p . _Just . fieldAt ps k

-- | Fill in the old values of the modification, dropping no-ops
resolve :: Definition -> DefMod -> Either String [Edit]
resolve d = \case
  Name n -> pure [SetName (defName d) n | defName d /= n]
  Desc t -> pure [SetDesc (defDescription d) t | defDescription d /= t]
  New n kws
    | any ((== n) . fieldName) (d ^. fields) -> Left ("Field " ++ show n ++ " already exists")
    | otherwise -> pure [SetField [] (NewField n) Nothing (Just (d ^. field n & spec . description .~ foldMap kwDesc kws))]
  ModField r fms -> do
    (ps, f) <- lookupField r d
    let f' = foldl' (flip modField) f fms
    pure [SetField ps (fieldId f) (Just f) (Just f') | not (null (fieldDiff f f'))]
  Delete r -> do
    (ps, f) <- lookupField r d
    pure [SetField ps (fieldId f) (Just f) Nothing]

-- | Give new fields the ids the server assigned to them
settle :: Definition -> Edit -> Edit
settle d = \case
  SetField ps (NewField n) Nothing (Just _)
    | f : _ <- sortOn (Down . fieldId) [f | (ps', f) <- allFields d, ps' == ps, fieldName f == n]
    -> SetField ps (fieldId f) Nothing (Just f)
  e -> e

modField :: FieldMod -> Field -> Field
modField = \case
  AddKw k -> spec . description %~ \fd -> if any (sameKw k) (knownKws fd) then fd else fd <> kwDesc k
  DelKw k -> spec . description %~ dropKw k
  FName n -> \f -> f{fieldName = n}
  FDesc t -> spec . description %~ \fd -> foldMap kwDesc (knownKws fd) <> t
  FMandatory b -> spec . isMandatory .~ b
  FDuplicable b -> spec . isDuplicable .~ b

kwDesc :: Keyword -> FieldDescription
kwDesc k = FieldDesc [KwPart k]

-- | Drop the keyword along with the blanks separating it from the rest
dropKw :: Keyword -> FieldDescription -> FieldDescription
dropKw k (FieldDesc ps) = FieldDesc (dropWhileEnd blank (go ps))
  where
    go = \case
      KwPart k' : rest | sameKw k k' -> go (dropWhile blank rest)
      p : rest -> p : go rest
      [] -> []
    blank = \case RawPart t -> T.all isSpace t; KwPart _ -> False

-- | The description without its keywords
rawText :: FieldDescription -> Text
rawText (FieldDesc ps) = T.unwords (T.words (T.concat [t | RawPart t <- ps]))

sameKw :: Keyword -> Keyword -> Bool
sameKw a b = show a == show b

-- | Every field with the ids of its parents, subfields right after their parent
allFields :: Definition -> [([FieldName], Field)]
allFields = go [] . (^. fields)
  where
    go ps fs = concat
      [ (ps, f) : go (ps ++ [fieldId f]) (f ^. fields)
      | f <- sortOn fieldOrder (M.elems fs) ]

lookupField :: FieldRef -> Definition -> Either String ([FieldName], Field)
lookupField r d = case [x | x@(_, f) <- allFields d, matches f] of
  [x] -> Right x
  [] -> Left ("No field " ++ name)
  xs -> Left ("Ambiguous field " ++ name ++ ", use one of the ids " ++ unwords [idText (fieldId f) | (_, f) <- xs])
  where
    (matches, name) = case r of
      ById i -> ((== ExistingField i) . fieldId, show i)
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

-- | @invert c • c • d = d@ for definitions @d@ the change was resolved against
instance Act Change Definition where
  Change es • d = foldl' (flip applyEdit) d es

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
    , cmd "name" "Rename the definition" (Modify . Name <$> strArgument (metavar "NAME"))
    , cmd "desc" "Set the definition's description" (Modify . Desc <$> strArgument (metavar "TEXT"))
    , cmd "new" "Add a field" (fmap Modify . New <$> strArgument (metavar "NAME") <*> many (argument kwReader (metavar "KEYWORD..." <> completeWith keywords)))
    , cmd "delete" "Delete a field" (Modify . Delete <$> refArg)
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
  , FDesc <$> option descReader (long "desc" <> metavar "TEXT" <> help "Set the description following the keywords")
  , FMandatory <$> (flag' True (long "mandatory" <> help "Make the field mandatory") <|> flag' False (long "optional" <> help "Make the field optional"))
  , FDuplicable <$> (flag' True (long "duplicable" <> help "Make the field duplicable") <|> flag' False (long "single" <> help "Make the field not duplicable"))
  ]

-- | A keyword, with or without the leading @$@
kwReader :: ReadM Keyword
kwReader = eitherReader \s -> first errorBundlePretty (parse (parseKeyword <* eof) "" (T.pack ('$' : dropWhile (== '$') s)))

descReader :: ReadM FieldDescription
descReader = eitherReader (first errorBundlePretty . parse parseFieldDescription "" . T.pack)

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

idText :: FieldName -> String
idText = \case
  ExistingField i -> show i
  NewField _ -> "new"

fieldLabel :: Field -> Doc ann
fieldLabel f = pretty (idText (fieldId f)) <+> quote (fieldName f)

-- | What changes from the first field to the second
fieldDiff :: Field -> Field -> [Doc AnsiStyle]
fieldDiff o n = concat
  [ ["name:" <+> quote (fieldName n) | fieldName o /= fieldName n]
  , [added ("+" <> viaShow k) | k <- kws n, not (any (sameKw k) (kws o))]
  , [removed ("-" <> viaShow k) | k <- kws o, not (any (sameKw k) (kws n))]
  , ["desc:" <+> quote (raw n) | raw o /= raw n]
  , ["mandatory:" <+> yesNo (mandatory' n) | mandatory' o /= mandatory' n]
  , ["duplicable:" <+> yesNo (duplicable' n) | duplicable' o /= duplicable' n]
  ]
  where
    kws = knownKws . (^. spec . description)
    raw = rawText . (^. spec . description)

mandatory', duplicable' :: Field -> Bool
mandatory' = (^. spec . isMandatory)
duplicable' = (^. spec . isDuplicable)

prettyEdit :: Edit -> Doc AnsiStyle
prettyEdit = \case
  SetName o n -> "name:" <+> removed (quote o) <+> "->" <+> added (quote n)
  SetDesc o n -> "desc:" <+> removed (quote o) <+> "->" <+> added (quote n)
  SetField _ _ Nothing (Just f) -> added ("+ field" <+> fieldLabel f <+> descDoc (f ^. spec . description))
  SetField _ _ (Just f) Nothing -> removed ("- field" <+> fieldLabel f)
  SetField _ _ (Just o) (Just n) -> "field" <+> fieldLabel o <> ":" <+> hsep (fieldDiff o n)
  SetField _ _ Nothing Nothing -> mempty

fieldsTable :: Definition -> Doc AnsiStyle
fieldsTable d = vsep $ annotate underlined (row "ID" "Name" "Flags" <+> "Description")
  : [ row (idText (fieldId f)) (indent ps ++ T.unpack (fieldName f)) (flags f) <+> descDoc (f ^. spec . description)
    | (ps, f) <- fs ]
  where
    fs = allFields d
    indent ps = replicate (2 * length ps) ' '
    row i nm fl = pretty (replicate (idW - length i) ' ' ++ i) <+> pretty (padR nameW nm) <+> pretty (padR 5 fl)
    idW = maximum (2 : [length (idText (fieldId f)) | (_, f) <- fs])
    nameW = maximum (4 : [length (indent ps) + T.length (fieldName f) | (ps, f) <- fs])
    padR w s = s ++ replicate (w - length s) ' '
    flags f = [if mandatory' f then 'M' else ' ', if duplicable' f then 'D' else ' ']

descDoc :: FieldDescription -> Doc AnsiStyle
descDoc (FieldDesc ps) = hcat [case p of KwPart k -> kwDoc (viaShow k); RawPart t -> pretty t | p <- ps]

defInfo :: Definition -> Doc AnsiStyle
defInfo d = vsep
  [ annotate bold (pretty (defName d)) <+> parens ("definition" <+> maybe "?" viaShow (defId d) <> ", version" <+> maybe "?" pretty (defVersion d))
  , pretty (defDescription d)
  , ""
  , fieldsTable d
  ]

fieldInfo :: ([FieldName], Field) -> Doc AnsiStyle
fieldInfo (ps, f) = vsep [annotate bold (pretty (k ++ replicate (12 - length k) ' ')) <+> v | (k, v) <- rows]
  where
    fd = f ^. spec . description
    rows =
      [ ("ID", pretty (idText (fieldId f)))
      , ("Name", pretty (fieldName f))
      , ("Parent", maybe "-" (pretty . idText) (listToMaybe (reverse ps)))
      , ("Keywords", hsep (map (kwDoc . viaShow) (knownKws fd)))
      , ("Description", pretty (rawText fd))
      , ("Mandatory", yesNo (mandatory' f))
      , ("Duplicable", yesNo (duplicable' f))
      , ("Condition", maybe "-" (pretty . condText) (getLast (specCondition (fieldSpec f))))
      , ("Order", pretty (fieldOrder f))
      , ("Subfields", hsep (map fieldLabel (sortOn fieldOrder (M.elems (f ^. fields)))))
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
      d' <- cob (updDefinition i (Change es •))
      modifyIORef changes ((i, Change (map (settle d') es)) :)
      pure summary
  Undo -> readIORef changes >>= \case
    [] -> fail "Nothing to undo"
    (i, c) : h -> do
      let c'@(Change es) = invert c
      _ <- cob (updDefinition i (c' •))
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
    isDel = \case SetField _ _ (Just _) Nothing -> True; _ -> False

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
