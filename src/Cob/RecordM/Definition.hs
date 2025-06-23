{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Cob.RecordM.Definition
  ( -- * Static Definition
    Definition(..), Field(..), FieldDescription(..), Condition(..), DefinitionState(..), FieldRequired(..)
  , simpleDefinition, simpleField
    -- * Definition Quoting (DSL)
  , FieldName, getFieldOrder, getFieldId, DefinitionQ, fromDSL, runDSL, extendFromDSL
  , (|=), (|+), (|=!), (|=*), (|=!*), mandatory, duplicable
  , (===), (?)
    -- ** Keywords
  , Keyword(..), instanceLabel, instanceDescription, readOnly, number
  , datetime, date, time, text, list, dollarRef, dollarReferences
    -- * Fetching definition representation
  , DefinitionId(..), FromEmptyInstance(..)
    -- ** Debugging
  , _testBuild, parseFieldDescription, parseKeyword
  ) where

import Data.String
import Control.Monad
import Data.Aeson
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Bifunctor
import Data.Maybe
import qualified Data.Text as T
import Prettyprinter hiding (list)

import Cob.RecordM.Query
import Data.Coerce
import Data.List (sortBy, partition)
import Data.Ord (Down(..), comparing)
import Servant.API (ToHttpApiData(toUrlPiece))
import qualified Data.Aeson.Types as Aeson (Parser)
import Debug.Trace (trace)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char (string, char)
import qualified Text.Megaparsec.Char as P
import Data.Functor (($>))
import Data.Proxy
import Text.Megaparsec.Debug

-- | A Definition Id
newtype DefinitionId = DefId Int

instance Show DefinitionId where show (DefId i) = show i
instance ToJSON DefinitionId where toJSON (DefId i) = toJSON i
instance ToHttpApiData DefinitionId where toUrlPiece (DefId i) = toUrlPiece i

data Definition
  = Definition
    { defName :: Text
    , defState :: DefinitionState
    , defDescription :: Text
    , defFieldDefinitions :: Map FieldName Field
    }
    deriving Show

data Field
  = Field
    { fieldName :: Text
    , fieldDescription :: FieldDescription
    , fieldDuplicable :: Bool
    , fieldRequired :: FieldRequired
    , fieldFields :: Map FieldName Field
    , fieldCondition :: Maybe Condition
    , fieldId :: FieldName
    , fieldOrder :: Maybe Int -- ^ When order is Nothing, the Id is used for the order? See getFieldOrder
    }
    deriving Show

data FieldDescription
  = FieldDesc
    { knownKws :: [Keyword]
    , rawDesc :: Text
    }
instance Show FieldDescription where show = unpack . descText
instance Pretty FieldDescription where pretty = viaShow
instance ToJSON FieldDescription where toJSON = toJSON . descText
instance IsString FieldDescription where fromString = FieldDesc [] . fromString
instance Semigroup FieldDescription where
  FieldDesc kws1 desc1 <> FieldDesc kws2 desc2 = FieldDesc (kws1 <> kws2) (desc1 <> " " <> desc2)
instance Monoid FieldDescription where mempty = FieldDesc [] ""

descText :: FieldDescription -> Text
descText FieldDesc{..} = T.unwords (map (pack . show) knownKws) <> " " <> rawDesc

data Condition
  = Equals
     { lhs :: Text
     , rhs :: Text
     }
  deriving Show

data UnresolvedCondition
  = UnrEquals
     { lhs :: FieldName
     , rhs :: Text
     }
  deriving Show

data DefinitionState
  = EnabledDefinition
  deriving Show

data FieldRequired
  = MandatoryField
  | FieldNotRequired
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- $ Keywords
--------------------------------------------------------------------------------

data Keyword
  = RawKw Text
  | InstanceLabelKw
  | InstanceDescriptionKw
  | ReadOnlyKw
  | NumberKw
  | DateTimeKw
  | DateKw
  | TimeKw
  | TextKw
  | ListKw [Text]
  | RefKw { refKwDefName :: Text
          , refKwQuery   :: Query Void
          -- ^ This reference can be of any type.
          -- Use coerce to make it a query ()
          }
  | ReferencesKw { referencesKwDefName :: Text
                 , referencesKwFieldName :: Text
                 }
instance IsString Keyword where fromString = RawKw . fromString
instance Show Keyword where show = unpack . kwText

kwText :: Keyword -> Text
kwText = \case
    RawKw txt -> txt
    InstanceLabelKw -> keyword "instanceLabel"
    InstanceDescriptionKw -> keyword "instanceDescription"
    ReadOnlyKw -> keyword "readonly"
    NumberKw -> keyword "number"
    DateTimeKw -> keyword "datetime"
    DateKw -> keyword "date"
    TimeKw -> keyword "time"
    TextKw -> keyword "text"
    ListKw labels -> keyword ("[" <> T.intercalate "," labels <> "]")
    RefKw{refKwDefName=defName, refKwQuery=query}
      -> keyword ("ref(" <> defName <> "," <> pack (_q query) <> ")")
    ReferencesKw {referencesKwDefName, referencesKwFieldName}
      -> keyword ("references(" <> referencesKwDefName <> "," <> referencesKwFieldName <> ")")
   where
    keyword = ("$" <>)

instanceLabel, instanceDescription, readOnly, number, datetime, date, time, text :: FieldDescription
instanceLabel       = kwDesc InstanceLabelKw
instanceDescription = kwDesc InstanceDescriptionKw
readOnly            = kwDesc ReadOnlyKw
number              = kwDesc NumberKw
datetime            = kwDesc DateTimeKw
date                = kwDesc DateKw
time                = kwDesc TimeKw
text                = kwDesc TextKw

list :: [Text] -> FieldDescription
list = kwDesc . ListKw

dollarRef :: Text -> Query a -> FieldDescription
dollarRef t q = kwDesc (RefKw t (coerce {-refkw holds a typeless ref-} q))

dollarReferences :: Text -> Text -> FieldDescription
dollarReferences t q = kwDesc (ReferencesKw t q)

kwDesc :: Keyword -> FieldDescription
kwDesc kw = FieldDesc [kw] ""

--------------------------------------------------------------------------------
-- * The Definition Builder DSL / Monad
--------------------------------------------------------------------------------
{-| 

The definition builder DSL allows one to programatically create definitions.

== ** Example **

@
rodrigoTeste = do
  nomeField <- "Nome" := "$instanceLabel"
  "Teste2" := ""
  mandatory "Mandatory" "$readonly"
  "Mandatory2" :=!!! "$readonly"
  dupable "Dupable" "$instanceField"
  "Dupable2" :=*** "$instanceField"

  "Sub Field" :== "Descrip.." do
    "Its SubField" :== "..." do
      "Sub Sub" := ""
      "Sibling Sub Sub" := ""
    "Sibling Sub

  Conditional :== "" do
    if nomeField === "RODRIGO" then do
        "Condition True" := ""
        "Condition True 2" := ""
    else if nomeField === "TESTE"
@
-}

-- | Abstract type representing a definition's field name.
-- Useful for defining conditionals (e.g. see '(===)').
newtype FieldName = UnsafeFieldName Int -- Tagged by identifier
  deriving (Eq, Ord, Show, ToJSONKey, FromJSONKey)

getFieldOrder :: Field -> Int
getFieldOrder (Field{fieldOrder = Nothing, fieldId = UnsafeFieldName i}) = i
getFieldOrder (Field{fieldOrder = Just o}) = o

getFieldId :: FieldName -> String
getFieldId (UnsafeFieldName i) = show (-100000000 - i)

type DefinitionQ = DefinitionQM ()
type DefinitionQM = Free DefinitionQF

data DefinitionQF next
  = Declare Text FieldDescription (FieldName -> next)
  -- ^ A field
  | AddSubs FieldName (DefinitionQM [FieldName]) ([FieldName] -> next)
  -- ^ A field with sub-fields
  | Mandatory FieldName next
  -- ^ Make a field mandatory
  | Duplicable FieldName next
  -- ^ Make a field duplicable
  | IfCondition UnresolvedCondition (DefinitionQM [FieldName]) ([FieldName] -> next)
  deriving Functor

$(makeFree ''DefinitionQF)

-- | Declare a field
(|=) :: Text -> FieldDescription -> DefinitionQM FieldName
(|=) = declare

-- | Declare subfields for a field.
--
-- Typically used as
--
-- @
-- "This Field" |= "This description" |+ do
--    "Sub Field" |= "Desc..."
--    return []
-- @
(|+) :: DefinitionQM FieldName -> DefinitionQM [FieldName] -> DefinitionQM (FieldName, [FieldName])
(|+) decl subs = do
  field <- decl
  subfields <- addSubs field subs
  return (field, subfields)
infixr 0 |+

-- | Declare a mandatory field
(|=!) :: Text -> FieldDescription -> DefinitionQM FieldName
(|=!) lhs rhs = do
  field <- declare lhs rhs
  mandatory field
  return field

-- | Declare a duplicable field
(|=*) :: Text -> FieldDescription -> DefinitionQM FieldName
(|=*) lhs rhs = do
  field <- declare lhs rhs
  duplicable field
  return field
  
-- | Declare a duplicable and mandatory field
(|=!*) :: Text -> FieldDescription -> DefinitionQM FieldName
(|=!*) lhs rhs = do
  field <- declare lhs rhs
  mandatory field
  duplicable field
  return field

-- | Declare fields under a condition
(===) :: FieldName -> Text -> DefinitionQM [FieldName] -> DefinitionQM [FieldName]
(===) lhs rhs = ifCondition (lhs `UnrEquals` rhs)

-- | Syntax sugar for defining conditionals, between @'(===)'@ and the @do@ block.
--
-- @
-- fieldName === "Something" ? do
--    "Field A" |= "$instanceLabel"
--    "Field B" |= "$instanceDescription"
-- @
(?) :: (DefinitionQM [FieldName] -> DefinitionQM [FieldName]) -> DefinitionQM [FieldName] -> DefinitionQM [FieldName]
(?) = ($)
infixr 0 ?

-- Generated:
-- mandatory   :: FieldName -> DefinitionQ
-- duplicable  :: FieldName -> DefinitionQ


--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

fromDSL :: Text
        -- ^ Definition Name
        -> Text
        -- ^ Definition Description
        -> DefinitionQ
        -- ^ Definition quote (DSL building a definition)
        -> Definition
        -- ^ The resulting definition
fromDSL defName defDescription defQ
  = snd $ runDSL defName defDescription defQ

runDSL :: Text
        -- ^ Definition Name
        -> Text
        -- ^ Definition Description
        -> DefinitionQM a
        -- ^ Definition quote (DSL building a definition)
        -> (a, Definition)
        -- ^ The resulting definition and computation result
runDSL defName defDescription defQ
  = runIdentity $
    runFresh $
    runStateT
      (evalStateT (runReaderT (interpret defQ) []) (mempty, Nothing))
      (simpleDefinition defName defDescription)

extendFromDSL
  :: Definition
  -- ^ The Definition to update
  -> DefinitionQ
  -- ^ Definition quote (DSL extending an existing definition)
  -> Definition
  -- ^ The resulting definition
extendFromDSL def defQ
  = snd $
    (evalState . (\(Fresh f) -> f))
      (runStateT
         (evalStateT (runReaderT (interpret defQ) []) (mempty, Nothing))
         def)
    (maybe 1 ((+1) . coerce) $ listToMaybe $ sortBy (comparing Down) $ M.keys def.defFieldDefinitions)

interpret :: DefinitionQM a -> Interpreter a
interpret = iterM algebra

algebra :: DefinitionQF (Interpreter a) -> Interpreter a
algebra = \case
  Declare field descr f -> do
    fname <- fresh
    path <- ask
    currentCond <- gets snd
    let field_data = (simpleField fname field descr){fieldCondition = currentCond}
    lift $ modify (first $ M.insert fname path)
    lift . lift $
      modify (addField path field_data)
    f fname
  AddSubs field subs next -> do
    -- Concat is fine, we never have that many nested fields
    subsNames <-
      local (++ [field]) $
        interpret subs
    next subsNames

  Mandatory field f -> do
    paths <- lift $ gets fst
    lift . lift $ modify $
      modifyField (fromMaybe (error "field not found in paths") $ M.lookup field paths) field (\fi -> fi{fieldRequired = MandatoryField})
    f
  Duplicable field f -> do
    paths <- lift $ gets fst
    lift . lift $ modify $
      modifyField (fromMaybe (error "field not found in paths") $ M.lookup field paths) field (\fi -> fi{fieldDuplicable = True})
    f
  IfCondition cond under f -> do
    cond' <- resolve cond
    modify (second $ const $ Just cond')
    fieldNames <- interpret under
    f fieldNames
  where
    resolve :: UnresolvedCondition -> Interpreter Condition
    resolve = \case
      UnrEquals lhs rhs -> do
        paths <- lift $ gets fst
        let pathToLhs = paths M.! lhs
        defin <- lift . lift $ get
        return $ Equals (fieldName $ findField pathToLhs lhs defin.defFieldDefinitions) rhs

    findField :: [FieldName] -> FieldName -> Map FieldName Field -> Field
    findField [] f m = m M.! f
    findField (p:ps) f m = findField ps f (fieldFields (m M.! p))
  
type Interpreter a
  = ReaderT [FieldName]
      (StateT (Map FieldName [FieldName], Maybe Condition {- The current conditional in effect -})
           -- \^ The path to each FieldName in the Definition, and the current path
           -- Example:
           -- 
           -- Field A:
           --   Field B:
           --     Field C:
           --       -- Current Path == [A,B,C]
           -- 
           -- path to C == [A,B]
           (StateT Definition (Fresh Identity))) a

fresh :: Interpreter FieldName
fresh = do
  i <- lift . lift . lift $ get
  lift . lift . lift $ modify' (+1)
  return (UnsafeFieldName i)

--------------------------------------------------------------------------------
-- Utils: Definition
--------------------------------------------------------------------------------

addField :: [FieldName]
         -- ^ Path at which to add the field.
         -- If empty, add at definition root (fieldDefinitions),
         -- otherwise, traverse the maps and add at the corresponding sub-field.
         -> Field
         -- ^ The (sub)-field to add.
         -> Definition -> Definition
addField path new_field def =
  let fields' = editFieldMap path (M.insert new_field.fieldId new_field) def.defFieldDefinitions
   in def{defFieldDefinitions = fields'}

modifyField :: [FieldName]
            -- ^ Path to field
            -> FieldName
            -- ^ Actual Field
            -> (Field -> Field) -> Definition -> Definition
modifyField path fid upd def =
  let fields' = editFieldMap path (M.adjust upd fid) def.defFieldDefinitions
   in def{defFieldDefinitions = fields'}

editFieldMap :: [FieldName]
             -- ^ Path to field
             -> (Map FieldName Field -> Map FieldName Field)
             -- ^ Function to update Field Map at that path
             -> Map FieldName Field
             -- ^ Map to update
             -> Map FieldName Field
             -- ^ Updated map
editFieldMap [] upd field_map = upd field_map
editFieldMap (path:paths) upd field_map
  = case M.lookup path field_map of
      Nothing -> error $ "Couldn't find path " ++ show path ++ " in " ++ show field_map
      Just field0 ->
        let field1 = field0{fieldFields =
              editFieldMap paths upd field0.fieldFields}
         in M.insert path field1 field_map

  

simpleDefinition :: Text -> Text -> Definition
simpleDefinition name descr
  = Definition
    { defName = name
    , defDescription = descr
    , defState = EnabledDefinition
    , defFieldDefinitions = mempty
    }

simpleField :: FieldName -> Text -> FieldDescription -> Field
simpleField fid name descr
  = Field
    { fieldName = name
    , fieldDescription = descr
    , fieldDuplicable = False
    , fieldRequired = FieldNotRequired
    , fieldFields = mempty
    , fieldCondition = Nothing
    , fieldId = fid
    , fieldOrder = Nothing
    }

--------------------------------------------------------------------------------
-- Util: Fresh
--------------------------------------------------------------------------------

newtype Fresh m a = Fresh (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadState Int)

runFresh :: Monad m => Fresh m a -> m a
runFresh (Fresh f) = evalStateT f 1

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

instance Pretty Definition where
  pretty Definition{..} =
    pretty defName <> ":" <+> pretty defDescription <>
      nest 4
        (line <> vsep (map pretty (M.elems defFieldDefinitions)))
      
instance Pretty Field where
  pretty Field{..} =
    (case fieldCondition of
       Nothing -> mempty
       Just cond -> viaShow cond <> space
    ) <>
    pretty fieldName <> ":" <+> pretty fieldDescription <>
      (if fieldRequired == MandatoryField then space <> "(mandatory)" else mempty) <>
        (if fieldDuplicable then space <> "(duplicable)" else mempty) <>
          nest 4
            (line <> vsep (map pretty (M.elems fieldFields)))

--------------------------------------------------------------------------------
-- Small test to compile a Definition
--------------------------------------------------------------------------------

done :: Monad m => m [a]
done = return []


aux :: DefinitionQM ()
aux = do
  "Teste2"    |=  ""
  "Mandatory" |=! (readOnly <> instanceLabel)
  "Dupable"   |=* instanceDescription
  return ()

_testBuild :: Bool -> Definition
_testBuild b = fromDSL "Nome da Def" "Descr da Def" $ do

  nome <- "Nome" |= instanceLabel

  when b $ do
    "Only if true" |= "OK"
    pure ()

  forM_ [1..15 :: Int] $ \i ->
    ("Field " <> pack (show i)) |= readOnly

  "Sub Field" |=  "Descrip.." |+ do

      "Its SubField" |= "..." |+ do

          "Sub Sub" |= ""
          "Sibling Sub Sub" |= ""
          done

      "Sibling Sub" |= ""
      done

  "Conditional" |= "" |+ do

    nome === "RODRIGO" ? do
        "Condition True" |= ""
        "Condition True 2" |= ""
        done

    nome === "TESTE" ? do
        "NomeIsTeste 1" |=* ""
        "Nome Is Teste 2" |= ""
        done

  aux

  return ()

--------------------------------------------------------------------------------
-- * Fetching a definition representation
--------------------------------------------------------------------------------

-- | Used to fetch a definition using the endpoint which returns an empty instance.
-- That is the same endpoint as the one used by the RecordM web UI.
-- We use this newtype wrapper to avoid giving a FromJSON instance to
-- 'Definition' directly, and force being explicit that we're parsing a
-- definition from an empty instance JSON.
newtype FromEmptyInstance a = FromEmptyInstance a

instance FromJSON (FromEmptyInstance Definition) where
  parseJSON = withObject "FromEmptyInstance Definition" $ \obj -> do

    -- def
    jsonDef <- obj .: "jsonDefinition"
    defName <- jsonDef .: "name"
    defDescription <- jsonDef .:? "description" .!= ""
    defState <- maybe (pure EnabledDefinition) parseDefState =<< jsonDef .:? "state"

    -- fields
    defFieldDefinitions  <- parseFields =<< obj .: "fields"

    return $ FromEmptyInstance $
      Definition{..}
    where
      parseFields :: [Object] -> Aeson.Parser (Map FieldName Field)
      parseFields = fmap M.fromList . mapM parseField

      parseField :: Object -> Aeson.Parser (FieldName, Field)
      parseField obj = do
        fieldDef <- obj .: "fieldDefinition"
        fieldName <- fieldDef .: "name"
        fieldDescription <- parseFieldDescriptionAeson =<< fieldDef .:? "description" .!= ""
        fieldDuplicable <- fieldDef .: "duplicable"
        fieldRequired <- parseFieldRequired =<< fieldDef .: "required"
        fieldId <- UnsafeFieldName <$> fieldDef .: "id"
        fieldOrder <- fieldDef .: "order"

        fieldFields <- parseFields =<< obj .: "fields"
        fieldCondition <- parseCondition =<< obj .:? "condition" .!= Nothing

        return (fieldId, Field{..})

      parseFieldDescriptionAeson :: Text -> Aeson.Parser FieldDescription
      parseFieldDescriptionAeson t = do
        case runParser parseFieldDescription "" t of
          Left e -> fail $ "FAILED TO PARSE DESCRIPTION: " ++ T.unpack t ++ ":\n" ++ show e
          Right x -> return x

      parseCondition :: Maybe Object -> Aeson.Parser (Maybe Condition)
      parseCondition Nothing = pure Nothing
      parseCondition (Just obj) = do
        trace ("TODO: Support parsing conditions... " ++ show obj) $ pure Nothing

      parseDefState :: Text -> Aeson.Parser DefinitionState
      parseDefState = \case
        "enabled" -> return EnabledDefinition
        s -> fail $ "Unexpected DefinitionState: " ++ show s

      parseFieldRequired :: Maybe Text -> Aeson.Parser FieldRequired
      parseFieldRequired = \case
        Just "mandatory" -> return MandatoryField
        _ -> return FieldNotRequired

--------------------------------------------------------------------------------
-- ** Parsing keywords (with Megaparsec)
--------------------------------------------------------------------------------
-- + to simplify, assumes raw description only comes after all keywords
-- + currently ignores `*` default options in lists

parseFieldDescription :: Parsec Void Text FieldDescription
parseFieldDescription = do
  knownKws <- many (parseKeyword <* P.space)
  rawDesc <- takeRest -- the rest of the input
  return FieldDesc{ knownKws , rawDesc }

parseKeyword :: Parsec Void Text Keyword
parseKeyword = dbg "kw" $ (char '$' *>) $
      (string "instanceLabel" $> InstanceLabelKw)
  <|> (string "instanceDescription" $> InstanceDescriptionKw)
  <|> (string "readonly" $> ReadOnlyKw)
  <|> (string "number" $> NumberKw)
  <|> (string "datetime" $> DateTimeKw)
  <|> (string "date" $> DateKw)
  <|> (string "time" $> TimeKw)
  <|> (string "text" $> TextKw)
  <|> parseListKeyword
  <|> parseReferencesKeyword
  <|> parseRefKeyword
  <|> (RawKw <$> takeRest) -- fallback

parseListKeyword :: Parsec Void Text Keyword
parseListKeyword = fmap (ListKw . map T.pack) $ dbg "list" $
  char '[' *> sepBy1 (someTill anySingle (lookAhead (char ',' <|> char ']'))) (char ',') <* char ']'

parseRefKeyword :: Parsec Void Text Keyword
parseRefKeyword = do
  string "ref" *> char '('
  defName <- manyTill anySingle (char ',' *> P.space)
  queryString <- manyTill anySingle (char ')')
  return $ RefKw (tokensToChunk (Proxy @Text) defName) (byText (tokensToChunk (Proxy @Text) queryString))

parseReferencesKeyword :: Parsec Void Text Keyword
parseReferencesKeyword = dbg "refes" do
  string "references" *> char '('
  defName <- manyTill anySingle (char ',')
  fieldName <- manyTill anySingle (lookAhead (char ')'))
  char ')'
  return $ ReferencesKw (tokensToChunk (Proxy @Text) defName) (tokensToChunk (Proxy @Text) fieldName)
