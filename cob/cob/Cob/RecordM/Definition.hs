{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Cob.RecordM.Definition
  ( -- * Static Definition
    Definition(..), Field(..), FieldSpec(..), FieldDescription(..), DescPart(..), DefinitionState(..)
  , FieldName(..), simpleDefinition

    -- * Conditions
  , Condition(..), CondExpr(..), CmpOp(..), condText, parseCondText

    -- * Building and editing definitions with lenses
    --
    -- | The @'field'@ lens will find a field by name, or create a new field if none by that name exists.
    -- If there are multiple sibling fields with the same name, we fail at runtime with an ambiguity error.
    --
    -- @
    -- simpleDefinition "Kitchen" "A definition"
    --   & field "Name" . spec <>~ instanceLabel <> mandatory
    --   & field "Details" . field "Note" . spec <>~ text
    --   & fieldById (ExistingField 42) .~ Nothing
    -- @
  , HasFields(..), field, fieldById, spec
  , description, isMandatory, isDuplicable
  , mandatory, notMandatory, duplicable, notDuplicable, ifCondition
  , (&), (^.), (.~), (%~), (<>~)

    -- * Keywords
  , Keyword(..), instanceLabel, instanceDescription, readOnly, number
  , datetime, date, time, text, list, listDefault, dollarRef, dollarReferences

    -- * RecordM JSON representation
  , DefinitionId(..), definitionValue, parseDefinition
  , descText, knownKws

    -- ** Debugging
  , parseFieldDescription, parseKeyword
  ) where

import Data.String
import Data.Aeson
import Control.Monad.State (State, evalState, get, modify)
import Data.List (sortOn)
import Data.Monoid
import GHC.Generics (Generic, Generically(..))
import GHC.Stack (HasCallStack)
import Lens.Micro
import Data.Text (Text, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T

import Cob.RecordM.Query
import Cob.RecordM.Definition.Condition
import Data.Coerce
import Servant.API (ToHttpApiData(toUrlPiece))
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (string, char)
import Data.Functor (($>))
import Data.Proxy
import Data.Char (isSpace, isAlphaNum)

-- | A Definition Id
newtype DefinitionId = DefId Int

instance Show DefinitionId where show (DefId i) = show i
instance ToJSON DefinitionId where toJSON (DefId i) = toJSON i
instance ToHttpApiData DefinitionId where toUrlPiece (DefId i) = toUrlPiece i

data Definition
  = Definition
    { defId   :: Maybe DefinitionId
      -- ^ Nothing if the definition is new
    , defName :: Text
    , defState :: DefinitionState
    , defDescription :: Text
    , defFieldDefinitions :: Map FieldName Field
    , defVersion :: Maybe Integer
      -- ^ The "version", when the definition already exists.
      -- For atomic updates, like @'Ref'@'s version
    }
    deriving Show

data Field
  = Field
    { fieldName :: Text
    , fieldSpec :: FieldSpec
    , fieldFields :: Map FieldName Field
    , fieldId :: FieldName
    , fieldOrder :: Int
      -- ^ You can update the order of the fields to re-order them in RecordM
      -- upstream. (e.g. you can set fields with order 0,1,2 to have order
      -- 2,1,0 respectively, and they'll be reversed in RecordM).
      --
      -- New fields get the next order id available.
    }
    deriving Show

-- | Monoidal specification of a RecordM field.
--
-- The @'Last'@ fields mean the last mappend wins, e.g.
-- @'mandatory' <> 'notMandatory'@ means @'notMandatory'@
data FieldSpec = FieldSpec
  { specDescription :: FieldDescription
  , specMandatory   :: Last Bool
  , specDuplicable  :: Last Bool
  , specCondition   :: Last Condition
    -- ^ Shown only when the condition holds
  }
  deriving Show
  deriving Generic
  deriving (Semigroup, Monoid) via Generically FieldSpec

instance IsString FieldSpec where fromString s = mempty{specDescription = fromString s}

-- | The description keeps its parts in order, so that 'descText' gives back
-- exactly the text it was parsed from.
newtype FieldDescription = FieldDesc [DescPart]
data DescPart = KwPart Keyword | RawPart Text

instance IsString  FieldDescription where fromString = either error id . runFieldDescriptionParser . T.pack
instance Show      FieldDescription where show = unpack . descText
instance Semigroup FieldDescription where FieldDesc a <> FieldDesc b = FieldDesc (a ++ b)
instance Monoid    FieldDescription where mempty = FieldDesc []

descText :: FieldDescription -> Text
descText (FieldDesc ps) = T.unwords [either id kwText (partEither p) | p <- ps]
  where
    partEither (KwPart k) = Right k
    partEither (RawPart t) = Left t

knownKws :: FieldDescription -> [Keyword]
knownKws (FieldDesc ps) = [k | KwPart k <- ps]

data FieldName
  = ExistingField Int
  -- ^ The id of a field which exists in the server
  | NewField Text
  -- ^ A field created here, doesn't have a number id yet.
  deriving (Eq, Ord, Show)

mandatory, notMandatory, duplicable, notDuplicable :: FieldSpec
mandatory     = mempty{specMandatory = Last (Just True)}
notMandatory  = mempty{specMandatory = Last (Just False)}
duplicable    = mempty{specDuplicable = Last (Just True)}
notDuplicable = mempty{specDuplicable = Last (Just False)}

-- | Only show the field when the condition holds, e.g. @ifCondition "=Yes"@
-- (the parent field is @Yes@) or @ifCondition "Kind=A || Kind=B"@.
-- Fails if the condition doesn't parse.
ifCondition :: HasCallStack => Text -> FieldSpec
ifCondition t = case parseCondText t of
  Just c -> mempty{specCondition = Last (Just c)}
  Nothing -> error $ "ifCondition: can't parse condition " ++ show t

data DefinitionState
  = EnabledDefinition
  deriving Show

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
  | ListKw [Text] (Maybe Text)
  -- ^ Options and the default option
  | RefKw { refKwDefName :: Text
          , refKwQuery   :: Query Void
          -- ^ This reference can be of any type.
          -- Use coerce to make it a query ()
          }
  | ReferencesKw { referencesKwDefName :: Text
                 , referencesKwFieldName :: Text
                 }

instance IsString Keyword where fromString = RawKw . fromString
instance Show     Keyword where show = unpack . kwText

kwText :: Keyword -> Text
kwText = \case
    RawKw txt -> keyword txt
    InstanceLabelKw -> keyword "instanceLabel"
    InstanceDescriptionKw -> keyword "instanceDescription"
    ReadOnlyKw -> keyword "readonly"
    NumberKw -> keyword "number"
    DateTimeKw -> keyword "datetime"
    DateKw -> keyword "date"
    TimeKw -> keyword "time"
    TextKw -> keyword "text"
    ListKw labels def -> keyword ("[" <> T.intercalate "," [l <> if Just l == def then "*" else "" | l <- labels] <> "]")
    RefKw{refKwDefName=defName, refKwQuery=query}
      -> keyword ("ref(" <> defName <> "," <> pack (_q query) <> ")")
    ReferencesKw {referencesKwDefName, referencesKwFieldName}
      -> keyword ("references(" <> referencesKwDefName <> "," <> referencesKwFieldName <> ")")
   where
    keyword = ("$" <>)

instanceLabel, instanceDescription, readOnly, number, datetime, date, time, text :: FieldSpec
instanceLabel       = kwSpec InstanceLabelKw
instanceDescription = kwSpec InstanceDescriptionKw
readOnly            = kwSpec ReadOnlyKw
number              = kwSpec NumberKw
datetime            = kwSpec DateTimeKw
date                = kwSpec DateKw
time                = kwSpec TimeKw
text                = kwSpec TextKw

list :: [Text] -> FieldSpec
list ls = kwSpec (ListKw ls Nothing)

-- | A list with a default option
listDefault :: [Text] -> Text -> FieldSpec
listDefault ls def = kwSpec (ListKw ls (Just def))

dollarRef :: Text -> Query a -> FieldSpec
dollarRef t q = kwSpec (RefKw t (coerce {-refkw holds a typeless ref-} q))

dollarReferences :: Text -> Text -> FieldSpec
dollarReferences t q = kwSpec (ReferencesKw t q)

kwSpec :: Keyword -> FieldSpec
kwSpec kw = mempty{specDescription = FieldDesc [KwPart kw]}

--------------------------------------------------------------------------------
-- * Creating and editing
--------------------------------------------------------------------------------

simpleDefinition :: Text -> Text -> Definition
simpleDefinition name descr
  = Definition
    { defId = Nothing
    , defName = name
    , defDescription = descr
    , defState = EnabledDefinition
    , defFieldDefinitions = mempty
    , defVersion = Nothing
    }

-- | Things with (sub)fields: a 'Definition' and each 'Field'
class HasFields s where
  fields :: Lens' s (Map FieldName Field)

instance HasFields Definition where
  fields = lens (.defFieldDefinitions) (\d fs -> d{defFieldDefinitions = fs})

instance HasFields Field where
  fields = lens (.fieldFields) (\f fs -> f{fieldFields = fs})

-- | The field with this id, if any. Setting 'Nothing' removes it.
fieldById :: HasFields s => FieldName -> Lens' s (Maybe Field)
fieldById k = fields . lens (M.lookup k) (\fs mf -> M.alter (const mf) k fs)

-- | Find an existing field by name amongst these fields or create a new one
-- with this name. It can be applied to the top-level definition or to a
-- field's sub-fields. 'field' and 'fieldById' are the primary way by which
-- fields are created and modified on definitions:
--
-- @
-- def
-- -- (1)
-- & field "First" . spec <>~ mandatory <> duplicate <> instanceLabel
-- -- (2)
-- & field "Name" . field "Shown if yes" %~ \f -> f
--     & spec <>~ ifCondition "=Yes" <> readOnly
--     & field "Sub" . spec <>~ text
--     & field "Other" . spec <>~ number
-- @
--
-- where
--  (1) create or find field "First" and augment (<>~) its spec with @mandatory@, @duplicate@, and @$instanceLabel@.
--  (2) create field "Name", subfield "Shown if yes", and update the subfield
--  to have a condition @=Yes@ and @$readonly@, plus two more sub-sub-fields.
--
-- Fails if several siblings have the same name; use 'fieldById' to refer to a
-- field by its id specifically.
field :: (HasCallStack, HasFields s) => Text -> Lens' s Field
field name = fields . lens get_field set_field
  where
    get_field fs = case filter ((== name) . (.fieldName)) (M.elems fs) of
      [] -> new_field fs
      [f] -> f
      dups -> error $ "field: ambiguous name " ++ show name ++ ", use fieldById with one of " ++ show (map (.fieldId) dups)
    set_field fs f = M.insert f.fieldId f fs
    new_field fs = Field
      { fieldName = name
      , fieldSpec = mempty
      , fieldFields = mempty
      , fieldId = NewField name
      , fieldOrder = 1 + maximum (0 : map (.fieldOrder) (M.elems fs))
      }

spec :: Lens' Field FieldSpec
spec = lens (.fieldSpec) (\f s -> f{fieldSpec = s})

description :: Lens' FieldSpec FieldDescription
description = lens (.specDescription) (\s d -> s{specDescription = d})

isMandatory, isDuplicable :: Lens' FieldSpec Bool
isMandatory  = lens (fromMaybe False . getLast . (.specMandatory))  (\s b -> s{specMandatory = Last (Just b)})
isDuplicable = lens (fromMaybe False . getLast . (.specDuplicable)) (\s b -> s{specDuplicable = Last (Just b)})

--------------------------------------------------------------------------------
-- * RecordM JSON representation
--------------------------------------------------------------------------------

-- | Encode a 'Definition' as RecordM expects it when creating or updating one.
--
-- Fields are numbered in pre-order, siblings sorted by 'fieldOrder'.
definitionValue :: Definition -> Value
definitionValue Definition{..} =
  object
    [ "name" .= defName
    , "description" .= defDescription
    , "fieldDefinitions" .= evalState (fieldValues defFieldDefinitions) 0
    , "state" .= stateValue defState
    , "version" .= defVersion
    , "id" .= defId
    ]
  where
    stateValue :: DefinitionState -> Text
    stateValue EnabledDefinition = "enabled"

    fieldValues :: Map FieldName Field -> State Int [Value]
    fieldValues = mapM fieldValue . sortOn (.fieldOrder) {- sort by order before picking the new order value -}
                                  . M.elems

    fieldValue :: Field -> State Int Value
    fieldValue f = do
      order <- modify (+1) >> get
      subs <- fieldValues f.fieldFields
      let -- RecordM takes negative ids for new fields
          idText (ExistingField i) = show i
          idText (NewField _) = show (-100000000 - order)
      pure $ object $
        [ "name" .= f.fieldName
        , "description" .= descText (f ^. spec . description)
        , "duplicable" .= (f ^. spec . isDuplicable)
        , "required" .= (if f ^. spec . isMandatory then Just "mandatory" else Nothing :: Maybe Text)
        , "order" .= order
        , "id" .= idText f.fieldId
        ]
        ++ [ "fields" .= subs | not (null subs) ]
        ++ [ "condition" .= condText c | Last (Just c) <- [f.fieldSpec.specCondition] ]

-- | Decode a 'Definition' as RecordM returns it.
parseDefinition :: Value -> Aeson.Parser Definition
parseDefinition = withObject "Definition" $ \jsonDef -> do

  -- def
  defId   <- Just . DefId <$> jsonDef .: "id"
  defName <- jsonDef .: "name"
  defDescription <- jsonDef .:? "description" .!= ""
  defState <- parseDefState =<< jsonDef .: "state"
  defVersion <- jsonDef .: "version"

  -- fields
  defFieldDefinitions  <- parseFields =<< jsonDef .: "fieldDefinitions"

  return Definition{..}
  where
    parseFields :: [Object] -> Aeson.Parser (Map FieldName Field)
    parseFields = fmap M.fromList . mapM parseField

    parseField :: Object -> Aeson.Parser (FieldName, Field)
    parseField fieldDef = do
      fieldName <- fieldDef .: "name"
      specDescription <- parseFieldDescriptionAeson =<< fieldDef .:? "description" .!= ""
      specDuplicable <- Last . Just <$> fieldDef .: "duplicable"
      specMandatory <- Last . Just <$> (parseFieldRequired =<< fieldDef .: "required")
      specCondition <- Last <$> (parseCondition =<< fieldDef .:? "condition")
      fieldId <- ExistingField <$> fieldDef .: "id"
      fieldOrder <- fieldDef .: "order"
      fieldFields <- parseFields =<< fieldDef .: "fields"
      let fieldSpec = FieldSpec{..}
      return (fieldId, Field{..})

    parseFieldDescriptionAeson :: Text -> Aeson.Parser FieldDescription
    parseFieldDescriptionAeson t = either fail pure (runFieldDescriptionParser t)

    parseCondition :: Maybe Text -> Aeson.Parser (Maybe Condition)
    parseCondition = traverse $ \t ->
      maybe (fail $ "Unexpected condition format: " ++ show t) pure (parseCondText t)

    parseDefState :: Text -> Aeson.Parser DefinitionState
    parseDefState = \case
      "enabled" -> return EnabledDefinition
      s -> fail $ "Unexpected DefinitionState: " ++ show s

    parseFieldRequired :: Maybe Text -> Aeson.Parser Bool
    parseFieldRequired = \case
      Just "mandatory" -> return True
      Nothing -> return False
      Just r -> fail $ "Unexpected required value: " ++ show r

--------------------------------------------------------------------------------
-- ** Parsing keywords (with Megaparsec)
--------------------------------------------------------------------------------

runFieldDescriptionParser :: Text -> Either String FieldDescription
runFieldDescriptionParser t = case runParser parseFieldDescription "runFieldDescriptionParser" t of
  Left e -> Left $ "Failed to parse description " ++ show t ++ ":\n" ++ errorBundlePretty e
  Right d
    | descText d /= t -> Left $ "Description " ++ show t ++ " would be sent back as " ++ show (descText d)
    | otherwise -> Right d

parseFieldDescription :: Parsec Void Text FieldDescription
parseFieldDescription =
  FieldDesc <$> many (KwPart <$> parseKeyword <|> RawPart <$> takeWhile1P Nothing (/= '$')) <* eof

parseKeyword :: Parsec Void Text Keyword
parseKeyword = (char '$' *>) $
      named "instanceLabel" InstanceLabelKw
  <|> named "instanceDescription" InstanceDescriptionKw
  <|> named "readonly" ReadOnlyKw
  <|> named "number" NumberKw
  <|> named "datetime" DateTimeKw
  <|> named "date" DateKw
  <|> named "time" TimeKw
  <|> named "text" TextKw
  <|> parseListKeyword
  <|> parseReferencesKeyword
  <|> parseRefKeyword
  <|> (RawKw <$> takeWhileP Nothing (not . isSpace)) -- fallback
  where
    named n k = try (string n <* notFollowedBy (satisfy isAlphaNum)) $> k

parseListKeyword :: Parsec Void Text Keyword
parseListKeyword = do
  items <- char '[' *> sepBy1 (takeWhile1P Nothing (`notElem` [',', ']'])) (char ',') <* char ']'
  let labels = map (\l -> fromMaybe l (T.stripSuffix "*" l)) items
  return $ ListKw labels (T.stripSuffix "*" =<< listToMaybe (filter ("*" `T.isSuffixOf`) items))

parseRefKeyword :: Parsec Void Text Keyword
parseRefKeyword = do
  string "ref" *> char '('
  defName <- manyTill anySingle (char ',')
  queryString <- manyTill anySingle (char ')')
  return $ RefKw (tokensToChunk (Proxy @Text) defName) (byText (tokensToChunk (Proxy @Text) queryString))

parseReferencesKeyword :: Parsec Void Text Keyword
parseReferencesKeyword = do
  string "references" *> char '('
  defName <- manyTill anySingle (char ',')
  fieldName <- manyTill anySingle (lookAhead (char ')'))
  char ')'
  return $ ReferencesKw (tokensToChunk (Proxy @Text) defName) (tokensToChunk (Proxy @Text) fieldName)
