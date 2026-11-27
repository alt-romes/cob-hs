{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Cob.RecordM.Definition
  ( -- * Static Definition
    Definition(..), Field(..), FieldDescription(..), DescPart(..), Condition(..), DefinitionState(..), FieldRequired(..)
  , simpleDefinition, field
  , FieldName, getFieldOrder, getFieldId

    -- * Keywords
  , Keyword(..), instanceLabel, instanceDescription, readOnly, number
  , datetime, date, time, text, list, listDefault, dollarRef, dollarReferences

    -- * RecordM JSON representation
  , DefinitionId(..), definitionValue, parseDefinition
  , descText, knownKws

    -- ** Debugging
  , parseFieldDescription, parseKeyword

  , -- * Modifying and constructing definition using lenses.
  , (&), (%~), Endo(..)
  ) where

import Data.String
import Data.Aeson
import Data.Functor.Identity
import Data.Monoid
import Lens.Micro
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

import Cob.RecordM.Query
import Data.Coerce
import Servant.API (ToHttpApiData(toUrlPiece))
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char (string, char)
import Data.Functor (($>))
import Data.Proxy
import Data.Char (isSpace, isAlphaNum)
-- import Text.Megaparsec.Debug

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
      -- ^ Keyed by 'NewField' for fields created with the DSL, otherwise by
      -- the 'ExistingField' id picked up from the server.
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

data FieldSpec = FieldSpec
  { specDescription :: FieldDescription
  , specMandatory   :: Last Bool
  , specDuplicable  :: Last Bool
  , specCondition   :: Last Condition
  }

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

-- | Abstract type representing a definition's field name.
-- Useful for defining conditionals (e.g. see '(===)').
data FieldName
  = ExistingField Int
  -- ^ The id of a field which exists in the server
  | NewField Text
  -- ^ A field created here, doesn't have a number id yet.
  deriving (Eq, Ord, Show)

-- -- | New fields are ordered by declaration.
-- getFieldOrder :: Field -> Int
-- getFieldOrder Field{fieldOrder = Just o} = o
-- getFieldOrder Field{fieldId = NewField n} = n
-- getFieldOrder Field{fieldId = ExistingField i} = i
--
-- -- | The id sent to the server. New fields take negative ids.
-- getFieldId :: FieldName -> String
-- getFieldId (ExistingField i) = show i
-- getFieldId (NewField n) = show (-100000000 - n)

data Condition
  = Equals
     { lhs :: Text -- ???
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
  | ListKw [Text] (Maybe Text)
  -- ^ Options and the default option
  | RefKw { refKwDefName :: Text
          , refKwQuery   :: Query Void
          -- ^ This reference can be of any type.
          -- Use coerce to make it a query ()
          }
  | ReferencesKw { referencesKwDefName :: Text
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
list ls = kwDesc (ListKw ls Nothing)

-- | A list with a default option
listDefault :: [Text] -> Text -> FieldDescription
listDefault ls def = kwDesc (ListKw ls (Just def))

dollarRef :: Text -> Query a -> FieldDescription
dollarRef t q = kwDesc (RefKw t (coerce {-refkw holds a typeless ref-} q))

dollarReferences :: Text -> Text -> FieldDescription
dollarReferences t q = kwDesc (ReferencesKw t q)

kwDesc :: Keyword -> FieldDescription
kwDesc kw = FieldDesc [KwPart kw]

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

field :: FieldName -> Lens' Definition Field
field name = lens get_field set_field where

  get_field def = case M.lookup name def.defFieldDefinitions of
    Nothing -> new_field def name
    Just existing -> existing

  new_field _ (ExistingField i) = error $ "existing field " ++ show i ++ " was not found in definition fields lookup"
  new_field d (NewField name) = Field
    { fieldName = name
    , fieldDescription = mempty
    , fieldDuplicable = False
    , fieldRequired = FieldNotRequired
    , fieldFields = mempty
    , fieldCondition = Nothing
    , fieldId = NewField name
    , fieldOrder = getNextOrderId d
    }

  -- overwrites existing field with that name if .~ set instead of %~.
  -- that's also why most operators are Field -> Field functions.
  -- if you want to use .~, you'll need to apply them to something.
  set_field def f = M.insert name f def.defFieldDefinitions

named :: Text -> Lens' Definition Field

-- | Allow creating new fields with @"My new field"  instanceLabel <> 
instance IsString _ where

-- | Pick the next available order id to append a new field to the definition
getNextOrderId :: Definition -> Int
getNextOrderId d = go (M.elems d.defFieldDefinitions)
  where
    go :: [Field] -> Int
    go fs = maximum [ max f.fieldOrder (go (M.elems f.fieldFields)) | f <- fs ]

--------------------------------------------------------------------------------
-- * RecordM JSON representation
--------------------------------------------------------------------------------

-- | Encode a 'Definition' as RecordM expects it when creating or updating one.
definitionValue :: Definition -> Value
definitionValue Definition{..} =
  object
    [ "name" .= defName
    , "description" .= defDescription
    , "fieldDefinitions" .= map fieldValue (M.elems defFieldDefinitions)
    , "state" .= stateValue defState
    , "version" .= defVersion
    , "id" .= defId
    ]
  where
    stateValue :: DefinitionState -> Text
    stateValue EnabledDefinition = "enabled"

    fieldValue :: Field -> Value
    fieldValue f@Field{..} =
      object $
        [ "name" .= fieldName
        , "description" .= descText fieldDescription
        , "duplicable" .= fieldDuplicable
        , "required" .= requiredValue fieldRequired
        , "order" .= getFieldOrder f
        , "id" .= getFieldId fieldId
        ]
        ++ [ "fields" .= map fieldValue (M.elems fieldFields) | not (M.null fieldFields) ]
        ++ [ "condition" .= ("=" <> rhs) | Just Equals{rhs} <- [fieldCondition] ]

    requiredValue :: FieldRequired -> Text
    requiredValue MandatoryField = "mandatory"
    requiredValue FieldNotRequired = ""

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
      fieldDescription <- parseFieldDescriptionAeson =<< fieldDef .:? "description" .!= ""
      fieldDuplicable <- fieldDef .: "duplicable"
      fieldRequired <- parseFieldRequired =<< fieldDef .: "required"
      fieldId <- ExistingField <$> fieldDef .: "id"
      fieldOrder <- fieldDef .: "order"

      fieldFields <- parseFields =<< fieldDef .: "fields"
      fieldCondition <- parseCondition =<< fieldDef .:? "condition"

      return (fieldId, Field{..})

    parseFieldDescriptionAeson :: Text -> Aeson.Parser FieldDescription
    parseFieldDescriptionAeson t = either fail pure (runFieldDescriptionParser t)

    parseCondition :: Maybe String -> Aeson.Parser (Maybe Condition)
    parseCondition Nothing = pure Nothing
    parseCondition (Just cond) = do
      -- Only supports simple conditions for now, like "=Yes" or "=No"
      -- TODO: seems horrible, need to fix!
      case cond of
        ('=':rhs) -> return $ Just (Equals "" (pack rhs))
        _ -> fail $ "Unexpected condition format: " ++ show cond

    parseDefState :: Text -> Aeson.Parser DefinitionState
    parseDefState = \case
      "enabled" -> return EnabledDefinition
      s -> fail $ "Unexpected DefinitionState: " ++ show s

    parseFieldRequired :: Maybe Text -> Aeson.Parser FieldRequired
    parseFieldRequired = \case
      Just "mandatory" -> return MandatoryField
      Nothing -> return FieldNotRequired
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
