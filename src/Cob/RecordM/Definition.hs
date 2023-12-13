{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Cob.RecordM.Definition
  ( -- * Static Definition
    Definition(..), Field(..), Condition(..), DefinitionState(..), FieldRequired(..)
    -- * Definition Quoting (DSL)
  , FieldName, getFieldOrder, getFieldId, DefinitionQ, fromDSL, runDSL
  , (|=), (|+), (|=!), (|=*), (|=!*), mandatory, duplicable
  , (===), (?)
    -- ** Keywords
  , Keyword, keyword, instanceLabel, instanceDescription, readOnly, number
  , datetime, date, text, list
    -- ** Debugging
  , _testBuild
  ) where

import Control.Monad
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader
import Data.Text
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Bifunctor
import Data.Maybe
import qualified Data.Text as T

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
    , fieldDescription :: Text
    , fieldDuplicable :: Bool
    , fieldRequired :: FieldRequired
    , fieldFields :: Maybe (Map FieldName Field)
    , fieldCondition :: Maybe Condition
    , fieldId :: FieldName
    }
    deriving Show

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
  deriving Show


-- * The Definition Builder DSL / Monad
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
-- TODO: Export as abstract
newtype FieldName = UnsafeFieldName Int -- Tagged by identifier
  deriving (Eq, Ord, Show)
getFieldOrder :: FieldName -> Int
getFieldOrder (UnsafeFieldName i) = i
getFieldId :: FieldName -> String
getFieldId (UnsafeFieldName i) = show (-100000000 - i)

type DefinitionQ = DefinitionQM ()
type DefinitionQM = Free DefinitionQF

data DefinitionQF next
  = Declare Text Text (FieldName -> next)
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
(|=) :: Text -> Text -> DefinitionQM FieldName
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
(|=!) :: Text -> Text -> DefinitionQM FieldName
(|=!) lhs rhs = do
  field <- declare lhs rhs
  mandatory field
  return field

-- | Declare a duplicable field
(|=*) :: Text -> Text -> DefinitionQM FieldName
(|=*) lhs rhs = do
  field <- declare lhs rhs
  duplicable field
  return field
  
-- | Declare a duplicable and mandatory field
(|=!*) :: Text -> Text -> DefinitionQM FieldName
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
-- $ Keywords
--------------------------------------------------------------------------------

type Keyword = Text

keyword :: Text -> Keyword
keyword = ("$" <>)

instanceLabel, instanceDescription, readOnly, number, datetime, date, text :: Keyword
instanceLabel       = keyword "instanceLabel"
instanceDescription = keyword "instanceDescription"
readOnly            = keyword "readonly"
number              = keyword "number"
datetime            = keyword "datetime"
date                = keyword "date"
text                = keyword "text"

list :: [Text] -> Keyword
list labels = keyword ("[" <> T.intercalate "," labels <> "]")

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
    findField (p:ps) f m = findField ps f (fromMaybe mempty $ fieldFields (m M.! p))
  
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
        let field1 = field0{fieldFields = Just $
              editFieldMap paths upd (fromMaybe mempty field0.fieldFields)}
         in M.insert path field1 field_map

  

simpleDefinition :: Text -> Text -> Definition
simpleDefinition name descr
  = Definition
    { defName = name
    , defDescription = descr
    , defState = EnabledDefinition
    , defFieldDefinitions = mempty
    }

simpleField :: FieldName -> Text -> Text -> Field
simpleField fid name descr
  = Field
    { fieldName = name
    , fieldDescription = descr
    , fieldDuplicable = False
    , fieldRequired = FieldNotRequired
    , fieldFields = Nothing
    , fieldCondition = Nothing
    , fieldId = fid
    }

--------------------------------------------------------------------------------
-- Util: Fresh
--------------------------------------------------------------------------------

newtype Fresh m a = Fresh (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadState Int)

runFresh :: Monad m => Fresh m a -> m a
runFresh (Fresh f) = evalStateT f 1

--------------------------------------------------------------------------------
-- Small test to compile a Definition
--------------------------------------------------------------------------------

done :: Monad m => m [a]
done = return []


aux :: DefinitionQM ()
aux = do
  "Teste2"    |=  ""
  "Mandatory" |=! (readOnly |+| instanceLabel)
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

(|+|) :: Text -> Text -> Text
(|+|) d1 d2 = d1 <> " " <> d2
