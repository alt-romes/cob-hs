{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

-- | Automatically derive 'Record' given its fields and definition name
-- A definition called \"Dogs\" with fields ["Owner Name", "Dog Name"], and type that will instance 'Record',
--
-- @
-- data Dog = Dog Text Text
-- @
--
-- should automatically generate
--
-- @
-- instance ToJSON Dog where
--      toJSON (Dog ownerName dogName) = object
--          [ "Owner Name" .= ownerName
--          , "Dog Name" .= dogName ]
-- instance FromJSON Dog where
--      parseJSON = withObject "Dogs record" \v -> do
--          [ownerName] <- v .: "owner_name"
--          [dogName]   <- v .: "dog_name"
--          return (Dog ownerName dogName)
-- instance Record Dog where
--      definition = \"Dogs\"
-- @
--
-- when the following is in the module
--
-- @
-- mkRecord ''Dog \"Dogs\" ["Owner Name", "Dog Name"]
-- @
--
-- Check 'SupportedRecordType' for which primitive types are supported. All
-- JSON enabled types are supported as well.
-- The ID field will be treated specially as a field that contains a self reference to the record
module Cob.RecordM.TH (mkRecord, SupportedRecordType, mkRecordEnum) where

import Data.Time

import Data.Char (toLower)
import Data.Maybe (fromJust, catMaybes, listToMaybe)

import Control.Monad (zipWithM, foldM)

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.String (fromString)

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, object, (.=), withObject, (.:), (.:?), Value(String))

import Cob.RecordM.Record (Record(..))
import Cob.Ref (Ref(..))

import Language.Haskell.TH
import qualified Data.Text as T

-- | All other types use the default ToJSON implementation
data SupportedRecordType = StringT
                         | TextT
                         | ByteStringT
                         | RefT
                         | IntT
                         | FloatT
                         | DoubleT
                         | DateTimeT
                         | MaybeT !SupportedRecordType
                         | OtherT !Name

mkToJSON :: [SupportedRecordType] -> [Field] -> Q Exp
mkToJSON tys fields = [e| object (catMaybes $(ListE <$> zipWithM mkToJSONItem tys fields)) |]
    where
        mkToJSONItem :: SupportedRecordType -> Field -> Q Exp
        mkToJSONItem ty field = [e| (fromString $(mkString field) .=) <$> $(mods ty) ($(toMaybe ty) $(mkVarE $ fieldNormalizeVar field)) |]
            where
                -- There is no special case for serializing the ID of a record
                -- differently, so we serialize ID as we do all other refs.
                mods RefT        = [e| ((show . ref_id) <$>) |]
                mods IntT        = [e| (show <$>)            |]
                mods FloatT      = [e| (show <$>)            |]
                mods DoubleT     = [e| (show <$>)            |]
                mods DateTimeT   = [e| (formatTime undefined "%s" <$>) |]
                mods (MaybeT mt) = [e| $(mods mt)            |]
                mods _           = [e| id                    |]

                toMaybe (MaybeT _ ) = [e| id   |]
                toMaybe _           = [e| Just |]

mkParseJSON :: Name -> [SupportedRecordType] -> [Field] -> Q Exp
mkParseJSON tyConName tys fields = do
        finalStmt <- NoBindS . AppE (VarE $ mkName "return") <$> foldM foldConArgs (ConE tyConName) (zip tys fields)
#if __GLASGOW_HASKELL__ >= 900
        LamE [VarP $ mkName "v"] . DoE Nothing . (++ [finalStmt]) <$> zipWithM mkParseJSONItem tys fields
#else
        LamE [VarP $ mkName "v"] . DoE . (++ [finalStmt]) <$> zipWithM mkParseJSONItem tys fields
#endif
    where
        mkParseJSONItem :: SupportedRecordType -> Field -> Q Stmt
        mkParseJSONItem (MaybeT _) field =
            BindS <$> [p|  $(mkVarP $ fieldNormalizeVar field)  |] <*> [e| v .:? fromString $(mkString $ fieldNormalize field) |]
        mkParseJSONItem t field
          -- The special case for parsing the ID of a record differently from
          -- how we parse references to other records in fields of a record
          | field == "ID", RefT <- t =
            BindS <$> [p| $(mkVarP $ fieldNormalizeVar field) |] <*> [e| parseJSON (Object v) |] -- parse the Ref directly, not under a field
        mkParseJSONItem _ field =
            BindS <$> [p| [$(mkVarP $ fieldNormalizeVar field)] |] <*> [e| v .: fromString $(mkString $ fieldNormalize field) |]

        foldConArgs :: Exp -> (SupportedRecordType, Field) -> Q Exp
        expr `foldConArgs` (ty, field)
          -- Special ID case
          | field == "ID", RefT <- ty  = AppE expr <$> [e| $(mkVarE $ fieldNormalizeVar field) |]
        expr `foldConArgs` (ty, field) = AppE expr <$> [e| $(mods ty) $(mkVarE $ fieldNormalizeVar field) |]

        mods :: SupportedRecordType -> Q Exp
        mods ty = case ty of
              RefT       -> [e| Ref Nothing . read                    |]
              IntT       -> [e| read                                  |]
              DoubleT    -> [e| read                                  |] -- TODO: Is this right? how to account for commas etc
              DateTimeT  -> [e| fromJust . parseTimeM False undefined "%s" |]
              MaybeT ty2 -> [e| ($(mods ty2) <$>) . (listToMaybe =<<) |]
              _          -> [e| id                                    |]
              
mkRecordPat :: Name -> [SupportedRecordType] -> [Field] -> Q Pat
mkRecordPat tyConName tyConArgList fields = do
    pats <- zipWithM mkArgPat tyConArgList fields
#if __GLASGOW_HASKELL__ >= 902
    return $ ConP tyConName [] pats
#else
    return $ ConP tyConName pats
#endif
    where
        mkArgPat :: SupportedRecordType -> Field -> Q Pat
        mkArgPat ty field = case ty of
            StringT     -> defaultRet
            TextT       -> defaultRet
            ByteStringT -> defaultRet
            RefT        -> defaultRet -- ConP 'Ref . (:[]) <$> mkVarP (fieldNormalizeRef field)
            IntT        -> defaultRet
            FloatT      -> defaultRet
            DoubleT     -> defaultRet
            MaybeT _    -> defaultRet
            DateTimeT   -> defaultRet
            OtherT _    -> defaultRet
            where
                defaultRet :: Q Pat
                defaultRet = mkVarP $ fieldNormalizeVar field

parseTyConArgList :: [Type] -> Q [SupportedRecordType]
parseTyConArgList = mapM parseTyConArg
    where
        parseTyConArg :: Type -> Q SupportedRecordType
        parseTyConArg = \case
            ConT conTy
              | conTy == ''String     -> return StringT
              | conTy == ''Text       -> return TextT
              | conTy == ''ByteString -> return ByteStringT
              | conTy == ''Int        -> return IntT
              | conTy == ''Float      -> return FloatT
              | conTy == ''Double     -> return DoubleT
              | conTy == ''UTCTime    -> return DateTimeT
              | otherwise             -> do
                  tyInfo <- reify conTy
                  case tyInfo of
                    TyConI (TySynD _ _ synTy) -> parseTyConArg synTy -- If type is a type synonym, parse synonym type instead
                    _ -> return (OtherT conTy)                       -- Type isn't a type synonym, so it's just something else
            t@(AppT (ConT conTy) t2)
                | conTy == ''Ref -> return RefT
                | conTy == ''Maybe -> MaybeT <$> parseTyConArg t2
                | otherwise -> fail $ "Records with applied constructed types other than 'Ref' and 'Maybe' are not supported. Attempted: " <> show t
            t -> fail $ "Unknown unsupported: " <> show t

recordTypeInfo :: Name -> Q (Name, Name, [Type])
recordTypeInfo ty = do
    tyInfo <- reify ty
    case tyInfo of
      TyConI tyConDecl ->
          case tyConDecl of
            DataD    _ tyName [] _ [NormalC tyConName tyConArgList] _ -> return (tyName, tyConName, map snd tyConArgList)
            NewtypeD _ tyName [] _ (NormalC tyConName tyConArgList) _ -> return (tyName, tyConName, map snd tyConArgList)
            DataD    _ tyName [] _ [RecC tyConName tyConArgList]    _ -> return (tyName, tyConName, map (\(_,_,t) -> t) tyConArgList)
            NewtypeD _ tyName [] _ (RecC tyConName tyConArgList)    _ -> return (tyName, tyConName, map (\(_,_,t) -> t) tyConArgList)
            _ -> fail "makeRecord should be called on a datatype or newtype declaration with only one normal or record constructor and no type variables"
      _ -> fail "makeRecord should be called on a datatype or newtype"

-- | mkRecord is able to fully generate instances from ToJSON, FromJSON and Record for a data type in accordance with @RecordM@
--
-- It receives the Type, the name of the table, and a list of ordered fields (matching the type constructor arguments)
--
-- @
-- data Owner = Owner Text
-- data Dog = Dog (Ref Owner) Text Int
--
-- mkRecord ''Owner "Owners" ["Owner Name"]
-- mkRecord ''Dog "Dogs" ["Owner", "Dog Name", "Dog Age"]
-- @
--
-- The type can be constructed in a number of ways. Type synonyms are automatically followed
-- By default:
--
-- 'Maybe' will represent an optional @RecordM@ field, that may or may not exist for a given record.
--
-- 'Ref' will automatically parse a reference to another table
--
--      The exception to 'Ref' is the field "ID" which identifies the reference to the record itself.
--      This specific 'id :: Ref RecordType' field is parsed and serialized specially:
--        * When parsing, the self reference is found in the body _source rather than a layer deeper as happens for other references
--        * When serializing, the self reference field is serialized as expected ("ID" = integer_value_of_id)
--      This is only mostly useful to fetch the record ID directly into the
--      record structure, without having to construct it from the returned
--      reference posteriorly. TODO: An alternative design for most RecordM
--      methods would be to require every data type to have a self reference
--      field, instead of always returning the reference to the record in a pair.
--
--      TODO: What happens if you update a record whose datatype includes the ID? You basically send a "set ID" upstream...?
-- 
-- 'Int' will automatically parse an Int
-- 'Double' will automatically parse a Double
--
-- Note: If a double from @RecordM@ is incorrectly described as an int, a no parse error will be thrown
type Field = String
mkRecord :: Name -> String -> [Field] -> Q [Dec]
mkRecord ty definitionName fields = do
    (tyName, tyConName, tyConArgList') <- recordTypeInfo ty
    tyConArgList <- parseTyConArgList tyConArgList'
    [d|
        instance ToJSON $(mkTy tyName) where
            toJSON $(mkRecordPat tyConName tyConArgList fields) = $(mkToJSON tyConArgList fields)

        instance FromJSON $(mkTy tyName) where
            parseJSON = withObject $(mkString $ show tyName)
                $(mkParseJSON tyConName tyConArgList fields)

        instance Record $(mkTy tyName) where
            definition = $(mkString definitionName)
      |]

--- Util
fieldNormalize :: Field -> String
fieldNormalize = map (toLower . \c -> if c == ' ' then '_' else c)

fieldNormalizeVar :: Field -> String
fieldNormalizeVar = ("___" <>) . fieldNormalize

-- fieldNormalizeRef :: Field -> String
-- fieldNormalizeRef = (<> "_id") . fieldNormalizeVar

mkTy :: Name -> Q Type
mkTy = return . ConT 

mkString :: String -> Q Exp
mkString = return . LitE . StringL

mkVarE :: String -> Q Exp
mkVarE = return . VarE . mkName

mkVarP :: String -> Q Pat
mkVarP = return . VarP . mkName

--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

-- | Create JSON instances for serializing an enum (as in the keyword @$[]@ in RecordM, e.g. @$[Option 1,Value 2]@)
--
-- @
-- data Status = Pending | Confirmed
--
-- -- Specify the matching RecordM strings for serializing Status
-- mkRecordEnum ''Status ["Pending Status", "Confirmed Status"]
--
-- -- Will produce code like
-- instance ToJSON Status where
--   toJSON Pending = String "Pending Status"
--   toJSON Confirmed = String "Confirmed Status"
-- @
mkRecordEnum :: Name -> [String] -> Q [Dec]
mkRecordEnum ty tags = do
  tyInfo <- reify ty
  case tyInfo of
    TyConI (DataD _ tyName [] _ consList _) -> do
      if length consList /= length tags then
        fail $ "mkRecordEnum: Number of constructors (" ++ show consList ++ ") does not match number of tags (" ++ show tags ++ ")"
      else do
        toJSONInst <-
          instanceD (cxt []) [t| ToJSON $(conT tyName) |] [funD 'toJSON (zipWith mkToJSONClause consList tags)]
        fromJSONInst <-
          instanceD (cxt []) [t| FromJSON $(conT tyName) |] [funD 'parseJSON [mkFromJSONClause consList]]
        return [toJSONInst, fromJSONInst]
    _ -> fail "mkRecordEnum should be called on an enum datatype"
  where
    mkToJSONClause :: Con -> String -> Q Clause
    mkToJSONClause (NormalC conName []) tag = clause [conP conName []] (normalB [e| String ($(varE 'T.pack) $(litE $ stringL tag)) |]) []
    mkToJSONClause _ _ = fail "mkRecordEnum: Only normal constructors without arguments are supported (Enum Types)"

    mkFromJSONClause :: [Con] -> Q Clause
    mkFromJSONClause consList = clause [] (normalB
        [e| withText $(litE $ stringL (show ty)) $ \v -> $(
              caseE (appE (varE 'T.unpack) (varE 'v)) (zipWith mkFromJSONCase consList tags)
          )
        |]) []

    mkFromJSONCase :: Con -> String -> Q Match
    mkFromJSONCase (NormalC conName []) tag = match (litP $ stringL tag) (normalB [e| pure $(conE conName) |]) []
    mkFromJSONCase _ _ = fail "mkRecordEnum: Only normal constructors without arguments are supported (Enum Types)"
