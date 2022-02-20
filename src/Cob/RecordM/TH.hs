{-# LANGUAGE TemplateHaskellQuotes, LambdaCase #-}

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
module Cob.RecordM.TH (mkRecord) where


import Data.Char (toLower)
import Data.Maybe (catMaybes, listToMaybe)

import Control.Monad (zipWithM, foldM)

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.String (fromString)

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, object, (.=), withObject, (.:), (.:?))

import Cob.RecordM (Record(..), Ref(..))

import Language.Haskell.TH

data SupportedRecordType = StringT
                         | TextT
                         | ByteStringT
                         | RefT
                         | IntT
                         | DoubleT
                         | MaybeT SupportedRecordType
                         | OtherT Name

mkToJSON :: [SupportedRecordType] -> [Field] -> Q Exp
mkToJSON tys fields = [e| object (catMaybes $(ListE <$> zipWithM mkToJSONItem tys fields)) |]
    where
        mkToJSONItem :: SupportedRecordType -> Field -> Q Exp
        mkToJSONItem ty field = [e| (fromString $(mkString field) .=) <$> $(mods ty) ($(toMaybe ty) $(mkVarE $ fieldNormalizeVar field)) |]
            where
                mods RefT        = [e| ((show . ref_id) <$>) |]
                mods IntT        = [e| (show <$>)            |]
                mods DoubleT     = [e| (show <$>)            |]
                mods (MaybeT mt) = [e| $(mods mt)            |]
                mods _           = [e| id                    |]

                toMaybe (MaybeT _ ) = [e| id   |]
                toMaybe _           = [e| Just |]

mkParseJSON :: Name -> [SupportedRecordType] -> [Field] -> Q Exp
mkParseJSON tyConName tys fields = do
        finalStmt <- NoBindS . AppE (VarE $ mkName "return") <$> foldM foldConArgs (ConE tyConName) (zip tys fields)
        LamE [VarP $ mkName "v"] . DoE Nothing . (++ [finalStmt]) <$> zipWithM mkParseJSONItem tys fields
    where
        mkParseJSONItem :: SupportedRecordType -> Field -> Q Stmt
        mkParseJSONItem (MaybeT _) field =
            BindS <$> [p|  $(mkVarP $ fieldNormalizeVar field)  |] <*> [e| v .:? fromString $(mkString $ fieldNormalize field) |]
        mkParseJSONItem _ field =
            BindS <$> [p| [$(mkVarP $ fieldNormalizeVar field)] |] <*> [e| v .: fromString $(mkString $ fieldNormalize field) |]

        foldConArgs :: Exp -> (SupportedRecordType, Field) -> Q Exp
        exp `foldConArgs` (ty, field) = AppE exp <$> [e| $(mods ty) $(mkVarE $ fieldNormalizeVar field) |]

        mods :: SupportedRecordType -> Q Exp
        mods ty = case ty of
              RefT       -> [e| Ref . read                            |]
              IntT       -> [e| read                                  |]
              DoubleT    -> [e| read                                  |]
              MaybeT ty2 -> [e| ($(mods ty2) <$>) . (listToMaybe =<<) |]
              _          -> [e| id                                    |]
              
mkRecordPat :: Name -> [SupportedRecordType] -> [Field] -> Q Pat
mkRecordPat tyConName tyConArgList fields = do
    pats <- zipWithM mkArgPat tyConArgList fields
    return $ ConP tyConName pats
    where
        mkArgPat :: SupportedRecordType -> Field -> Q Pat
        mkArgPat ty field = case ty of
            StringT     -> defaultRet
            TextT       -> defaultRet
            ByteStringT -> defaultRet
            RefT        -> defaultRet -- ConP 'Ref . (:[]) <$> mkVarP (fieldNormalizeRef field)
            IntT        -> defaultRet
            DoubleT     -> defaultRet
            MaybeT _    -> defaultRet
            OtherT _    -> defaultRet
            where
                defaultRet :: Q Pat
                defaultRet = mkVarP $ fieldNormalizeVar field

parseTyConArgList :: [Type] -> Q [SupportedRecordType]
parseTyConArgList = mapM parseTyConArg
    where
        parseTyConArg :: Type -> Q SupportedRecordType
        parseTyConArg = \case
            t@(ConT conTy)
              | conTy == ''String     -> return StringT
              | conTy == ''Text       -> return TextT
              | conTy == ''ByteString -> return ByteStringT
              | conTy == ''Int        -> return IntT
              | conTy == ''Double     -> return DoubleT
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
            parseJSON = withObject $(mkString $ show tyName) $
                $(mkParseJSON tyConName tyConArgList fields)

        instance Record $(mkTy tyName) where
            definition = $(mkString definitionName)
      |]

--- Util
fieldNormalize :: Field -> String
fieldNormalize = map (toLower . \c -> if c == ' ' then '_' else c)

fieldNormalizeVar :: Field -> String
fieldNormalizeVar = ("___" <>) . fieldNormalize

fieldNormalizeRef :: Field -> String
fieldNormalizeRef = (<> "_id") . fieldNormalizeVar

mkTy :: Name -> Q Type
mkTy = return . ConT 

mkString :: String -> Q Exp
mkString = return . LitE . StringL

mkVarE :: String -> Q Exp
mkVarE = return . VarE . mkName

mkVarP :: String -> Q Pat
mkVarP = return . VarP . mkName

