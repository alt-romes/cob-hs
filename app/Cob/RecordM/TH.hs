{-# LANGUAGE TemplateHaskellQuotes, LambdaCase #-}

-- | Automatically derive 'Record' given its fields and definition name
-- A definition called \"Dogs\" with fields ["Owner Name", "Dog Name"], and type that will instance 'Record',
-- @
-- data Dog = Dog Text Text
-- @
-- should automatically generate
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
-- when the following is in the module
-- @
-- $(mkRecord ''Dog "Dogs" ["Owner Name", "Dog Name"])
-- @
module Cob.RecordM.TH (mkRecord) where

import Data.Char (toLower)

import Control.Monad (zipWithM, foldM)

import Data.Text (Text, unpack)
import Data.ByteString (ByteString)

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, object, (.=), withObject, (.:))

import Cob.RecordM (Record(..), Ref(..))

import Language.Haskell.TH

-- noBang :: a -> (Bang, a)
-- noBang = (,) (Bang NoSourceUnpackedness NoSourceStrictness)

data SupportedRecordType = StringT
                         | TextT
                         | ByteStringT
                         | RefT
                         | IntT
                         | OtherT Name

mkToJSON :: [SupportedRecordType] -> [Text] -> Q Exp
mkToJSON tys fields = [e| object $(ListE <$> zipWithM mkToJSONItem tys fields) |]
    where
        mkToJSONItem :: SupportedRecordType -> Text -> Q Exp
        mkToJSONItem ty field = [e| $(mkString $ unpack field) .= $(varName ty) |]
            where
                varName RefT       = [e| show $(mkVarE $ fieldNormalizeRef field) |]
                varName IntT       = [e| show $(mkVarE $ fieldNormalize field) |]
                varName _          = mkVarE $ fieldNormalize field

mkParseJSON :: Name -> [SupportedRecordType] -> [Text] -> Q Exp
mkParseJSON tyConName tys fields = do
        finalStmt <- NoBindS . AppE (VarE $ mkName "return") <$> foldM foldConArgs (ConE tyConName) (zip tys fields)
        LamE [VarP $ mkName "v"] . DoE Nothing . (++ [finalStmt]) <$> zipWithM mkParseJSONItem tys fields
    where
        mkParseJSONItem :: SupportedRecordType -> Text -> Q Stmt
        mkParseJSONItem ty field = BindS <$> [p| [$(mkVarP $ fieldNormalize field)] |] <*> [e| v .: $(mkString $ fieldNormalize field) |]

        foldConArgs :: Exp -> (SupportedRecordType, Text) -> Q Exp
        exp `foldConArgs` (ty, field) = AppE exp <$>
            case ty of
              RefT -> [e| Ref (read $(mkVarE $ fieldNormalize field)) |]
              IntT -> [e| (read $(mkVarE $ fieldNormalize field)) |]
              _    -> [e| $(mkVarE $ fieldNormalize field) |]
              
mkRecordPat :: Name -> [SupportedRecordType] -> [Text] -> Q Pat
mkRecordPat tyConName tyConArgList fields = do
    pats <- zipWithM mkArgPat tyConArgList fields
    return $ ConP tyConName pats
    where
        mkArgPat :: SupportedRecordType -> Text -> Q Pat
        mkArgPat ty field = case ty of
            StringT     -> defaultRet
            TextT       -> defaultRet
            ByteStringT -> defaultRet
            RefT        -> ConP 'Ref . (:[]) <$> mkVarP (fieldNormalizeRef field)
            IntT        -> defaultRet
            OtherT _    -> defaultRet
            where
                defaultRet :: Q Pat
                defaultRet = mkVarP $ fieldNormalize field

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
              | otherwise             -> do
                  tyInfo <- reify conTy
                  case tyInfo of
                    TyConI (TySynD _ _ synTy) -> parseTyConArg synTy -- If type is a type synonym, parse synonym type instead
                    _ -> return (OtherT conTy)                       -- Type isn't a type synonym, so it's just something else
              -- fail $ "Records with constructed types other than 'Int', 'String', 'Text' or 'ByteString' are not supported. Attempted: " <> show t
            t@(AppT (ConT conTy) _)
                | conTy == ''Ref -> return RefT
                | otherwise -> fail $ "Records with applied constructed types other than 'Ref' are not supported. Attempted: " <> show t
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
            _ -> fail "makeRecord should be called on a datatype or newtype declaration with only one normal constructor and no type variables"
      _ -> fail "makeRecord should be called on a datatype or newtype"

mkRecord :: Name -> String -> [Text] -> Q [Dec]
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
fieldNormalize :: Text -> String
fieldNormalize = map (toLower . \c -> if c == ' ' then '_' else c) . unpack

fieldNormalizeRef :: Text -> String
fieldNormalizeRef = (<> "_id") . fieldNormalize

mkTy :: Name -> Q Type
mkTy = return . ConT 

mkString :: String -> Q Exp
mkString = return . LitE . StringL

mkVarE :: String -> Q Exp
mkVarE = return . VarE . mkName

mkVarP :: String -> Q Pat
mkVarP = return . VarP . mkName
