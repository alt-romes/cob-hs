{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cob.RecordM.Servant where

import qualified Data.Text as T
import qualified Data.Map as M

import Data.Proxy
import qualified Data.List as L (intercalate)

import Servant.API hiding (Delete)
import qualified Servant.API
import qualified Servant.Client as C
import qualified Servant.Client.Streaming as SC
import Data.Aeson

import qualified Streamly.Data.Stream as Streamly

import qualified Servant.Types.SourceT as Servant

import Cob.Ref
import Cob.RecordM.Definition

-- TODO: Maybe use servant-streamly
instance FromSourceIO a (Streamly.Stream IO a) where
  fromSourceIO src = Servant.unSourceT src go
   where
    go :: Servant.StepT IO a -> IO (Streamly.Stream IO a)
    go step = case step of
      Servant.Stop             -> return Streamly.nil
      Servant.Error e          -> return $ Streamly.fromEffect $ fail e
      Servant.Skip  n          -> go n
      Servant.Yield x nextStep -> Streamly.cons x <$> go nextStep
      Servant.Effect nextStep  -> nextStep >>= go

type RecordM a = "recordm" :> "recordm" :> a


type Search = "definitions" :> "search" :> (
              (QueryParam' '[Required, Strict] "def" String :> SearchCommon)
              :<|>
              (QueryParam' '[Required, Strict] "defId" DefinitionId :> SearchCommon)
                                           )
type SearchCommon
    =  QueryParam "q" String
    :> QueryParam "from" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" SortParam
    :> Get '[JSON] Value

type StreamSearch = "definitions" :> "search" :> "stream" :> (
              (QueryParam' '[Required, Strict] "def" String :> StreamSearchCommon)
              :<|>
              (QueryParam' '[Required, Strict] "defId" DefinitionId :> StreamSearchCommon)
                                                             )
type StreamSearchCommon
  =  QueryParam "q" String
  :> QueryParam "sort" SortParam
  :> StreamGet NewlineFraming JSON (Streamly.Stream IO Value)

type InstancesGet    = "instances" :> Capture "id" Integer :> QueryParam "If-None-Match" String :> Get '[JSON] Value
type InstancesDelete = "instances" :> Capture "id" Integer :> QueryParam "ignoreRefs" Bool :> Servant.API.Delete '[JSON] NoContent

type IntegrationAdd    a = "instances" :> "integration" :> ReqBody '[JSON] (AddSpec a)    :> Post '[JSON] (Ref a)
type IntegrationUpdate a = "instances" :> "integration" :> ReqBody '[JSON] (UpdateSpec a) :> Put '[JSON] OperationsSummary
type IntegrationDelete a = "instances" :> "integration" :> ReqBody '[JSON] (DeleteSpec a) :> Servant.API.Delete '[JSON] OperationsSummary

type DefinitionNew = "definitions" :> ReqBody '[JSON] Definition :> Post '[JSON] Value



searchByName :: String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
searchById   :: DefinitionId -> Maybe String -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
(searchByName :<|> searchById) = C.client (Proxy @(RecordM Search))

streamSearchByName :: String -> Maybe String -> Maybe SortParam -> SC.ClientM (Streamly.Stream IO Value)
streamSearchById   :: DefinitionId -> Maybe String -> Maybe SortParam -> SC.ClientM (Streamly.Stream IO Value)
(streamSearchByName :<|> streamSearchById) = SC.client (Proxy @(RecordM StreamSearch))

getInstance    :: Integer -> Maybe String -> C.ClientM Value
deleteInstance :: Integer -> Maybe Bool -> C.ClientM NoContent
getInstance     = C.client (Proxy @(RecordM InstancesGet))
deleteInstance  = C.client (Proxy @(RecordM InstancesDelete))

addInstance     :: forall a. ToJSON a => AddSpec a -> C.ClientM (Ref a)
updateInstances :: forall a. ToJSON a => UpdateSpec a -> C.ClientM OperationsSummary
deleteInstances :: forall a. DeleteSpec a -> C.ClientM OperationsSummary
addInstance     = C.client (Proxy @(RecordM (IntegrationAdd a)))
updateInstances = C.client (Proxy @(RecordM (IntegrationUpdate a)))
deleteInstances = C.client (Proxy @(RecordM (IntegrationDelete a)))

newDefinition :: Definition -> C.ClientM Value
newDefinition = C.client (Proxy @(RecordM DefinitionNew))

-- Sort given a list of pairs @(<field>, <direction>)@
newtype SortParam = SortParam [(String, String)]
instance ToHttpApiData SortParam where
  toQueryParam (SortParam ls) = T.pack $ L.intercalate "," $ map (\(f,d) -> f <> (':' : d)) ls

data AddSpec a = AddSpec { _atype :: !String, _avalues :: !a, _waitForSearchAvailability :: !Bool }
instance ToJSON a => ToJSON (AddSpec a) where
  toJSON (AddSpec t v w) = object [ "type" .= t
                                  , "values" .= v
                                  , "waitForSearchAvailability" .= w
                                  ]

data DeleteSpec a = DeleteSpec { _rtype :: !String, _rcondition :: !String, _ignoreRefs :: !Bool }
instance ToJSON (DeleteSpec a) where
  toJSON (DeleteSpec a b c) = object [ "type" .= a
                                     , "condition" .= b
                                     , "ignoreRefs" .= c
                                     ]

data UpdateSpec a = UpdateSpec { _utype :: !String, _ucondition :: !String, _uvalues :: !a }
instance ToJSON a => ToJSON (UpdateSpec a) where
  toJSON (UpdateSpec a b c) = object [ "type"      .= a
                                       , "condition" .= b
                                       , "values"    .= c
                                       -- , "version"   .= d -- is done through the condition or could be done through the other (non-integration) api
                                       ]

data OperationsSummary = OperationsSummary { updatedOS :: {-# UNPACK #-} !Int, deletedOS :: {-# UNPACK #-} !Int, forbiddenOS :: {-# UNPACK #-} !Int, errorOS :: {-# UNPACK #-} !Int } deriving Show
instance FromJSON OperationsSummary where
  parseJSON = withObject "OperationsSummary" $ \obj ->
    OperationsSummary
      <$> (obj .: "updated")
      <*> (obj .: "deleted")
      <*> (obj .: "forbidden")
      <*> (obj .: "error")

instance ToJSON Definition where
  toJSON Definition{..} =
    object
      [ "name" .= defName
      , "description" .= defDescription
      , "fieldDefinitions" .= (map snd . M.toList) defFieldDefinitions
      , "state" .= defState
      -- ROMES:TODO: Should this be set to html encoded things or just nothing?
      , "htmlEncodedName" .= ("" :: T.Text)
      , "htmlEncodedDescription" .= ("" :: T.Text)
      ]

instance ToJSON Field where
  toJSON (Field{..}) =
    object $
      [ "name" .= fieldName
      , "description" .= fieldDescription
      , "duplicable" .= fieldDuplicable
      , "required" .= fieldRequired
      , "order" .= getFieldOrder fieldId
      , "id" .= getFieldId fieldId
      ]
      ++ case fieldFields of
           Nothing -> []
           Just fields ->
             [ "fields" .= (map snd . M.toList) fields ]
      ++ case fieldCondition of
           Nothing -> []
           Just cond ->
             [ "condition" .= cond ]

instance ToJSON Condition where
  toJSON Equals{lhs, rhs} = String (lhs <> "=" <> rhs)

instance ToJSON FieldRequired where
  toJSON MandatoryField = "mandatory"
  toJSON FieldNotRequired = ""

instance ToJSON DefinitionState where
  toJSON EnabledDefinition = "enabled"

