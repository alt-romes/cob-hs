{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Cob.RecordM.Servant where

import qualified Data.Text as T

import Data.Proxy
import qualified Data.List as L (intercalate)

import Servant.API hiding (Delete)
import Servant.Multipart.API
import Servant.Multipart.Client ()
import qualified Servant.API
import qualified Servant.Client as C
import qualified Servant.Client.Streaming as SC
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

import Cob.Ref
import Cob.RecordM.Record
import Cob.RecordM.Definition

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
  :> StreamGet NewlineFraming JSON (SourceIO Value)

type StructuredSearch     = "definitions" :> "search" :> QueryParam' '[Required, Strict] "def" String :> ReqBody '[JSON] Value :> Post '[JSON] Value
type StructuredSearchById = "definitions" :> "search" :> QueryParam' '[Required, Strict] "defId" DefinitionId :> ReqBody '[JSON] Value :> Post '[JSON] Value

type InstancesGet    = "instances" :> Capture "id" Integer :> QueryParam "If-None-Match" String :> Get '[JSON] Value
type InstancesDelete = "instances" :> Capture "id" Integer :> QueryParam "ignoreRefs" Bool :> Servant.API.Delete '[JSON] NoContent

type InstancesFileUpload tag = "instances" :> Capture "id" Integer :> "files" :> Capture "field" String :> MultipartForm tag (MultipartData tag) :> Post '[JSON] NoContent

type IntegrationAdd    a = "instances" :> "integration" :> ReqBody '[JSON] (AddSpec a)    :> Post '[JSON] (Ref a)
type IntegrationUpdate a = "instances" :> "integration" :> ReqBody '[JSON] (UpdateSpec a) :> Put '[JSON] OperationsSummary
type IntegrationDelete a = "instances" :> "integration" :> ReqBody '[JSON] (DeleteSpec a) :> Servant.API.Delete '[JSON] OperationsSummary

type DefinitionGet = "definitions" :> Capture "definitionId" DefinitionId :> Get '[JSON] Value
type DefinitionNew = "definitions" :> ReqBody '[JSON] Value :> Post '[JSON] (Ref Definition)
type DefinitionUpd = "definitions" :> Capture "definitionId" DefinitionId :> ReqBody '[JSON] Value :> Put '[JSON] Value
type DefinitionDelete = "definitions" :> Capture "definitionId" DefinitionId :> Servant.API.Delete '[JSON] NoContent

searchByName :: String       -> Maybe String -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
searchById   :: DefinitionId -> Maybe String -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
(searchByName :<|> searchById) = C.client (Proxy @(RecordM Search))

streamSearchByName :: String -> Maybe String -> Maybe SortParam -> SC.ClientM (SourceIO Value)
streamSearchById   :: DefinitionId -> Maybe String -> Maybe SortParam -> SC.ClientM (SourceIO Value)
(streamSearchByName :<|> streamSearchById) = SC.client (Proxy @(RecordM StreamSearch))

structuredSearchByName :: String       -> Value -> C.ClientM Value
structuredSearchById   :: DefinitionId -> Value -> C.ClientM Value
structuredSearchByName = C.client (Proxy @(RecordM StructuredSearch))
structuredSearchById   = C.client (Proxy @(RecordM StructuredSearchById))

getInstance    :: Integer -> Maybe String -> C.ClientM Value
deleteInstance :: Integer -> Maybe Bool -> C.ClientM NoContent
getInstance     = C.client (Proxy @(RecordM InstancesGet))
deleteInstance  = C.client (Proxy @(RecordM InstancesDelete))

uploadFile :: forall tag. C.HasClient C.ClientM (RecordM (InstancesFileUpload tag)) => Integer -> String -> (BSL.ByteString, MultipartData tag) -> C.ClientM NoContent
uploadFile = C.client (Proxy @(RecordM (InstancesFileUpload tag)))

addInstance     :: forall a. Record a => AddSpec a -> C.ClientM (Ref a)
updateInstances :: forall a. Record a => UpdateSpec a -> C.ClientM OperationsSummary
deleteInstances :: forall a. DeleteSpec a -> C.ClientM OperationsSummary
addInstance     = C.client (Proxy @(RecordM (IntegrationAdd a)))
updateInstances = C.client (Proxy @(RecordM (IntegrationUpdate a)))
deleteInstances = C.client (Proxy @(RecordM (IntegrationDelete a)))

getDefinition :: DefinitionId -> C.ClientM Value
newDefinition :: Value -> C.ClientM (Ref Definition)
updDefinition :: DefinitionId -> Value -> C.ClientM Value{-returns updated definition-}
deleteDefinition :: DefinitionId -> C.ClientM NoContent
getDefinition = C.client (Proxy @(RecordM DefinitionGet))
newDefinition = C.client (Proxy @(RecordM DefinitionNew))
updDefinition = C.client (Proxy @(RecordM DefinitionUpd))
deleteDefinition = C.client (Proxy @(RecordM DefinitionDelete))

-- Sort given a list of pairs @(<field>, <direction>)@
newtype SortParam = SortParam [(String, String)]
instance ToHttpApiData SortParam where
  toQueryParam (SortParam ls) = T.pack $ L.intercalate "," $ map (\(f,d) -> f <> (':' : d)) ls

data AddSpec a = AddSpec { _atype :: !String, _avalues :: !a, _waitForSearchAvailability :: !Bool }
instance Record a => ToJSON (AddSpec a) where
  toJSON (AddSpec t v w) = object [ "type" .= t
                                  , "values" .= toRecordM v
                                  , "waitForSearchAvailability" .= w
                                  ]

data DeleteSpec a = DeleteSpec { _rtype :: !String, _rcondition :: !String, _ignoreRefs :: !Bool }
instance ToJSON (DeleteSpec a) where
  toJSON (DeleteSpec a b c) = object [ "type" .= a
                                     , "condition" .= b
                                     , "ignoreRefs" .= c
                                     ]

data UpdateSpec a = UpdateSpec !(Ref a) !(Versioned a)
instance Record a => ToJSON (UpdateSpec a) where
  toJSON (UpdateSpec ref Versioned{version, value}) = object
    [ "type"      .= definition @a
    -- the special value "recordmInstanceId:N[@V]" to match a single instance.
    -- If the optional '@V' is used, the update will only take place if the
    -- instance N is at version V
    , "condition" .= ("recordmInstanceId:" <> show ref <> "@" <> show version)
    , "values"    .= toRecordM value
    ]

data OperationsSummary = OperationsSummary
  { updatedOS   :: !Int
  , deletedOS   :: !Int
  , forbiddenOS :: !Int
  , errorOS     :: !Int }
  deriving Show
instance FromJSON OperationsSummary where
  parseJSON = withObject "OperationsSummary" $ \obj ->
    OperationsSummary
      <$> (obj .: "updated")
      <*> (obj .: "deleted")
      <*> (obj .: "forbidden")
      <*> (obj .: "error")
