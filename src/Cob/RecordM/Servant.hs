{-# LANGUAGE DataKinds #-}
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
import qualified Servant.API
import qualified Servant.Client as C
import qualified Servant.Client.Streaming as SC
import Data.Aeson

import Cob.RecordM.Ref

type RecordM a = "recordm" :> "recordm" :> a


type Search = "definitions" :> "search" :> (
              (QueryParam' '[Required, Strict] "def" String :> SearchCommon)
              :<|>
              (QueryParam' '[Required, Strict] "defId" Int :> SearchCommon)
                                           )
type SearchCommon
    =  QueryParam "q" T.Text
    :> QueryParam "from" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" SortParam
    :> Get '[JSON] Value

type StreamSearch = "definitions" :> "search" :> "stream" :> (
              (QueryParam' '[Required, Strict] "def" String :> StreamSearchCommon)
              :<|>
              (QueryParam' '[Required, Strict] "defId" Int :> StreamSearchCommon)
                                                             )
type StreamSearchCommon
  =  QueryParam "q" T.Text
  :> QueryParam "sort" SortParam
  :> StreamGet NewlineFraming JSON (SourceIO Value)

type InstancesGet    = "instances" :> Capture "id" Int :> QueryParam "If-None-Match" String :> Get '[JSON] Value
type InstancesDelete = "instances" :> Capture "id" Int :> QueryParam "ignoreRefs" Bool :> Servant.API.Delete '[JSON] ()

type IntegrationAdd    a = "instances" :> "integration" :> ReqBody '[JSON] (AddSpec a)    :> Post '[JSON] (Ref a)
type IntegrationUpdate a = "instances" :> "integration" :> ReqBody '[JSON] (UpdateSpec a) :> Put '[JSON] OperationsSummary
type IntegrationDelete a = "instances" :> "integration" :> ReqBody '[JSON] (DeleteSpec a) :> Servant.API.Delete '[JSON] OperationsSummary

searchByName :: String -> Maybe T.Text -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
searchById   :: Int    -> Maybe T.Text -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
(searchByName :<|> searchById) = C.client (Proxy @(RecordM Search))

streamSearchByName :: String -> Maybe T.Text -> Maybe SortParam -> SC.ClientM (SourceIO Value)
streamSearchById   :: Int    -> Maybe T.Text -> Maybe SortParam -> SC.ClientM (SourceIO Value)
(streamSearchByName :<|> streamSearchById) = SC.client (Proxy @(RecordM StreamSearch))

getInstance    :: Int -> Maybe String -> C.ClientM Value
deleteInstance :: Int -> Maybe Bool -> C.ClientM ()
getInstance     = C.client (Proxy @(RecordM InstancesGet))
deleteInstance  = C.client (Proxy @(RecordM InstancesDelete))

addInstance     :: forall a. ToJSON a => AddSpec a -> C.ClientM (Ref a)
updateInstances :: forall a. ToJSON a => UpdateSpec a -> C.ClientM OperationsSummary
deleteInstances :: forall a. DeleteSpec a -> C.ClientM OperationsSummary
addInstance     = C.client (Proxy @(RecordM (IntegrationAdd a)))
updateInstances = C.client (Proxy @(RecordM (IntegrationUpdate a)))
deleteInstances = C.client (Proxy @(RecordM (IntegrationDelete a)))

-- Sort given a list of pairs @(<field>, <direction>)@
newtype SortParam = SortParam [(String, String)]
instance ToHttpApiData SortParam where
  toQueryParam (SortParam ls) = T.pack $ L.intercalate "," $ map (\(f,d) -> f <> (':' : d)) ls

data AddSpec a = AddSpec { _atype :: String, _avalues :: a, _waitForSearchAvailability :: Bool }
instance ToJSON a => ToJSON (AddSpec a) where
  toJSON (AddSpec t v w) = object [ "type" .= t
                                  , "values" .= v
                                  , "waitForSearchAvailability" .= w
                                  ]

data DeleteSpec a = DeleteSpec { _rtype :: String, _rcondition :: String, _ignoreRefs :: Bool }
instance ToJSON (DeleteSpec a) where
  toJSON (DeleteSpec a b c) = object [ "type" .= a
                                     , "condition" .= b
                                     , "ignoreRefs" .= c
                                     ]

data UpdateSpec a = UpdateSpec { _utype :: String, _ucondition :: String, _uvalues :: a }
instance ToJSON a => ToJSON (UpdateSpec a) where
  toJSON (UpdateSpec a b c) = object [ "type" .= a
                                     , "condition" .= b
                                     , "values" .= c
                                     ]

data OperationsSummary = OperationsSummary { updatedOS :: Int, deletedOS :: Int, forbiddenOS :: Int, errorOS :: Int }
instance FromJSON OperationsSummary where
  parseJSON = withObject "OperationsSummary" $ \obj ->
    OperationsSummary
      <$> (obj .: "updated")
      <*> (obj .: "deleted")
      <*> (obj .: "forbidden")
      <*> (obj .: "error")

