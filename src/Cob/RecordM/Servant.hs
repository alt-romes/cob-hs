{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

import qualified Streamly.Prelude as Streamly

import qualified Servant.Types.SourceT as Servant

import Cob.Ref

instance Streamly.IsStream t => FromSourceIO a (t IO a) where
  fromSourceIO src =
    Streamly.concatMapM id $ Streamly.fromAhead $ Streamly.fromPure $ Servant.unSourceT src go
   where
    go :: Streamly.IsStream t => Servant.StepT IO a -> IO (t IO a)
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
              (QueryParam' '[Required, Strict] "defId" Int :> SearchCommon)
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
              (QueryParam' '[Required, Strict] "defId" Int :> StreamSearchCommon)
                                                             )
type StreamSearchCommon
  =  QueryParam "q" String
  :> QueryParam "sort" SortParam
  :> StreamGet NewlineFraming JSON (Streamly.Serial Value)

type InstancesGet    = "instances" :> Capture "id" Integer :> QueryParam "If-None-Match" String :> Get '[JSON] Value
type InstancesDelete = "instances" :> Capture "id" Integer :> QueryParam "ignoreRefs" Bool :> Servant.API.Delete '[JSON] NoContent

type IntegrationAdd    a = "instances" :> "integration" :> ReqBody '[JSON] (AddSpec a)    :> Post '[JSON] (Ref a)
type IntegrationUpdate a = "instances" :> "integration" :> ReqBody '[JSON] (UpdateSpec a) :> Put '[JSON] OperationsSummary
type IntegrationDelete a = "instances" :> "integration" :> ReqBody '[JSON] (DeleteSpec a) :> Servant.API.Delete '[JSON] OperationsSummary

searchByName :: String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
searchById   :: Int    -> Maybe String -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
(searchByName :<|> searchById) = C.client (Proxy @(RecordM Search))

streamSearchByName :: String -> Maybe String -> Maybe SortParam -> SC.ClientM (Streamly.Serial Value)
streamSearchById   :: Int    -> Maybe String -> Maybe SortParam -> SC.ClientM (Streamly.Serial Value)
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


