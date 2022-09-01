{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Cob.RecordM.Servant where

import qualified Data.Text as T

import Data.Proxy
import qualified Data.List as L (intercalate)

import Servant.API
import qualified Servant.Client as C
import qualified Servant.Client.Streaming as SC
import Data.Aeson

type RecordM a = "recordm" :> a


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


searchByName :: String -> Maybe T.Text -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
searchById   :: Int    -> Maybe T.Text -> Maybe Int -> Maybe Int -> Maybe SortParam -> C.ClientM Value
streamSearchByName :: String -> Maybe T.Text -> Maybe SortParam -> SC.ClientM (SourceIO Value)
streamSearchById   :: Int    -> Maybe T.Text -> Maybe SortParam -> SC.ClientM (SourceIO Value)
(searchByName :<|> searchById) = C.client (Proxy @(RecordM Search))
(streamSearchByName :<|> streamSearchById) = SC.client (Proxy @(RecordM StreamSearch))

-- Sort given a list of pairs @(<field>, <direction>)@
newtype SortParam = SortParam [(String, String)]
instance ToHttpApiData SortParam where
  toQueryParam (SortParam ls) = T.pack $ L.intercalate "," $ map (\(f,d) -> f <> (':' : d)) ls
