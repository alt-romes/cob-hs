{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Demo where

import Data.Text (Text)

import Cob
import Cob.UserM
import Cob.RecordM
import Cob.RecordM.TH

data Album = Album { cover :: Text, album :: Text, artist :: Text } deriving Show
mkRecord ''Album "ROMES Albums" ["Cover", "Album", "Artist"]

data Artist = Artist { url :: Text, name :: Text } deriving Show
mkRecord ''Artist "ROMES Artists" ["Picture", "Name"]

data Looks = Looks { code :: Text, width :: Int } deriving Show
mkRecord ''Looks "CYO Looks" ["Code", "Width"]

getFirstLooks :: Cob IO [Looks]
getFirstLooks = do
    looks <- rmLazyDefinitionSearch_ @Looks "*"
    return (take 40 looks)
