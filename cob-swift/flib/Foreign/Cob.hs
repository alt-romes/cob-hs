{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, OverloadedStrings #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin Foreign.Swift.Lib #-}
module Foreign.Cob where

import Cob.UserM
import Cob.RecordM
import Cob.RecordM.Record
import Cob.RecordM.TH
import Cob.Session
import Control.Monad.Reader

import Foreign.Swift
import Foreign.Swift.Marshal
import Foreign.Swift.Lib

import Cob.RecordM.Definition
import Cob.RecordM.Query
import Data.Aeson

import Data.Proxy (Proxy(..))
import Data.Void
import qualified Data.Kind as K
import Cob.RecordM.Dashboard
import Cob.Ref
import Cob.RecordM.DateTime

instance ToMoatType Void where
  toMoatType _ = Concrete "Never" []

swiftData ''FieldRequired
swiftData ''Query
swiftData ''Keyword
swiftData ''FieldDescription
swiftData ''Condition
swiftData ''FieldName
swiftData ''DefinitionState
swiftData ''Field
swiftData ''Definition
swiftData ''DefinitionId

swiftPtr ''CobSession

swiftMarshal PtrKind ''CobSession
swiftMarshal JSONKind ''DefinitionId
swiftMarshal JSONKind ''Definition

-- | Get a definition
cobDefinition :: CobSession -> DefinitionId -> IO Definition
cobDefinition s di = do
  runReaderT (getDefinitionRep di) s
$(foreignExportSwift 'cobDefinition)

-- | Login to get a CobSession (todo: read the haddocks into Swift too!)
cobLogin :: Host -> CobToken -> IO CobSession
cobLogin a b = do
  s <- withSession a b pure
  return s{logger=print} -- overwrite thing that was scope-bound in withUMSession
$(foreignExportSwift 'cobLogin)

--------------------------------------------------------------------------------
-- Shuffdle bits
--------------------------------------------------------------------------------

-- | Shuffdle Usernames
newtype Username = Username String
mkRecord ''Username "Usernames" ["Nome"]

data Mode = Normal | Hard
mkRecordEnum ''Mode ["Normal", "Hard"]

data ShuffdleStat = SS
  { word  :: String
  , date  :: DateTime
  , user  :: Ref Username
  , mode  :: Mode
  , moves :: Int
  }
mkRecord ''ShuffdleStat "Wins" ["Word", "Date", "Username", "Mode", "Moves"]

data BoardConf = BoardConf { boardDef :: String, boardQuery :: String }

-- | A board with queries for strings fixed for a certain definition? Here Shuffdle
data UnresolvedBoard = UnresolvedBoard { uboard :: Board BoardConf }

data ResolvedBoard = ResolvedBoard { board :: Board ResolvedItem }

swiftData ''TotalsLine
swiftData ''BoardComponent
swiftData ''Board
swiftData ''ResolvedItem
swiftData ''ResolvedBoard
swiftData ''BoardConf
swiftData ''UnresolvedBoard
swiftMarshal JSONKind ''ResolvedBoard
swiftMarshal JSONKind ''UnresolvedBoard

resolveIt :: UnresolvedBoard -> CobSession -> CatchFFI IO ResolvedBoard
resolveIt (UnresolvedBoard b) s = CatchFFI $ flip runReaderT s $
  fmap ResolvedBoard . resolveBoard $
    (\BoardConf{boardDef, boardQuery} ->
      withDynRecord (DynRecord boardDef) $
        SomeQuery @DynRecord (byStr boardQuery)) <$> b
$(foreignExportSwift 'resolveIt)

shuffdleBoard :: CobSession -> IO ResolvedBoard
shuffdleBoard = runReaderT $ ResolvedBoard <$> do
  resolveBoard (Board "Stats"
    [{- CLabel "Stats", -} CTotals "Wins"
      [TDefinitionCount (SomeQuery (defaultQuery :: Query ShuffdleStat))]])

$(foreignExportSwift 'shuffdleBoard)

