{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Cob.RecordM.Dashboard where

import Data.Text (Text)
import Cob.RecordM
import Cob.RecordM.Record
import Cob.RecordM.Query

-- | An unresolved 'Board' stores in the leafs a 'Query' for some existential @a@ that instances @Record a@
data SomeQuery = forall a. Record a => SomeQuery (Query a)

data Board a = Board
  { boardTitle      :: Text
  , boardComponents :: [BoardComponent a]
  }
  deriving Functor

data BoardComponent a
  = CLabel { label :: !Text }
  | CTotals
    { label :: !Text
    , totals_lines :: [TotalsLine a]
    }
  deriving Functor

  -- | CMenu
  -- | CKibana
  -- | CFilter
  -- | CCalendar
  -- | CList 
  -- | CMermaid
  -- | CModalActivator
  -- | CMarkdown
  -- | CSlides
  -- | CHierarchy
  -- | CViewer
  -- | CImageViewer
  -- | CInstanceViewer

data TotalsLine a
  = TDefinitionCount { count :: a }
  deriving Functor
  -- | TDomainCount
  -- | TFieldSum
  -- | TFieldAverage
  -- | TFieldWeightedAverage
  -- | TDMEquipmentCount
  -- | TLink

resolveBoard :: MonadCob m => Board SomeQuery -> m (Board Int)
resolveBoard (Board t cps) = Board t <$> mapM resolveComponent cps

resolveComponent :: MonadCob m => BoardComponent SomeQuery -> m (BoardComponent Int)
resolveComponent (CLabel t) = pure $ CLabel t
resolveComponent (CTotals t ls) = CTotals t <$> mapM resolveTotalsLine ls

resolveTotalsLine :: MonadCob m => TotalsLine SomeQuery -> m (TotalsLine Int)
resolveTotalsLine (TDefinitionCount (SomeQuery q)) =
  TDefinitionCount <$> definitionCount q

