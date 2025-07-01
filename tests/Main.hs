{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text

import Data.Aeson
import Data.List
import Data.Ord

import Cob.Ref
import Cob.RecordM
import Cob.RecordM.TH

-- Test the generation of Record instances:
data CobTr
  = CobTr
    { desc :: Maybe Text
    , date :: Text
    , mov  :: String
    , sald :: Maybe Text
    , id   :: Ref CobTr
    }
    deriving Show
mkRecord ''CobTr "CASA Finanças Movimentos" ["Descritivo", "Data", "Movimento", "Saldo", "ID"]

-- Test it compiles
data EnumTest
  = OptionA
  | OptionB
  | OptionC
mkRecordEnum ''EnumTest ["Option A", "Option B", "Option C"]

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] -- , properties

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]

unitTests = testGroup "Unit tests" []
  -- [ testCase "List comparison (different length)" $
  --     [1, 2, 3] `compare` [1,2] @?= GT

  -- -- the following test does not hold
  -- , testCase "List comparison (same length)" $
  --     [1, 2, 3] `compare` [1,2,2] @?= LT
  -- ]

