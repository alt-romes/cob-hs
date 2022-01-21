{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Lens.TH

import Data.Aeson
import System.Environment

import Data.Bifunctor
import Control.Monad
import Data.List

import Cob
import Cob.RecordM
import Cob.RecordM.TH

import Data.Function

data Classificação = Classificação

type Observação = String
type DataMov = String
type Movimento = Double
type Conta = String
type Data = String
type UltimoSaldo = String

data MovimentoR = MovimentoR { _classificação :: Maybe (Ref Classificação)
                             , _descrição     :: Maybe Observação
                             , _datmv         :: DataMov
                             , _mov           :: Movimento
                             , _conta         :: Conta
                             , _dta           :: Data
                             , _ultsaldo      :: UltimoSaldo }

mkRecord ''MovimentoR "CASA Finanças Movimentos" ["Classificação", "Observação", "Data mov", "Movimento", "Conta", "Data", "Último Saldo"]
makeLenses ''MovimentoR

logic :: (Ref MovimentoR, Movimento) -> [(Ref Classificação, Movimento)] -> Cob ()
logic (movimentoId, total) splits = do

    guard (total == sum (map snd splits))

    [movimento] <- movimentoId ^?^ 1
    movimentoId ^=^ movimento
                        & classificação ?~ Ref 76564

    let commonMov = movimento
                        & classificação ?~ Ref 76564
                        & descrição ?~ "Desdobramento automático: " <> show movimentoId
                        & ultsaldo .~ "Não"

    forM_ splits $ \(refclass, amount) -> do
        (^+^) (commonMov & classificação ?~ refclass & mov .~ amount)

    forM_ splits $ \(_, amount) -> do
        (^+^) (commonMov & mov .~ -amount)



main :: IO ()
main = do
    session <- makeSession "mimes8.cultofbits.com" "Zwhwb71CwCGmRkvkzbjIW6YESN2gdIyXzdADZSgnKkliQmH6ECcXcxrjVaS5Urt8NfJnQlQgvsV85dpeGx4/EGFT/+OewkHrr2niAIxaWUN4xSXIbeq+n3Ft0TM5T9bF0WL4GCd2gH4UCRKWw5UISg=="
    ([prim], pargs) <- bimap parseArgs parseArgs . splitAt 0 <$> getArgs
    either <- runCob session (logic prim pargs)
    print either
    return ()

parseArgs :: [String] -> [(Ref a, Double)]
parseArgs = map ((\(x, ':':y) -> (Ref (read x), read y)) . break (== ':'))

rmSearch = curry rmDefinitionSearch_

(^?^) :: (Record b, RecordMQuery a) => a -> Int -> Cob [b]
(^?^) q n = rmDefinitionSearch_ (q, n)

(^=^) :: (Record a) => Ref a -> a -> Cob ()
(^=^) = rmUpdateInstance

(^+^) :: (Record a) => a -> Cob (Ref a)
(^+^) = rmAddInstance

infix 0 ^=^
infix 0 ^+^
infix 0 ^?^
