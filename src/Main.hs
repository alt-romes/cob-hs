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

makeLenses ''MovimentoR
mkRecord ''MovimentoR "CASA Finanças Movimentos" ["Classificação", "Observação", "Data mov", "Movimento", "Conta", "Data", "Último Saldo"]

newtype Owner = Owner String
mkRecord ''Owner "Owners" ["Owner"]

logic :: (Ref MovimentoR, Movimento) -> [(Ref Classificação, Movimento)] -> Cob ()
logic (movimentoId, total) splits = do
    guard (total == sum (map snd splits))

    [updatedMov] <- rmUpdateInstances_ movimentoId (classificação ?~ Ref 76564)

    let commonMov = updatedMov & descrição ?~ "Desdobramento automático: " <> show movimentoId
                               & ultsaldo  .~ "Não"

    forM_ splits $ \(refclass, amount) ->
        rmAddInstance (commonMov & classificação ?~ refclass
                                 & mov .~ amount)

    forM_ splits $ \(_, amount) ->
        rmAddInstance (commonMov & mov .~ -amount)


main :: IO ()
main = do
    session <- makeSession "mimes8.cultofbits.com" ""
    ([prim], pargs) <- bimap parseArgs parseArgs . splitAt 1 <$> getArgs
    either <- runCob session (logic prim pargs)
    print either
    return ()

parseArgs :: [String] -> [(Ref a, Double)]
parseArgs = map ((\(x, ':':y) -> (Ref (read x), read y)) . break (== ':'))
