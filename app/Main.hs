{-# LANGUAGE TemplateHaskell #-}
module Main where

-- import Data.Aeson
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

data MovimentoR = MovimentoR { rclass :: Maybe (Ref Classificação)
                             , desc   :: Maybe Observação
                             , datmv  :: DataMov
                             , mov    :: Movimento
                             , conta  :: Conta
                             , dta    :: Data
                             , ulsal  :: UltimoSaldo }


mkRecord ''MovimentoR "CASA Finanças Movimentos" ["Classificação", "Observação", "Data mov", "Movimento", "Conta", "Data", "Último Saldo"]

desdobramento :: Ref Classificação
desdobramento = Ref 76564


logic :: Cob ()
logic = do
    -- Get and parse cmd arguments
    ([(movimentoId, total)], splits) <- bimap parseArgs parseArgs . splitAt 0 <$> getArgs & liftCob

    -- Total equals splits
    guard (total == foldl (\x y -> x + snd y) 0 splits)

    -- Get record and update primary movement
    [movimento] <- rmDefinitionSearch_ movimentoId
    rmUpdateInstance movimentoId movimento { rclass = Just desdobramento }

    -- Create instances
    let commonMov = MovimentoR
                        (Just desdobramento)
                        (Just ("Desdobramento automático: " <> show movimentoId))
                        (datmv movimento) 0 (conta movimento) (dta movimento) "Não"

    forM_ splits $ \(refclass, amount) -> do
        rmAddInstance commonMov { rclass = Just refclass, mov = amount }
    
    forM_ splits $ \(_, amount) -> do
        rmAddInstance commonMov { mov = -amount }


main :: IO ()
main = do
    session <- makeSession "mimes8.cultofbits.com" "Zwhwb71CwCGmRkvkzbjIW6YESN2gdIyXzdADZSgnKkliQmH6ECcXcxrjVaS5Urt8NfJnQlQgvsV85dpeGx4/EGFT/+OewkHrr2niAIxaWUN4xSXIbeq+n3Ft0TM5T9bF0WL4GCd2gH4UCRKWw5UISg=="
    either <- runCob session logic
    print either
    return ()

parseArgs :: [String] -> [(Ref a, Double)]
parseArgs = map ((\(x, ':':y) -> (Ref (read x), read y)) . break (== ':'))
