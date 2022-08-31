{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Lens.TH

import System.Environment

import Control.Monad.Except
import Data.Bifunctor
import Control.Monad
import Data.List

import Cob
import Cob.RecordM
import Cob.RecordM.TH
import Cob.Testing

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

parseArgs :: [String] -> [(Ref a, Double)]
parseArgs = map ((\(x, ':':y) -> (Ref (read x), read y)) . break (== ':'))

logic :: (Ref MovimentoR, Movimento) -> [(Ref Classificação, Movimento)] -> Cob IO ()
logic (movimentoId, total) splits = do
    unless (total == sum (map snd splits)) (throwError "Total sum doesn't equal separate amounts")

    [updatedMov] <- rmUpdateInstances_ movimentoId (classificação ?~ Ref 76564)

    let commonMov = updatedMov & descrição ?~ "Desdobramento automático: " <> show movimentoId
                               & ultsaldo  .~ "Não"

    forM_ splits $ \(refclass, amount) ->
        rmAddInstance (commonMov & classificação ?~ refclass
                                 & mov .~ amount)

    forM_ splits $ \(_, amount) ->
        rmAddInstance (commonMov & mov .~ -amount)



-- Demo

newtype Owner = Owner String
mkRecord ''Owner "Owners" ["Owner"]

data Dog = Dog (Ref Owner) String
         deriving (Show)
mkRecord ''Dog "Dogs" ["Owner", "Dog"]

test1 :: Cob IO [(Ref Dog, Dog)]
test1 = do
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)

run1 :: Cob IO [(Ref Dog, Dog)]
run1 = do
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)
    owner1 <- rmAddInstance (Owner "Owner1")
    dog1 <- rmAddInstance (Dog owner1 "dog1")
    dog2 <- rmAddInstance (Dog owner1 "dog2")
    rmDefinitionSearch @Dog ("owner:" <> show owner1)

main :: IO ()
main = do
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession "mimes8.cultofbits.com" cobToken
    res      <- runCobTests session run1
    print res



    -- ([prim], pargs) <- bimap parseArgs parseArgs . splitAt 1 <$> getArgs
    -- either <- runCob session (logic prim pargs)
    -- print either
