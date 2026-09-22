{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad

import Control.Exception hiding (try, catch)

import Cob.RecordM.TH
import Cob.Ref
import Cob.Session
import Cob
import qualified Data.Text as T

newtype Owner = Owner String deriving (Show, Eq)
mkRecord ''Owner "Owners" ["Owner"]

data Dog = Dog (Ref Owner) String
         deriving (Show, Eq)
mkRecord ''Dog "Dogs" ["Owner", "Dog"]

data KD = KD { email :: T.Text }
mkRecord ''KD "Kanjideck Digital Fulfillment" ["Email"]

newtype KD' = KD' { ref :: Ref KD }
mkRecord ''KD' "Kanjideck Digital Accesses" ["Accessed By"]


freeCob :: Cob ()
freeCob = do

  ba <- addSync (KD "test@example.com")
  _ <- addSync (KD' ba)

  bb <- addSync (Owner "Bombásio")

  d1 <- addSync (Dog bb "Bombinhas")

  x  <- get bb

  delete bb

  unless (Owner "Bombásio" /= x) $ do
    liftCob (putStrLn "everything went right")

  liftCob (print "end")

main :: IO ()
main = do
  cobToken <- init <$> readFile "cob-token.secret"
  withSession "mimes.cultofbits.com" cobToken $ \session ->
    -- mockCob session freeCob
    runCob session freeCob

