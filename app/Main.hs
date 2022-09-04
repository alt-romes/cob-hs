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

newtype Owner = Owner String deriving (Show, Eq)
mkRecord ''Owner "Owners" ["Owner"]

data Dog = Dog (Ref Owner) String
         deriving (Show, Eq)
mkRecord ''Dog "Dogs" ["Owner", "Dog"]

freeCob :: Cob ()
freeCob = do

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
  session  <- makeSession "mimes8.cultofbits.com" cobToken
  mockCob session freeCob

