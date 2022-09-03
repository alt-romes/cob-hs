{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad

import Cob.RecordM.TH
import Cob.Ref
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

  x <- get d1

  liftCob $ do

    unless (Dog bb "Bombinhas" /= x) $ do

      print "everything went right"

    print "end"


main = putStrLn "Hello World!"
