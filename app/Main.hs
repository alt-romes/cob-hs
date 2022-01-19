{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Cob.RecordM.TH

data Dog = Dog String String

$(mkRecord ''Dog "Dogs" ["Owner Name", "Dog Name"])


main :: IO ()
main = putStrLn "Hi2"
