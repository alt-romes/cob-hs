{-# LANGUAGE TemplateHaskellQuotes #-}
module Cob.RecordM.TH where

import Language.Haskell.TH

makeRecord :: Name -> String -> [String] -> Q [Dec]
makeRecord ty definition fields = do
    tyInfo <- reify ty
