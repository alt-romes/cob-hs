{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Cob.RecordM.Definition
import Cob.RecordM.Query
import Cob

$(do
    loc <- location
    runIO $ do
      putStrLn $ generateSwiftCode @(Query _)
      print loc
      pure []
  )

main :: IO ()
main = do
  putStrLn "Running..."
  -- putStrLn $ generateSwiftCode @FieldRequired
  -- putStrLn $ generateSwiftCode @(Query _)
  -- putStrLn $ generateSwiftCode @Keyword
  -- putStrLn $ generateSwiftCode @FieldDescription
  -- putStrLn $ generateSwiftCode @Condition
  -- putStrLn $ generateSwiftCode @FieldName
  -- putStrLn $ generateSwiftCode @DefinitionState
  -- putStrLn $ generateSwiftCode @Field
  -- putStrLn $ generateSwiftCode @Definition

