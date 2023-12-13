{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Main where

import Control.Monad.Reader
import Cob.RecordM.Definition.Xlsx
import Cob.UserM

import Options.Generic

data Opts w = Opts
  { server      ::w::: String   <?> "Domain name of CoB server"
  , user        ::w::: String   <?> "Login username for CoB server"
  , filepath    ::w::: FilePath <?> "Path to .xlsx file"
  , maxListSize ::w::: Int      <!> "15"    <?> "Over <maxListSize> distinct rows we no longer consider this column a $[...]"
  , maxRefSize  ::w::: Int      <!> "30"    <?> "Over <maxRefSize> distinct rows we no longer consider this column for $ref(...)"
  , concurrently::w::: Bool     <!> "False" <?> "Create the auxiliary definitions and insert values concurrently"
  , worksheetName::w::: Text    <?> "The name of the worksheet from which to create the main definition"
  }
  deriving Generic
deriving instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do

  Opts{ server, user, filepath
      , maxListSize, maxRefSize
      , concurrently, worksheetName } <- unwrapRecord "cob-import-def"

  putStrLn "Password:"
  pass <- getLine

  sess <- umSession server user pass

  runCob sess $ do
    createDefFromXlsx
      filepath
      OptionsXlsxImporter{ worksheetName
                         , startCoord=(1,1)
                         , maxListSize
                         , maxRefSize
                         , concurrently
                         }

  where
    runCob s o = runReaderT o s

