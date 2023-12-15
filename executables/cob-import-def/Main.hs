{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiWayIf        #-}
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
  { filepath    ::w::: FilePath     <?> "Path to .xlsx file"
  , worksheetName::w::: Text        <?> "The name of the worksheet from which to create the main definition"
  , server      ::w::: Maybe String <?> "Domain name of CoB server"
  , user        ::w::: Maybe String <?> "Login username for CoB server"
  , maxListSize ::w::: Int          <!> "15"    <?> "Over <maxListSize> distinct rows we no longer consider this column a $[...]"
  , maxRefSize  ::w::: Int          <!> "30"    <?> "Over <maxRefSize> distinct rows we no longer consider this column for $ref(...)"
  , firstRow    ::w::: Int          <!> "1"     <?> "The row where the table starts"
  , firstCol    ::w::: Int          <!> "1"     <?> "The column where the table starts"
  , concurrently::w::: Bool         <!> "False" <?> "Create the auxiliary definitions and insert values concurrently"
  , dryRun      ::w::: Bool         <!> "False" <?> "Dry run will only determine and print the definitions that would be created from a given Excel, but does not create the definitions in RecordM"
  }
  deriving Generic
deriving instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do

  Opts{ server, user, filepath
      , maxListSize, maxRefSize
      , firstRow, firstCol
      , concurrently, worksheetName
      , dryRun } <- unwrapRecord "cob-import-def"

  let opts =
        OptionsXlsxImporter
          { worksheetName
          , startCoord=(RowIndex firstRow, ColumnIndex firstCol)
          , maxListSize
          , maxRefSize
          , concurrently
          }

  if | dryRun -> dryRunDefFromXlsx filepath opts

     | Just user' <- user
     , Just server' <- server
     -> do

      putStrLn "Input password:"
      pass <- getLine

      withUMSession server' user' pass $ \sess ->
        runCob sess $
          createDefFromXlsx filepath opts

     | otherwise
     -> putStrLn "When not doing a dry run, --server and --user are required arguments."

  where
    runCob s o = runReaderT o s

