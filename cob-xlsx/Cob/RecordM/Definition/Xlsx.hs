{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cob.RecordM.Definition.Xlsx
  ( module Cob.RecordM.Definition.Xlsx
    -- ** Re-exports
  , RowIndex(..), ColumnIndex(..)
  ) where

import Control.Concurrent.Async hiding (Default(..), def)
import Control.Concurrent.Async.Pool hiding (Default(..), def)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Codec.Xlsx
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Prettyprinter (pretty)
import qualified Prettyprinter as PP

import Cob.RecordM.Definition
import Control.Monad
import Control.Exception
import Data.List (find, sort)
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE

import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Aeson
import Cob.RecordM.Servant as Servant hiding (newDefinition)
import Cob.RecordM (newDefinition)
import Cob.Session
import Cob.Utils
import qualified Servant.Client
import Data.Semigroup

data OptionsXlsxImporter = OptionsXlsxImporter
  { maxListSize :: Int
  -- ^ The threshold over which N many distinct values are no longer considered for a $[] field
  , maxRefSize  :: Int
  -- ^ The threshold over which N many distinct values are no longer considered for a $ref field
  , worksheetName :: Text
    -- ^ Name of the worksheet from which to parse definition
  , startCoord :: (RowIndex, ColumnIndex)
    -- ^ The (Row, Column) from which to start parsing the
    -- to-be-definition table
  , concurrently :: Bool
    -- ^ Whether to create auxiliary definitions and insert values in them concurrently
  }

-- | Create a t'Definition' from an Excel Spreadsheet file (t'FilePath')
xlsxFileToDef :: FilePath
              -- ^ The path to the Excel spreadsheet
              -> OptionsXlsxImporter
              -> IO (Map Text [Text], Definition)
              -- ^ The resulting definition, and a map from definition names to
              -- values to be included in those definitions as records. The
              -- resulting definition might use $ref to refer to definitions that
              -- don't exist and are only specified as key-element pairs in this
              -- map.
xlsxFileToDef fp opts = do
  bs <- BSL.readFile fp
  return $ xlsxToDef opts (toXlsxFast bs)

-- ROMES:TODO: Sacar automaticamente $link tb (e.g. ao encontrar um link nas cells)

-- ROMES:TODO: $date vs $datetime vs $time, see which formats map to which (kind of easy).

-- | Create a t'Definition' from an Excel Spreadsheet (t'Xlsx')
xlsxToDef :: OptionsXlsxImporter
          -> Xlsx
          -- ^ The spreadsheet
          -> (Map Text [Text], Definition)
          -- ^ The resulting definition, and a map from definition names to
          -- values to be included in those definitions as records. The
          -- resulting definition might use $ref to refer to definitions that
          -- don't exist and are only specified as key-element pairs in this
          -- map.
xlsxToDef
  OptionsXlsxImporter
    { worksheetName
    , startCoord=(start_r,start_c)
    , maxListSize
    , maxRefSize
    } spreadsheet = 
  let
    allCells = case spreadsheet ^? ixSheet worksheetName . wsCells of
                 Nothing -> error $ "Worksheet "
                                     ++ show worksheetName
                                     ++ " not found in the spreadsheet"
                 Just cells -> cells
    headerCells :: [(Cell, [Cell])]
    headerCells
      = [ (cellHeader, columnCells)
          | ((rh,ch), cellHeader) <- M.toAscList allCells
          , rh == start_r
          , ch >= start_c
          , let columnCells
                  = M.elems $
                    M.filterWithKey (\(rc,cc) _ -> rc > rh && cc == ch) allCells
        ]
   in runDSL worksheetName "@AUTOGENXLSX" do
     M.unionsWith (<>) <$>
       forM headerCells \(headerCell, columnCells) -> do
         case headerCell ^. cellValue of
           Just (CellText headerContent) -> do
             let nonEmptyCells = filter (isJust . _cellValue) columnCells
             -- Determine the description from the style of the cell below
             case descFromCells headerContent nonEmptyCells of
               (fieldDesc, recordsToAdd) -> do
                  headerContent |= fieldDesc
                  pure recordsToAdd
           _ -> pure mempty

  where

    descFromCells :: Text
                  -- ^ Name of this column to name a new definition
                  -> [Cell]
                  -- ^ The cells below the header in this column
                  -> (FieldDescription, Map Text [Text])
                  -- ^ Returns the description of this definition field, and,
                  -- if the field is a $ref, returns a non-empty map from the
                  -- name of the field to the value of the records that should
                  -- be added to a new definition with that name.
    descFromCells _ [] = ("", mempty)
    descFromCells rawColName (cell:cells) =
      let
        normalizedDefName
          = T.take 20 $ -- Definition name length limit
            T.filter (\c -> c /= '(' && c /= ')' && c /= ',') $
            T.replace "\n" " " rawColName
       in case sconcat $ NE.map (cellType spreadsheet) (cell NE.:| take 4 cells) of
        CellDateT -> (datetime, mempty)
        CellNumberT -> (number, mempty)
        CellTextT
          -- If any textual cell has length > 255, we make this $text.
          | any (maybe False ((> 255) . T.length) . (^? cellValue . _Just . _CellText)) (take maxRefSize $ cell:cells)
          -> (text, mempty)
          -- Otherwise, we heuristically determine whether the field should be
          -- a $[], $ref, or just plain.
          | otherwise
          -> determineListOrRef normalizedDefName (cell:cells)

    determineListOrRef :: Text -> [Cell] -> (FieldDescription, Map Text [Text])
    determineListOrRef defName cells
        | let groups
                = NE.group $ sort $
                  mapMaybe ((\case
                    CellText ""  -> Nothing
                    CellText txt -> Just txt
                    _ -> Nothing)
                             <=< (^.cellValue))
                           cells
          -- We only want to count the amount of names that are used more
          -- than once, and if there are many amongst these that are used
          -- more than once, we likely want to create a list or ref, even if
          -- some of the options are only used once.
        , let n = length (take (maxRefSize+1) groups)
        , n > 1 && n < maxRefSize
          -- Extract the first of the equal modulo capitalization group values
        , let canonGroups = map NE.head groups
        = if n < maxListSize
              then (list canonGroups, mempty)
              else
                -- ngroups > 5 && < 20, make a new definition with these values
                (dollarRef defName "*", M.singleton defName canonGroups)
        | otherwise
        = mempty

data CellType
  = CellNumberT
  | CellDateT
  | CellTextT

instance Semigroup CellType where
  CellTextT <> _ = CellTextT
  _ <> CellTextT = CellTextT
  CellDateT <> CellNumberT = CellDateT
  CellNumberT <> CellDateT = CellDateT
  a <> _b = a

cellType :: Xlsx -> Cell -> CellType
cellType xlsx cell = fromMaybe CellTextT do
  -- No keywords in any other case
  (CellDouble double) <- cell ^. cellValue
  styleXfId <- cell ^. cellStyle

  let cellXf = fromMaybe (error $ show styleXfId ++ " CellXf Id not found in " ++ show (getStylesheet xlsx ^? styleSheetCellXfs)) $
                getStylesheet xlsx ^? styleSheetCellXfs . ix styleXfId
      numFmt = fromMaybe (-1) $ cellXf ^. cellXfNumFmtId

  -- Check if CellDouble is formatted as a date,
  -- to determine if field is $number or $datetime
  if isDateTime numFmt []
     then return CellDateT
     else return CellNumberT

getStylesheet :: Xlsx -> StyleSheet
getStylesheet xlsx = either throw id $ parseStyleSheet $ xlsx ^. xlStyles

-- See https://github.com/tidyverse/readxl/blob/866eff6e1a73226598fbcd5510607706402fd0ab/src/ColSpec.h#L111
-- TODO: Upstream
isDateTime :: Int
           -- ^ Does this numFmt id mean a datetime?
           -> [Int]
           -- ^ The list of custom datetime formats
           -> Bool
           -- ^ Whether the given numFmt id is a datetime format
isDateTime id custom
  -- Page and section numbers below refer to
  -- ECMA-376 (version, date, and download URL given in XlsxCell.h)
  --
  -- Example from L.2.7.4.4 p4698 for hypothetical cell D2
  -- Cell D2 contains the text "Q1" and is defined in the cell table of sheet1
  -- as:
  --
  -- <c r="D2" s="7" t="s">
  --     <v>0</v>
  -- </c>
  --
  -- On this cell, the attribute value s="7" indicates that the 7th (zero-based)
  -- <xf> definition of <cellXfs> holds the formatting information for the cell.
  -- The 7th <xf> of <cellXfs> is defined as:
  --
  -- <xf numFmtId="0" fontId="4" fillId="2" borderId="2" xfId="1" applyBorder="1"/>
  --
  -- The number formatting information cannot be found in a <numFmt> definition
  -- because it is a built-in format; instead, it is implicitly understood to be
  -- the 0th built-in number format.
  --
  -- This function stores knowledge about these built-in number formats.
  --
  -- 18.8.30 numFmt (Number Format) p1786
  -- Date times: 14-22, 27-36, 45-47, 50-58, 71-81 (inclusive)
  | (id >= 14 && id <= 22) ||
    (id >= 27 && id <= 36) ||
    (id >= 45 && id <= 47) ||
    (id >= 50 && id <= 58) ||
    (id >= 71 && id <= 81)
  = True

  -- Built-in format that's not a date
  | id < 164
  = False

  | otherwise
  = id `elem` custom

--------------------------------------------------------------------------------

newtype NameRecord = NameRecord { value :: Text }
instance ToJSON NameRecord where
    toJSON (NameRecord value) =
      object
        [ "Value" Data.Aeson..= value
        ]
instance FromJSON NameRecord where
    parseJSON = withObject "NameRecord" $ \v -> do
        [value] <- v .: "value"
        return (NameRecord value)

-- | Create a CoB 'Definition' and auxiliary definitions for $ref fields from
-- the given .xlsx file. Tries to be smart.
createDefFromXlsx :: MonadCob m
                  => FilePath
                  -> OptionsXlsxImporter
                  -> m ()
createDefFromXlsx fp opts = do
  sess@CobSession{clientEnv} <- ask

  (refs, definition) <- liftIO $ xlsxFileToDef fp opts

  liftIO $ do

    let forHow_ = if opts.concurrently then forConcurrently_ else forM_

    -- ROMES:TODO: Flag to enable or disable concurrency (for log readability)
    forHow_ (M.toList refs) \(defName, values) -> do

      putStrLn $ "Creating auxiliary definition: " <> T.unpack defName
      runReaderT (newDefinition (fromDSL defName "@AUTOGENAUX" (void $ "Value" |= "$instanceLabel"))) sess

      forHow_ values \rawValue -> do
        putStrLn $ "Adding instance: " <> T.unpack rawValue
        Servant.Client.runClientM
          (Servant.addInstance (AddSpec (T.unpack defName) (NameRecord rawValue) False)) clientEnv

    putStrLn $ "Creating main definition: " <> T.unpack opts.worksheetName

  newDefinition definition

--------------------------------------------------------------------------------
-- * Dry Run
--------------------------------------------------------------------------------

dryRunDefFromXlsx :: FilePath -> OptionsXlsxImporter -> IO ()
dryRunDefFromXlsx fp opts = do
  (refs, definition) <- liftIO $ xlsxFileToDef fp opts
  print $
    PP.vsep
      (map (\(name, instances) ->
             pretty name <>
               PP.nest 4 (PP.vsep (map pretty instances))
           )
           (M.toList refs)) <> PP.line <>
    pretty definition
