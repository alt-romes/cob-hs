{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Cob.RecordM.Definition.Xlsx where

import Data.Maybe
import Codec.Xlsx
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M

import Cob.RecordM.Definition
import Control.Monad
import Control.Exception

-- ROMES:TODO: Sacar automaticamente $[] e $link tb (e.g. ao encontrar um link nas cells)?


-- | Create a t'Definition' from an Excel Spreadsheet (t'Xlsx')
xlsxToDef :: Text
          -- ^ Name of the worksheet from which to parse definition
          -> (RowIndex, ColumnIndex)
          -- ^ The (Row, Column) from which to start parsing the
          -- to-be-definition table
          -> Xlsx
          -- ^ The spreadsheet
          -> Definition
          -- ^ The resulting definition
xlsxToDef wsName (start_r,start_c) spreadsheet = 
  let
    allCells = case spreadsheet ^? ixSheet wsName . wsCells of
                 Nothing -> error $ "Worksheet "
                                     ++ show wsName
                                     ++ " not found in the spreadsheet"
                 Just cells -> cells
    headerCells
      = [ ((r,c), cell)
            | ((r,c), cell) <- M.toAscList allCells
            , r == start_r
            , c >= start_c ]
   in fromDSL wsName "" do

     forM_ headerCells \((r,c),cell) -> do

       case cell ^. cellValue of
         Just (CellText txt) -> do
           -- Determine the description from the style of the cell below
           txt |= descFromCell (allCells ^? ix (r+1,c))
           pure ()
         _ -> pure ()
  where
    descFromCell :: Maybe Cell -> Text
    descFromCell Nothing     = mempty
    descFromCell (Just cell) = fromMaybe mempty do
      -- No keywords in any other case
      (CellDouble double) <- cell ^. cellValue
      styleXfId <- cell ^. cellStyle

      let cellXf = fromMaybe (error $ show styleXfId ++ " CellXf Id not found in " ++ show (stylesheet ^? styleSheetCellXfs)) $
                    stylesheet ^? styleSheetCellXfs . ix styleXfId
          numFmt = fromMaybe (-1) $ cellXf ^. cellXfNumFmtId

      -- Check if CellDouble is formatted as a date,
      -- to determine if field is $number or $datetime
      if isDateTime numFmt []
         then return datetime
         else return number

    stylesheet :: StyleSheet
    stylesheet = either throw id $ parseStyleSheet $ spreadsheet ^. xlStyles

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
