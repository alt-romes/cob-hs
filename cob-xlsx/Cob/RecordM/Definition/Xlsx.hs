{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cob.RecordM.Definition.Xlsx where

import qualified Data.ByteString.Lazy as BSL
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

-- | Create a t'Definition' from an Excel Spreadsheet file (t'FilePath')
xlsxFileToDef :: FilePath
              -- ^ The path to the Excel spreadsheet
              -> Text
              -- ^ Name of the worksheet from which to parse definition
              -> (RowIndex, ColumnIndex)
              -- ^ The (Row, Column) from which to start parsing the
              -- to-be-definition table
              -> IO (Map Text [Text], Definition)
              -- ^ The resulting definition, and a map from definition names to
              -- values to be included in those definitions as records. The
              -- resulting definition might use $ref to refer to definitions that
              -- don't exist and are only specified as key-element pairs in this
              -- map.
xlsxFileToDef fp t coords = do
  bs <- BSL.readFile fp
  return $ xlsxToDef t coords (toXlsxFast bs)

-- ROMES:TODO: Sacar automaticamente $[] e $link tb (e.g. ao encontrar um link nas cells)?

-- ROMES:TODO: $date vs $datetime vs $time, see which formats map to which (kind of easy).

-- | Create a t'Definition' from an Excel Spreadsheet (t'Xlsx')
xlsxToDef :: Text
          -- ^ Name of the worksheet from which to parse definition
          -> (RowIndex, ColumnIndex)
          -- ^ The (Row, Column) from which to start parsing the
          -- to-be-definition table
          -> Xlsx
          -- ^ The spreadsheet
          -> (Map Text [Text], Definition)
          -- ^ The resulting definition, and a map from definition names to
          -- values to be included in those definitions as records. The
          -- resulting definition might use $ref to refer to definitions that
          -- don't exist and are only specified as key-element pairs in this
          -- map.
xlsxToDef wsName (start_r,start_c) spreadsheet = 
  let
    allCells = case spreadsheet ^? ixSheet wsName . wsCells of
                 Nothing -> error $ "Worksheet "
                                     ++ show wsName
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
   in runDSL wsName "@AUTOGENXLSX" do
     M.unionsWith (<>) <$>
       forM headerCells \(headerCell, columnCells) -> do
         case headerCell ^. cellValue of
           Just (CellText txt) -> do
             let nonEmptyCells = filter (isJust . _cellValue) columnCells
             -- Determine the description from the style of the cell below
             case descFromCells txt nonEmptyCells of
               (fieldDesc, recordsToAdd) -> do
                  txt |= fieldDesc
                  pure recordsToAdd
           _ -> pure mempty

  where

    descFromCells :: Text
                  -- ^ Name of this column
                  -> [Cell]
                  -- ^ The cells below the header in this column
                  -> (Text, Map Text [Text])
                  -- ^ Returns the description of this definition field, and,
                  -- if the field is a $ref, returns a non-empty map from the
                  -- name of the field to the value of the records that should
                  -- be added to a new definition with that name.
    descFromCells colName []          = ("", mempty)
    descFromCells colName (cell:cells) = first T.unwords
      case cellType spreadsheet cell of
        CellDateT -> ([datetime], mempty)
        CellNumberT -> ([number], mempty)
        CellTextT
          -- If any textual cell has length > 255, we make this $text.
          | any (maybe False ((> 255) . T.length) . (^? cellValue._Just._CellText)) (cell:cells)
          -> ([text], mempty)
          -- Otherwise, we heuristically determine whether the field should be
          -- a $[], $ref, or just plain.
          | otherwise
          -> determineListOrRef colName (cell:cells)

    determineListOrRef :: Text -> [Cell] -> ([Text], Map Text [Text])
    determineListOrRef colName cells
        | let groups
                = NE.groupAllWith T.toLower $
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
        , let ngroups = length $
                          filter ((> 1) . NE.length) groups
        , ngroups > 1 && ngroups < 30
          -- Extract the first of the equal modulo capitalization group values
        , let canonGroups = map NE.head groups
        = if ngroups < 10
              then ([list canonGroups], mempty)
              else
                -- ngroups > 5 && < 20, make a new definition with these values
                (["$ref(" <> colName <> ",*)"], M.singleton colName canonGroups)
        | otherwise
        = mempty


data CellType
  = CellNumberT
  | CellDateT
  | CellTextT

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

createDefFromXlsx :: (MonadReader CobSession m, MonadIO m)
                  => FilePath
                  -> Text
                  -> (RowIndex, ColumnIndex)
                  -> m ()
createDefFromXlsx fp t coords = do

  (refs, def) <- liftIO $ xlsxFileToDef fp t coords

  forM_ (M.toList refs) \(defName, values) -> do
    liftIO $ putStrLn $ "Creating auxiliary definition: " <> T.unpack defName
    newDefinition (fromDSL defName "@AUTOGENAUX" (void $ "Value" |= ""))
    performReq $ 
      forM_ values \rawValue -> do
        liftIO $ putStrLn $ "Adding instance: " <> T.unpack rawValue
        Servant.addInstance (AddSpec (T.unpack defName) (NameRecord rawValue) True)

  liftIO $ putStrLn $ "Creating main definition: " <> T.unpack t
  newDefinition def

