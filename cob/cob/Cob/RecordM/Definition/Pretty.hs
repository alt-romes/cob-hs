{-# LANGUAGE OverloadedStrings #-}
module Cob.RecordM.Definition.Pretty
  ( prettyDefinition, prettyField, prettyFieldDescription, fieldsTable
  ) where

import Data.List (sortOn, transpose)
import qualified Data.Map as M
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Terminal

import Cob.RecordM.Definition

-- | The definition header followed by its 'fieldsTable'
prettyDefinition :: Definition -> Doc AnsiStyle
prettyDefinition d = vsep
  [ annotate bold (pretty (defName d)) <+> parens ("definition" <+> maybe "?" viaShow (defId d) <> ", version" <+> maybe "?" pretty (defVersion d))
  , pretty (defDescription d)
  , ""
  , fieldsTable d
  ]

-- | All fields, subfields indented under their parent.
-- Flags: M mandatory, D duplicable. A field's condition is on its parent's value.
fieldsTable :: Definition -> Doc AnsiStyle
fieldsTable d = vsep $
  annotate underlined (cells header <+> "Description")
    : [cells r <+> prettyFieldDescription (fieldDescription f) | (r, f) <- rows]
  where
    header = ["ID", "Name", "Flags", "If"]
    rows =
      [ ([getFieldId (fieldId f), replicate (2 * n) ' ' ++ T.unpack (fieldName f), flags f, maybe "" condText (fieldCondition f)], f)
      | (n, _, f) <- allFields d ]
    widths = map (maximum . map length) (transpose (header : map fst rows))
    cells = hsep . zipWith (\w s -> pretty (s ++ replicate (w - length s) ' ')) widths
    flags f = [if isMandatory f then 'M' else ' ', if fieldDuplicable f then 'D' else ' ']

prettyFieldDescription :: FieldDescription -> Doc AnsiStyle
prettyFieldDescription fd =
  hsep (map (annotate (color Cyan) . viaShow) (knownKws fd) ++ [pretty (rawDesc fd) | not (T.null (rawDesc fd))])

-- | All attributes of a field, given its parent
prettyField :: Maybe FieldName -> Field -> Doc AnsiStyle
prettyField p f = vsep [annotate bold (fill 12 k) <+> v | (k, v) <- rows]
  where
    fd = fieldDescription f
    rows =
      [ ("ID", pretty (getFieldId (fieldId f)))
      , ("Name", pretty (fieldName f))
      , ("Parent", maybe "-" (pretty . getFieldId) p)
      , ("Keywords", hsep (map (annotate (color Cyan) . viaShow) (knownKws fd)))
      , ("Description", pretty (rawDesc fd))
      , ("Mandatory", yesNo (isMandatory f))
      , ("Duplicable", yesNo (fieldDuplicable f))
      , ("Condition", maybe "-" (pretty . condText) (fieldCondition f))
      , ("Order", pretty (getFieldOrder f))
      , ("Subfields", hsep [pretty (getFieldId (fieldId s)) <+> viaShow (fieldName s) | s <- sortOn getFieldOrder (M.elems (fieldFields f))])
      ]

-- | Every field with its depth and parent, subfields right after their parent
allFields :: Definition -> [(Int, Maybe FieldName, Field)]
allFields = go 0 Nothing . defFieldDefinitions
  where
    go n p fs = concat
      [ (n, p, f) : go (n + 1) (Just (fieldId f)) (fieldFields f)
      | f <- sortOn getFieldOrder (M.elems fs) ]

condText :: Condition -> String
condText (Equals _ v) = '=' : T.unpack v

isMandatory :: Field -> Bool
isMandatory f = fieldRequired f == MandatoryField

yesNo :: Bool -> Doc ann
yesNo b = if b then "yes" else "no"
