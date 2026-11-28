{-# LANGUAGE OverloadedStrings #-}
module Cob.RecordM.Definition.Pretty
  ( prettyDefinition, prettyField, prettyFieldDescription, fieldsTable
  ) where

import Data.List (sortOn, transpose)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid (Last(..))
import Lens.Micro
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
    : [cells r <+> prettyFieldDescription (f ^. spec . description) | (r, f) <- rows]
  where
    header = ["ID", "Name", "Flags", "If"]
    rows =
      [ ([idText (fieldId f), replicate (2 * n) ' ' ++ T.unpack (fieldName f), flags f, maybe "" (T.unpack . condText) (condition f)], f)
      | (n, _, f) <- allFields d ]
    widths = map (maximum . map length) (transpose (header : map fst rows))
    cells = hsep . zipWith (\w s -> pretty (s ++ replicate (w - length s) ' ')) widths
    flags f = [if f ^. spec . isMandatory then 'M' else ' ', if f ^. spec . isDuplicable then 'D' else ' ']

prettyFieldDescription :: FieldDescription -> Doc AnsiStyle
prettyFieldDescription (FieldDesc ps) = mconcat
  [ case p of KwPart k -> annotate (color Cyan) (viaShow k); RawPart t -> pretty t | p <- ps ]

-- | All attributes of a field, given its parent
prettyField :: Maybe FieldName -> Field -> Doc AnsiStyle
prettyField p f = vsep [annotate bold (fill 12 k) <+> v | (k, v) <- rows]
  where
    fd = f ^. spec . description
    rows =
      [ ("ID", pretty (idText (fieldId f)))
      , ("Name", pretty (fieldName f))
      , ("Parent", maybe "-" (pretty . idText) p)
      , ("Keywords", hsep (map (annotate (color Cyan) . viaShow) (knownKws fd)))
      , ("Description", pretty (descText fd))
      , ("Mandatory", yesNo (f ^. spec . isMandatory))
      , ("Duplicable", yesNo (f ^. spec . isDuplicable))
      , ("Condition", maybe "-" (pretty . condText) (condition f))
      , ("Order", pretty (fieldOrder f))
      , ("Subfields", hsep [pretty (idText (fieldId s)) <+> viaShow (fieldName s) | s <- sortOn fieldOrder (M.elems (fieldFields f))])
      ]

-- | Every field with its depth and parent, subfields right after their parent
allFields :: Definition -> [(Int, Maybe FieldName, Field)]
allFields = go 0 Nothing . defFieldDefinitions
  where
    go n p fs = concat
      [ (n, p, f) : go (n + 1) (Just (fieldId f)) (fieldFields f)
      | f <- sortOn fieldOrder (M.elems fs) ]

idText :: FieldName -> String
idText (ExistingField i) = show i
idText (NewField _) = "new"

condition :: Field -> Maybe Condition
condition f = getLast (specCondition (fieldSpec f))

yesNo :: Bool -> Doc ann
yesNo b = if b then "yes" else "no"
