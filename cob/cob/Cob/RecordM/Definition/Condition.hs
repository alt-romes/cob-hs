{-# LANGUAGE OverloadedStrings #-}
-- | RecordM field visibility conditions, e.g. @=Yes@ or @(Kind=A || Kind=B) && Size>1@
module Cob.RecordM.Definition.Condition
  ( Condition(..), CondExpr(..), CmpOp(..)
  , condText, parseCondText
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (string, space)

-- | A field's visibility condition
data Condition = Condition
  { condExpr   :: CondExpr
  , condSource :: Maybe Text
    -- ^ The original source text, used for exact printing.
    -- 'Nothing' when there's no original text for this condition.
  }
  deriving Show

-- | @&&@ binds tighter than @||@
data CondExpr
  = Compare (Maybe Text) CmpOp [Text]
    -- ^ Compares the named field, or the parent field if 'Nothing', with the
    -- values. With '=' and '!' it is compared against any of the values.
  | And [CondExpr]
  | Or [CondExpr]
  deriving (Eq, Show)

data CmpOp = Equal | NotEqual | Greater | GreaterEq | Lower | LowerEq
  deriving (Eq, Show)

opText :: CmpOp -> Text
opText = \case
  Equal -> "="; NotEqual -> "!"; Greater -> ">"; GreaterEq -> ">="; Lower -> "<"; LowerEq -> "<="

condText :: Condition -> Text
condText (Condition e src) = case src of
  -- use exact print condition text if possible
  Just t | parseCondExpr t == Just e -> t
  -- if parsed exact print text doesn't match expression, render from scratch:
  _ -> render e
  where
    render = \case
      Compare f o vs -> fromMaybe "" f <> opText o <> T.intercalate "," vs
      And es -> T.intercalate " && " [case x of Or _ -> "(" <> render x <> ")"; _ -> render x | x <- es]
      Or es -> T.intercalate " || " (map render es)

parseCondText :: Text -> Maybe Condition
parseCondText t = (\e -> Condition e (Just t)) <$> parseCondExpr t

parseCondExpr :: Text -> Maybe CondExpr
parseCondExpr = parseMaybe @Void (space *> orP <* eof)
  where
    orP  = junction Or  <$> sepBy1 andP (symbol "||")
    andP = junction And <$> sepBy1 atom (symbol "&&")
    atom = between (symbol "(") (symbol ")") orP <|> compareP
    compareP = do
      f <- T.strip <$> takeWhileP Nothing (`notElem` ['=', '!', '<', '>', '(', ')'])
      o <- choice [o <$ string (opText o) | o <- [GreaterEq, LowerEq, Greater, Lower, Equal, NotEqual]]
      vs <- T.strip . T.pack <$> many (notFollowedBy (string "&&" <|> string "||" <|> string ")") *> anySingle)
      pure (Compare (if T.null f then Nothing else Just f) o (T.splitOn "," vs))
    symbol s = string s <* space
    junction _ [e] = e
    junction c es  = c es
