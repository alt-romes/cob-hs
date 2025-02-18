{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cob.RecordM.Definition.Mermaid
  ( module Cob.RecordM.Definition.Mermaid
  ) where

import Debug.Trace
import Control.Concurrent.Async hiding (Default(..), def)
import Control.Concurrent.Async.Pool hiding (Default(..), def)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Data.Text.Metrics
import Data.Function
import System.IO (hFlush)
import GHC.IO.Handle.FD (stdout)
import Text.Read (readMaybe)

import Mermaid.Diagram.EntityRelationship

newtype OptionsMermaid = OptionsMermaid
  { concurrently :: Bool
    -- ^ Whether to create definitions concurrently
  }

-- | Create a t'Definition' from a Mermaid Entity Relationship file path (t'FilePath')
mermaidFileToDef :: FilePath
                 -- ^ The path to the Mermaid diagram
                 -> OptionsMermaid
                 -> IO [Definition]
mermaidFileToDef fp opts = do
  bs <- T.readFile fp
  case parseERDiagram bs of
    Left e -> fail e
    Right m -> return $ mermaidToDef opts m

-- | Create a t'Definition' from an Mermaid ER Diagram (t'EntityRelationDiagram')
mermaidToDef :: OptionsMermaid
             -> EntityRelationDiagram
             -> [Definition]
mermaidToDef OptionsMermaid{concurrently} ERDiagram{entityAttributes, entityRelations} =
  let defs = M.fromList $
        map (\a -> let name = fromMaybe a.entity a.nameAlias
                    in (a.entity, simpleDefinition name "")) entityAttributes
   in M.elems $
      foldr updateDefs defs entityRelations
  where
    updateDefs :: EntityRelation -> Map Entity Definition -> Map Entity Definition
    updateDefs ER{firstEntity, relationship, secondEntity, relationshipLabel} defs
      | Just' (Relationship leftCard identification rightCard) <- relationship
      , Just' secondEntity <- secondEntity
      , Just' relationshipLabel <- relationshipLabel
      , let firstName = (defs M.! firstEntity).defName
      , let secondName = (defs M.! secondEntity).defName

      , let addRelationshipLeft def = extendFromDSL def do
              secondName |= case rightCard of
                OneOrMore -> do
                  dollarReferences firstName secondName
              return ()

      , let addRelationshipRight def = extendFromDSL def do
              firstName |= case leftCard of
                OneOrMore -> do
                  _
              return ()

      = M.adjust addRelationshipRight secondEntity $
        M.adjust addRelationshipLeft firstEntity defs

      | Nothing' <- relationship
      , Nothing' <- secondEntity
      , Nothing' <- relationshipLabel
      = defs

