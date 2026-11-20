{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
module Cob.RecordM
  ( module Cob.RecordM
  -- ** Re-exports
  , MonadCob
  ) where

import Data.Aeson

import Control.Exception
import Cob.Exception

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Streamly.Data.Stream as Streamly
import qualified Streamly.Data.Stream.Prelude as Streamly

import qualified Servant.Client
import qualified Servant.Client.Streaming
import Servant.Multipart.API
import Servant.Multipart.Client (genBoundary)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import System.FilePath (takeFileName)

import Cob.RecordM.Servant as Servant
import Cob.RecordM.Record
import Cob.RecordM.Query
import Cob.RecordM.Definition
import Cob.Utils
import Cob.Session
import Cob.Ref
import Cob.Log
import Data.Bifunctor

--------------------------------------------------------------------------------
-- * Querying definitions
--------------------------------------------------------------------------------

-- | Search a @RecordM@ 'Definition' given a 'Query, and return a list of
-- references ('Ref') of a record and the corresponding records ('Record').
--
-- The 'Record' type to search for is inferred from the usage of the return
-- element. When the information available isn't enough to correctly infer the
-- search 'Record' type, an explicit type can be used to help the compiler.
--
-- ==== __Example__
--
-- @
-- dogs <- definitionSearch @DogsRecord "bobby"
--
-- definitionSearch
--   defaultRMQuery
--    { _q = "id:123"
--    , _from = 1
--    , _size = 21
--    , _sort = Just \"id\"
--    , _ascending = Just True }
--
-- definitionSearch (byRef 11223)
-- @
--
-- @Note@: the @OverloadedStrings@ and @ScopedTypeVariables@ language extensions must be enabled for the example above
definitionSearch :: forall a m. MonadCob m => Record a => Query a -> m [(Ref a, a)]
definitionSearch = fmap (map (second value)) . definitionSearchV

definitionSearchV :: forall a m. MonadCob m => Record a => Query a -> m [(Ref a, Versioned a)]
definitionSearchV rmQuery = do
    logInfo $ "Searching definition " <> toLogStr (definition @a) <> " with " <> toLogStr (_q rmQuery)
    rbody <- performReq $ searchByName (definition @a) (Just (_q rmQuery)) (Just (_from rmQuery)) (Just (_size rmQuery)) Nothing
    let
      parser = withObject "definition_search" $ \o -> do
        o'   <- o  .: "hits"
        hits <- o' .: "hits"
        forM hits $ \h -> do
          src <- h .: "_source"
          r <- parseCobRef @a src
          a <- parseVersioned (fromRecordM @a) src
          pure (r, a)
    parseOrThrowIO parser rbody

-- | Stream a definition!
streamDefinitionSearch :: forall a b m. MonadCob m => Record a => Query a -> (Streamly.Stream IO (Ref a, a) -> IO b) -> m b
streamDefinitionSearch rmQuery f = streamDefinitionSearchV @a @b @m rmQuery (f . fmap (second value))

streamDefinitionSearchV :: forall a b m. MonadCob m => Record a => Query a -> (Streamly.Stream IO (Ref a, Versioned a) -> IO b) -> m b
streamDefinitionSearchV rmQuery f = do
  logInfo $ "Streaming search definition " <> toLogStr (definition @a) <> " with " <> toLogStr (_q rmQuery)
  CobSession{clientEnv} <- ask
  let req = streamSearchByName (definition @a) (Just (_q rmQuery)) Nothing
  liftIO $ Servant.Client.Streaming.withClientM req clientEnv (\case
    Left e -> throwIO e
    Right stream ->
      let
        parseRefA = withObject "streaming_search" $ \o -> do
          src <- o .: "_source"
          r   <- parseCobRef @a src
          a   <- parseVersioned (fromRecordM @a) src
          pure (r, a)
      in
        f (Streamly.mapM (parseOrThrowIO parseRefA) stream))


-- | Get an instance by id and fail if the instance isn't found
--
-- TODO: Use /recordm/instances/{id} instead of definitions/search?
-- Difficulty of ^ is parsing
getInstance :: forall a m. MonadCob m => Record a => Ref a -> m a
getInstance ref = do
  logInfo $ "Fetching instance " <> toLogStr (show ref)
  definitionSearch (byRef ref) >>= \case
    [] -> liftIO $ fail ("Couldn't find instance with id " <> show ref)
    (_,x):_ -> pure x
{-# INLINE getInstance #-}

-- | Count the number of records matching a query in a definition
definitionCount :: forall a m. MonadCob m => Record a => Query a -> m Int
definitionCount rmQuery = do
    logInfo $ "Fetching definition count for " <> toLogStr (_q rmQuery)
    rbody <- performReq $ searchByName (definition @a) (Just (_q rmQuery)) (Just 0) (Just 0) Nothing
    parseOrThrowIO (withObject "definition_count" ((.: "hits") >=> (.: "total") >=> (.: "value"))) rbody

-- | Count the total number of records in a definition by its id
definitionCountById :: forall m. MonadCob m => DefinitionId -> m Int
definitionCountById did = do
    logInfo $ "Fetching definition total count by id for " <> toLogStr (show did)
    rbody <- performReq $ searchById did (Just "*") (Just 0) (Just 0) Nothing
    parseOrThrowIO (withObject "definition_count" ((.: "hits") >=> (.: "total") >=> (.: "value"))) rbody

--------------------------------------------------------------------------------
-- * Creating instances
--------------------------------------------------------------------------------

-- | Add to @RecordM@ a new instance given a 'Record', and return the 'Ref' of the newly created instance.
--
-- Note: By default RecordM answers before the added record being searchable.
-- If you want to add an instance but only continue when it's searchable, see
-- 'rmAddInstanceSync'
--
-- ==== __Example__
--
-- @
-- addDog :: Text -> Text -> Cob (Ref DogsRecord)
-- addDog name ownerName = do
--      rmAddInstance (DogsRecord name ownerName 0)
-- @
addInstance :: forall a m. MonadCob m => Record a => a -> m (Ref a)
addInstance = addInstanceWith False
{-# INLINE addInstance #-}

-- | Same as 'rmAddInstance' but only continue when added record is searchable.
--
-- The difference is the query parameter waitForSearchAvailability=true
-- Related to documentation on POST /recordm/instances/integration.
addInstanceSync :: forall a m. MonadCob m => Record a => a -> m (Ref a)
addInstanceSync = addInstanceWith True
{-# INLINE addInstanceSync #-}

-- | Add an instance to RecordM and set waitForSearchAvailability to the bool passed in the first paramater.
--
-- This is the internal method used by 'rmAddInstance' and 'rmAddInstanceSync
--
-- Example
-- @
-- ref <- rmAddInstanceWith True (ServersRecord ...)
-- -- Only because waitForSearchAvailability is true ^, will the update be able to
-- -- find the added instance and update it
-- rmUpdateInstance ref (property .~ newValue)
-- @
addInstanceWith :: forall a m. MonadCob m => Record a => Bool -> a -> m (Ref a)
addInstanceWith waitForSearchAvailability record = do
  logInfo $ "Creating instance in " <> toLogStr (definition @a) <> " for " <> toLogStr (encodeRecord record)
  -- Last I checked (Nov. 2023), instance creation doesn't return a version
  -- alongside the newly created ID
  performReq $ Servant.addInstance (AddSpec (definition @a) record waitForSearchAvailability)


--------------------------------------------------------------------------------
-- * Deleting instances
--------------------------------------------------------------------------------

-- | Delete an instance by id, ignoring if it has any references
--
-- See /recordm/instances/{id}
--
-- We don't do version checking! This will really delete the instance regardless of whether you were the last to update it.
deleteInstance :: forall a m. MonadCob m => Ref a -> m ()
deleteInstance r@(Ref ref) = do
  logInfo $ "Deleting instance " <> toLogStr (show r)
  _ <- performReq $ Servant.deleteInstance ref (Just True)
  pure ()

--------------------------------------------------------------------------------
-- * Updating instances
--------------------------------------------------------------------------------

-- | Update all instances matching a query and return the updated records.
--
-- This update is atomic!
-- If the record is changed while the function is running, the record won't be updated and (ideally 'OutdatedVersion', but really) 'UnknownUpdateError' will be thrown
--
-- This function might throw the following 'CobException's: 'UnknownUpdateError'
--
-- The resulting list of updated values doesn't necessarily reflect the order of the query results, I think
updateInstances :: forall a m. MonadCob m => Record a => Query a -> (a -> a) -> m [(Ref a, a)]
updateInstances query f = do
  cs <- ask
  logInfo $ "Updating instances of " <> toLogStr (definition @a) <> " matching " <> toLogStr (_q query)
  streamDefinitionSearchV query $ Streamly.toList . Streamly.parBuffered id . Streamly.mapM (\x -> runReaderT (updateInstance f x) cs)

updateInstance :: MonadCob m => Record a => (a -> a) -> (Ref a, Versioned a) -> m (Ref a, a)
updateInstance f (ref, Versioned{version, value}) = do
  CobSession{clientEnv=session} <- ask
  let updatedRecord = f value
  let req = Servant.updateInstances (Servant.UpdateSpec ref Versioned{version, value=updatedRecord})
  liftIO $ Servant.Client.runClientM req session >>= \case
    Left e -> throwIO e
    Right u
      -- TODO: We could use more precise errors; at least throw a specific
      -- error for not updating when the version was outdated. But I don't know
      -- what these ints mean...
      | errorOS u > 0 || updatedOS u <= 0 || forbiddenOS u > 0
                   -> throwIO (UnknownUpdateError ref (show u))
      | otherwise  -> pure (ref, updatedRecord)


--------------------------------------------------------------------------------
-- * Uploading files
--------------------------------------------------------------------------------

-- | A file to upload
data FileUpload
  -- | Path to a file on disk, streamed rather than loaded into memory
  = UploadFromDisk FilePath
  -- | File name and contents
  | UploadFromMemory FilePath BSL.ByteString

-- | Upload a file to a @$file@ field of an instance, given the field name and
-- the 'Ref' of the instance to add the upload to.
--
-- I think that if the instance already has an upload for that @$file@ with the
-- same file name (derived from 'FileUpload'), it will update that file instead.
uploadFile :: MonadCob m => Ref a -> String -> FileUpload -> m ()
uploadFile r fieldName contents = do
  logInfo $ "Uploading file " <> toLogStr name <> " to field " <> toLogStr fieldName <> " of instance " <> toLogStr (show r)
  boundary <- liftIO genBoundary
  let file :: MultipartResult tag -> (BSL.ByteString, MultipartData tag)
      file c = (boundary, MultipartData [] [FileData "file" (T.pack name) "application/octet-stream" c])
  _ <- performReq $ case contents of
    UploadFromDisk fp     -> Servant.uploadFile @Tmp (ref_id r) fieldName (file fp)
    UploadFromMemory _ bs -> Servant.uploadFile @Mem (ref_id r) fieldName (file bs)
  pure ()
  where
    name = case contents of
      UploadFromDisk fp    -> takeFileName fp
      UploadFromMemory n _ -> n

--------------------------------------------------------------------------------
-- * Definitions
--------------------------------------------------------------------------------

-- | Create a RecordM 'Definition', typically where 'Definition' is created
-- using 'fromDSL' and 'DefinitionQ'.
newDefinition :: forall m. MonadCob m => Definition -> m DefinitionId
newDefinition def = do
  logInfo $ "Creating new definition with name " <> toLogStr (defName def)
  Ref r <- performReq $ Servant.newDefinition (definitionValue def)
  pure (DefId (fromInteger r))

deleteDefinition :: MonadCob m => DefinitionId -> m ()
deleteDefinition (DefId di) = do
  logInfo $ "deleteDefinition: Validating that definition is empty by id: " <> toLogStr (di)
  total_insts <- definitionCountById (DefId di)
  if total_insts > 0
    then liftIO $
      throwIO (NonEmptyDefinitionDelete (DefId di) total_insts)
    else do
      logInfo $ "Definition " <> toLogStr (di) <> " is empty, proceeding with \"safe\" deletion"
      _ <- performReq $ Servant.deleteDefinition (DefId di)
      pure ()

-- | Get a representation of the Definition given its 'DefinitionId'
getDefinition :: forall m. MonadCob m => DefinitionId -> m Definition
getDefinition defId = do
  logInfo $ "Fetching definition by id " <> toLogStr (show defId)
  performReq (Servant.getDefinition defId) >>= parseOrThrowIO parseDefinition

-- | Update the given 'Definition' by 'DefinitionId' and return the updated 'Definition'
updDefinition :: forall m. MonadCob m => DefinitionId -> (Definition -> Definition) -> m Definition
updDefinition defId updDef = do
  def <- Cob.RecordM.getDefinition defId
  let newDef = definitionValue (updDef def)
  logInfo $ "Updating definition by id " <> toLogStr (show defId) <> " with new definition " <> toLogStr (encode newDef)
  performReq (Servant.updDefinition defId newDef) >>= parseOrThrowIO parseDefinition
