{-# LANGUAGE TypeApplications #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Cob where

import Data.String                   ( fromString )
import Data.ByteString               ( ByteString )
import Data.ByteString.Char8 as BSC8 ( unpack     )
import Data.ByteString.Lazy          ( toStrict   )

import Data.Maybe ( listToMaybe )

import Data.Aeson               ( Value(..), FromJSON, ToJSON )
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Aeson.Types         ( parseEither  )

import Control.Applicative ( Alternative, empty, (<|>) )
import Control.Monad       ( MonadPlus, (>=>), unless )

import Control.Monad.Except   ( MonadError, throwError, catchError )
import Control.Monad.Reader   ( MonadReader, ask, local            )
import Control.Monad.Writer   ( MonadWriter, tell, listen, pass    )
import Control.Monad.IO.Class ( MonadIO, liftIO                    )
import Control.Monad.Trans    ( MonadTrans, lift                   )

import Data.Bifunctor (first, second, bimap)

import Data.Time.Clock    (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

import Network.HTTP.Conduit as Net (Manager, Cookie(..), Request(..), Response(..), defaultRequest, newManager, tlsManagerSettings, createCookieJar)
import Network.HTTP.Simple (setRequestManager, JSONException(..), httpJSONEither, httpNoBody)
import Network.HTTP.Types  (statusIsSuccessful, Status(..))

-- | The Cob Monad.
-- A context in which computations that interact with @RecordM@ can be executed.
-- 
-- See 'CobT' for more.
type Cob a = CobT 'NoModule IO a

-- | Run a 'Cob' computation and get either a 'CobError' or a value in an 'IO' context.
runCob :: CobSession -> Cob a -> IO (Either CobError a)
runCob session = fmap fst . runCobT session
{-# INLINE runCob #-}

-- | A 'Cob' computation will either succeed or return a 'CobError'.
type CobError = String

-- | The 'CobT' monad transformer.
--
-- A constructed monad made out of an existing monad @m@ such that its
-- computations can be embedded in 'CobT', from which it's also possible to
-- interact with @RecordM@.
--
-- The @cm@ parameter is the 'CobModule' -- the base Cob monad is extended by each module
--
newtype CobT (c :: CobModule) m a = Cob { unCob :: CobSession -> m (Either CobError a, CobWriter c) }

-- | The kind of module running in this Cob computation
data CobModule = NoModule | RecordM | UserM

-- | Define which kind of Monoid writer Cob uses, depending on the module
type family CobWriter (c :: CobModule)
-- | @CobT NoModule@ uses @()@ for the writer monoid
type instance CobWriter 'NoModule = ()

instance Functor m => Functor (CobT c m) where
    fmap f = Cob . (fmap . fmap) (first (fmap f)) . unCob
    {-# INLINE fmap #-}
    -- [x] fmap id = id

instance (Monoid (CobWriter c), Monad m) => Applicative (CobT c m) where
    pure = Cob . const . pure . (, mempty) . Right
    {-# INLINE pure #-}
    (Cob f') <*> (Cob g) = Cob $ \r ->
        f' r >>= \case
            (Left e, l) -> pure (Left e, l)
            (Right f, l) -> bimap (fmap f) (l <>) <$> g r
    {-# INLINE (<*>) #-}
    -- [x] pure id <*> v = v
    -- [x] pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- [x] pure f <*> pure x = pure (f x)
    -- [x] u <*> pure y = pure ($ y) <*> u

instance (Monoid (CobWriter c), Monad m) => Monad (CobT c m) where
    (Cob x') >>= f' = Cob $ \r ->
        x' r >>= \case
          (Left err, l) -> return (Left err, l)
          (Right x, l)  -> second (l <>) <$> unCob (f' x) r
    {-# INLINE (>>=) #-}
    -- [x] return a >>= k = k a
    -- [x] m >>= return = m
    -- [x] m >>= (\x -> k x >>= h) = (m >>= k) >>= h

instance (Monoid (CobWriter c), Monad m) => Alternative (CobT c m) where
    empty = Cob $ const $ pure (Left "Cob (alternative) empty computation. No error message.", mempty)
    {-# INLINE empty #-} 

    -- | The Cob alternative instance.
    --
    -- The computation to the right of '<|>' will only be executed if the computation to the left fails.
    --
    -- This is good to represent idioms like @getOrAddInstance@.
    --
    -- For example, the following code can be read \"search a RecordM definition
    -- with @query@ and extract the first element if it exists. If it doesn't,
    -- add the @newInstance@ to RecordM\", which is short for \"find a record by
    -- this query or add this new one\".
    -- @
    -- (rmDefinitionSearch query ?!) <|> rmAddInstance newInstance
    -- @
    -- Note: The snippet above requires the @PostfixOperators@ extension to be
    -- enabled
    --
    -- This other example reads \"get an instance by id 123 or add a new dog called
    -- bobby\"
    -- @
    -- rmGetInstance (Ref 123) <|> rmAddInstance (Dog "bobby")
    -- @
    --
    -- Note: IO exceptions are not catched, that means that if an IO exception
    -- is thrown, the computation won't fail cleanly and therefore the added
    -- instances won't be deleted if running with 'runRecordMTests'
    (Cob x') <|> (Cob y) = Cob $ \r ->
        x' r >>= \case
            (Left e, l) -> second (l <>) <$> y r
            (Right x, l) -> pure (Right x, l) 
    {-# INLINE (<|>) #-} 

instance (Monoid (CobWriter c), Monad m) => MonadPlus (CobT c m) where

instance (Monoid (CobWriter c), Monad m) => MonadReader CobSession (CobT c m) where
    ask = Cob (pure . (, mempty) . Right)
    {-# INLINE ask #-}
    local f m = Cob (unCob m . f)
    {-# INLINE local #-}

instance (Monoid (CobWriter c), Monad m) => MonadError CobError (CobT c m) where
    throwError = Cob . const . pure . (, mempty) . Left
    {-# INLINE throwError #-}
    (Cob x') `catchError` handler = Cob $ \r ->
        x' r >>= \case
          (Left err, l) -> second (l <>) <$> unCob (handler err) r
          _ -> x' r
    {-# INLINE catchError #-}

-- | Allow Cob computations to fail
--
-- For example, when pattern matching against an expected single value in a
-- list, upon failure, cleanly propagate the error
-- 
-- In the following snippet, if more than one instance was found for the given
-- query, a CobError error will be thrown and propagated
-- @
-- [user1] <- rmDefinitionSearch_ "id:456767*" :: [User]
-- @
instance (Monoid (CobWriter c), Monad m) => MonadFail (CobT c m) where
    fail = throwError
    {-# INLINE fail #-}
    -- [x] fail s >>= f = fail s

instance (w ~ CobWriter c, Monoid w, Monad m) => MonadWriter w (CobT c m) where
    tell = Cob . const . pure . (Right (),)
    {-# INLINE tell #-}
    listen a' = Cob (unCob a' >=> \(a, w) -> return (fmap (,w) a, w))
    {-# INLINE listen #-}
    pass (Cob a') = Cob $ \r -> do
        a' r >>= \case
          (Left err, w) -> return (Left err, w)
          (Right (a, f), w) -> return (Right a, f w)
    {-# INLINE pass #-}

instance (Monoid (CobWriter c), MonadIO m) => MonadIO (CobT c m) where
    liftIO = Cob . const . fmap ((, mempty) . Right) . liftIO
    {-# INLINE liftIO #-}
    -- [x] liftIO . return = return
    -- [x] liftIO (m >>= f) = liftIO m >>= (liftIO . f)

-- | Lift a computation from the argument monad to a constructed 'CobT' monad.
--
-- This can be used to do computations in @m@ from 'CobT'.
--
-- For example: lift an 'IO' computation to a 'Cob' computation.
--
-- This can be used to do 'IO' from 'Cob'.
--
-- ==== __Example__
--
-- @
-- logic :: Cob ()
-- logic = do
--     lift (print "started!")
--     ...
--     ...
--     lift (print "finished!")
-- @
instance Monoid (CobWriter c) => MonadTrans (CobT c) where
    lift = Cob . const . fmap ((, mempty) . Right)
    {-# INLINE lift #-}
    -- [x] lift . return = return
    -- [x] lift (m >>= f) = lift m >>= (lift . f)


-- | The inverse of 'CobT'.
--
-- Run a 'CobT' computation and return either a 'CobError' or a value
-- in the argument monad @m@, alongside the writer log
runCobT :: CobSession -> CobT c m a -> m (Either CobError a, CobWriter c)
runCobT = flip unCob
{-# INLINE runCobT #-}


--- Sessions

-- | A priviledge granting 'CobToken'
type CobToken  = String
-- | A cob server host such as \"test.cultofbits.com\".
type Host      = String
-- | A 'Cob' session, required an argument to run a 'Cob' computation with 'runCob'.
-- It should be created using 'makeSession' unless finer control over the TLS
-- session manager or the cobtoken cookie is needed.
data CobSession = CobSession { tlsmanager :: Manager
                             , serverhost :: Host
                             , cobtoken   :: Cookie }

-- | Make a 'CobSession' with the default @TLS Manager@ given the 'Host' and 'CobToken'.
makeSession :: Host -> CobToken -> IO CobSession
makeSession host tok = do
    manager <- newManager tlsManagerSettings
    return $ CobSession manager host $ Cookie { cookie_name             = "cobtoken"
                                              , cookie_value            = fromString tok
                                              , cookie_secure_only      = True
                                              , cookie_path             = "/"
                                              , cookie_domain           = fromString host
                                              , cookie_expiry_time      = future
                                              , cookie_creation_time    = past
                                              , cookie_last_access_time = past
                                              , cookie_persistent       = True
                                              , cookie_host_only        = False
                                              , cookie_http_only        = False }
    where
    past   = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
    future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)


-- | An 'Existable' @e@ possibly wraps a value and provides an operation '?:'
-- to retrieve the element if it exists or return a default value (passed in
-- the second parameter) if it doesn't.
class Existable e where
    -- | Return the value from the 'Existable' if it exists, otherwise return the
    -- default value passed as the second argument
    (??) :: Monad m => e a -> m a -> m a
    infix 7 ??

    -- | Return the value from an 'Existable' in @'Monad' m@ if it exists, otherwise return the
    -- default value passed as the second argument
    (???) :: Monad m => m (e a) -> m a -> m a
    mex ??? def = mex >>= flip (??) def
    infix 7 ???
    {-# INLINE (???) #-}

    -- | Return the value from an 'Existable' in @'Monad' m@ if it exists,
    -- otherwise return 'Alternative' 'empty'
    --
    -- This is best used with the extension @PostfixOperators@ which allows this
    -- kind of idioms to be written:
    --
    -- @
    -- (rmDefinitionSearch query ?!) <|> rmAddInstance newInstance
    -- @
    (?!) :: (Monad m, Alternative m) => m (e a) -> m a
    (?!) = (??? empty)
    {-# INLINE (?!) #-}
    

-- | A 'Maybe' is 'Existable' because it's either 'Just' a value or 'Nothing'.
instance Existable Maybe where
    -- | Return the value from the 'Maybe' if it exists, otherwise return the
    -- default value passed as the second argument
    mb ?? def = maybe def return mb
    {-# INLINE (??) #-}

-- | A list is 'Existable' because it might have no elements or at least one
-- element. When using '?:', if the list is non-empty, the first element will
-- be returned. This is useful when doing a 'rmDefinitionSearch' and are
-- expecting exactly one result, and want to throw an error in the event that
-- none are returned.
--
-- Example:
-- 
-- @
-- user <- rmDefinitionSearch_ @UsersRecord name ?:: throwError "Couldn't find user!"
-- @
instance Existable [] where
    -- | Return the first element of the list if it exists, otherwise return the
    -- default value passed as the second argument
    ls ?? def = (maybe def return . listToMaybe) ls
    {-# INLINE (??) #-}


-- | @Internal@ The default HTTP request used internally (targeting RecordM) in
-- this module, given a 'CobSession'.
-- (Session managed TLS to session host:443 with session's cobtoken)
cobDefaultRequest :: CobSession -> Request
cobDefaultRequest session =
    setRequestManager (tlsmanager session) $
    Net.defaultRequest { secure    = True
                       , port      = 443
                       , host      = fromString $ serverhost session
                       , cookieJar = Just $ createCookieJar [cobtoken session] }


-- | @Internal@
--
-- Perform an HTTP request parsing the response body as JSON
-- If the response status code isn't successful an error is thrown.
-- In the event of a JSON parse error, the error is thrown.
httpValidJSON :: forall a m c. (Monoid (CobWriter c), MonadIO m, Show a, FromJSON a) => Request -> CobT c m a
httpValidJSON request = do

    response <- httpJSONEither request

    let status = responseStatus response
        body   = responseBody @(Either JSONException a) response

    unless (statusIsSuccessful status) $ -- When the status code isn't successful, fail with the status and body as error string
        throwError
        $  ("Request failed with status "
            <> show (statusCode status) <> " -- "
            <> BSC8.unpack (statusMessage status))
        <> ("\nResponse body: "
            <> either prettyBodyFromJSONException show body)

    either (throwError . prettyErrorFromJSONException) return body

    where prettyBodyFromJSONException  JSONParseException {}             = "()"
          prettyBodyFromJSONException  (JSONConversionException _ rv _)  = BSC8.unpack . toStrict . encodePretty . responseBody $ rv
          prettyErrorFromJSONException j = "Error parsing response body: " <> prettyBodyFromJSONException j <> "\nERROR: " <> case j of (JSONParseException _ _ e) -> show e; (JSONConversionException _ _ e) -> e


-- | @Internal@
--
-- Perform an HTTP request and ignore the response body
-- If the response status code isn't successful an error is thrown.
httpValidNoBody :: (Monoid (CobWriter c), MonadIO m) => Request -> CobT c m ()
httpValidNoBody request = do
    response <- httpNoBody request
    let status = responseStatus response
    unless (statusIsSuccessful status) $
        throwError ("Request failed with status "
                    <> show (statusCode status) <> " -- "
                    <> BSC8.unpack (statusMessage status))
