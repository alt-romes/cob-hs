{-# LANGUAGE ConstraintKinds #-} 
{-# LANGUAGE InstanceSigs #-} 
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

-- TODO: Cob.Simple module

import Data.String                   ( fromString )
import Data.ByteString.Char8 as BSC8 ( unpack     )
import Data.ByteString.Lazy          ( toStrict   )

import Data.DList ( DList )

import Data.Maybe ( listToMaybe )

import Data.Aeson               ( FromJSON )
import Data.Aeson.Encode.Pretty ( encodePretty )

import Control.Applicative ( Alternative, empty, (<|>) )
import Control.Monad       ( (>=>), unless )

import Control.Monad.Except   ( MonadError, throwError, catchError )
import Control.Monad.Reader   ( MonadReader, ask, local            )
import Control.Monad.Writer   ( MonadWriter, tell, listen, pass    )
import Control.Monad.IO.Class ( MonadIO, liftIO                    )
import Control.Monad.Trans    ( MonadTrans, lift                   )

import Control.Exception as E ( catch )

import Data.Bifunctor (first, second, bimap)

import Data.Time.Clock    (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

import Network.HTTP.Conduit (Manager, Cookie(..), Request(..), Response(..), defaultRequest, newManager, tlsManagerSettings, createCookieJar)
import Network.HTTP.Simple (setRequestManager, JSONException(..), HttpException, httpJSONEither, httpNoBody)
import Network.HTTP.Types  (statusIsSuccessful, Status(..))

-- | The 'Cob' monad (transformer).
--
-- A constructed monad made out of an existing monad @m@ such that its
-- computations can be embedded in 'Cob', from which it's also possible to
-- interact with @RecordM@ and @UserM@.
--
newtype Cob m a = Cob { unCob :: CobSession -> m (Either CobError a, CobWriters) }

-- | A 'Cob' computation will either succeed or return a 'CobError'.
type CobError = String

-- | The 'Cob' modules
data CobModule = RecordM | UserM

-- | CobWriters -- Cob is a MonadWriter whose monoid writer instance is a difference list for each module's specific requirements
type CobWriters = (DList (CobWriter 'RecordM), DList (CobWriter 'UserM))

-- | Define which kind of Monoid writer each module uses
--
-- The full 'Cob' writer is a tuple @(CobWriter 'RecordM, CobWriter 'Userm)@
type family CobWriter (c :: CobModule)

instance Functor m => Functor (Cob m) where
    fmap f = Cob . (fmap . fmap) (first (fmap f)) . unCob
    {-# INLINE fmap #-}
    -- [x] fmap id = id

instance Monad m => Applicative (Cob m) where
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

instance Monad m => Monad (Cob m) where
    (Cob x') >>= f' = Cob $ \r ->
        x' r >>= \case
          (Left err, l) -> return (Left err, l)
          (Right x, l)  -> second (l <>) <$> unCob (f' x) r
    {-# INLINE (>>=) #-}
    -- [x] return a >>= k = k a
    -- [x] m >>= return = m
    -- [x] m >>= (\x -> k x >>= h) = (m >>= k) >>= h

instance Monad m => Alternative (Cob m) where
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
            (Left _, l) -> second (l <>) <$> y r
            (Right x, l) -> pure (Right x, l) 
    {-# INLINE (<|>) #-} 

-- Cob doesn't instance MonadPlus because mzero isn't a right zero but rather is
-- mzero + the accumulators from the left computation
-- instance Monad m => MonadPlus (Cob m) where
    -- ...
    -- mzero >>= f  =  mzero
    -- v >> mzero   =  mzero

instance Monad m => MonadReader CobSession (Cob m) where
    ask = Cob (pure . (, mempty) . Right)
    {-# INLINE ask #-}
    local f m = Cob (unCob m . f)
    {-# INLINE local #-}

instance Monad m => MonadError CobError (Cob m) where
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
instance Monad m => MonadFail (Cob m) where
    fail = throwError
    {-# INLINE fail #-}
    -- [x] fail s >>= f = fail s

instance (w ~ CobWriters, Monad m) => MonadWriter w (Cob m) where
    tell = Cob . const . pure . (Right (),)
    {-# INLINE tell #-}
    listen a' = Cob (unCob a' >=> \(a, w) -> return (fmap (,w) a, w))
    {-# INLINE listen #-}
    pass (Cob a') = Cob $ \r -> do
        a' r >>= \case
          (Left err, w) -> return (Left err, w)
          (Right (a, f), w) -> return (Right a, f w)
    {-# INLINE pass #-}

instance MonadIO m => MonadIO (Cob m) where
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
instance MonadTrans Cob where
    lift = Cob . const . fmap ((, mempty) . Right)
    {-# INLINE lift #-}
    -- [x]??lift . return = return
    -- [x]??lift (m >>= f) = lift m >>= (lift . f)


-- | The inverse of 'Cob'.
--
-- Run a 'Cob' computation and get either a 'CobError' or a value
-- in the argument monad @m@, alongside the 'CobWriter's logs
runCobT :: CobSession -> Cob m a -> m (Either CobError a, CobWriters)
runCobT = flip unCob
{-# INLINE runCobT #-}

-- | Run a 'Cob' computation and get either a 'CobError' or a value in @m@.
runCob :: Functor m => CobSession -> Cob m a -> m (Either CobError a)
runCob session = fmap fst . runCobT session
{-# INLINE runCob #-}

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
makeSession cobhost tok = do
    manager <- newManager tlsManagerSettings
    return $ CobSession manager cobhost $
        Cookie { cookie_name             = "cobtoken"
               , cookie_value            = fromString tok
               , cookie_secure_only      = True
               , cookie_path             = "/"
               , cookie_domain           = fromString cobhost
               , cookie_expiry_time      = future
               , cookie_creation_time    = past
               , cookie_last_access_time = past
               , cookie_persistent       = True
               , cookie_host_only        = False
               , cookie_http_only        = False }
    where
    past   = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
    future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

-- | Create an empty 'CobSession', i.e. without a token
emptySession :: Host -> IO CobSession
emptySession = flip makeSession ""

-- | Update the 'CobToken' of a 'CobSession'
updateSessionToken :: CobSession -> CobToken -> CobSession
updateSessionToken s t = s { cobtoken = (cobtoken s) { cookie_value = fromString t } }

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


-- | @Internal@ The default HTTP request used internally, given a 'CobSession'.
-- (Session managed TLS to session host:443 with session's cobtoken)
cobDefaultRequest :: CobSession -> Request
cobDefaultRequest session =
    setRequestManager (tlsmanager session) $
        defaultRequest { secure    = True
                       , port      = 443
                       , host      = fromString $ serverhost session
                       , cookieJar = Just $ createCookieJar [cobtoken session] }

-- | @Internal@
--
-- Perform an HTTP request parsing the response body as JSON
-- If the response status code isn't successful an error is thrown.
-- In the event of a JSON parse error, the error is thrown.
httpValidJSON :: forall a m. (MonadIO m, Show a, FromJSON a) => Request -> Cob m a
httpValidJSON = flip httpValidJSON' id
{-# INLINE httpValidJSON #-}


-- | @Internal@
--
-- Perform an HTTP request parsing the response body as JSON
-- If the response status code isn't successful an error created with a function
-- from @(String -> error_type)@ is thrown.
-- In the event of a JSON parse error, the error is thrown.
httpValidJSON' :: forall a e m. (MonadIO m, MonadError e m, Show a, FromJSON a) => Request -> (String -> e) -> m a
httpValidJSON' request mkError = do

    response <- liftIO ((Right <$> httpJSONEither request) `E.catch` (return . Left @HttpException)) >>= either (throwError . mkError . show) return -- Handle Http Exceptions

    let status = responseStatus response
        body   = responseBody @(Either JSONException a) response

    unless (statusIsSuccessful status) $ -- When the status code isn't successful, fail with the status and body as error string
        throwError . mkError
        $  ("Request failed with a status of "
            <> show (statusCode status) <> " ("
            <> BSC8.unpack (statusMessage status) <> ")")
        <> ("\nResponse body: "
            <> either prettyBodyFromJSONException show body)

    either (throwError . mkError . prettyErrorFromJSONException) return body

    where prettyBodyFromJSONException  JSONParseException {}             = "()"
          prettyBodyFromJSONException  (JSONConversionException _ rv _)  = BSC8.unpack . toStrict . encodePretty . responseBody $ rv
          prettyErrorFromJSONException j = "Error parsing response body: " <> prettyBodyFromJSONException j <> "\nERROR: " <> case j of (JSONParseException _ _ e) -> show e; (JSONConversionException _ _ e) -> e


-- | @Internal@
--
-- Perform an HTTP request and ignore the response body
-- If the response status code isn't successful an error is thrown.
httpValidNoBody :: MonadIO m => Request -> Cob m ()
httpValidNoBody request = do
    response <- liftIO ((Right <$> httpNoBody request) `E.catch` (return . Left @HttpException)) >>= either (throwError . show) return -- Handle Http Exceptions
    let status = responseStatus response
    unless (statusIsSuccessful status) $
        throwError ("Request failed with a status of "
            <> show (statusCode status) <> " ("
            <> BSC8.unpack (statusMessage status) <> ")")
