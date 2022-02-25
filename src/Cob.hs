{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Cob where

import Data.String (fromString)
import Data.ByteString (ByteString)

import Control.Monad.Except   (MonadError, throwError, catchError)
import Control.Monad.Reader   (MonadReader, ask, local)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans    (MonadTrans, lift)

import Data.DList (DList, empty)
import Data.Bifunctor (first, second, bimap)

import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

import Network.HTTP.Conduit as Net (Manager, Cookie(..), Request(..), defaultRequest, newManager, tlsManagerSettings, createCookieJar)
import Network.HTTP.Simple (setRequestManager)

-- | The Cob Monad.
-- A context in which computations that interact with @RecordM@ can be executed.
-- 
-- See 'CobT' for more.
type Cob a = CobT () IO a

-- | Run a 'Cob' computation and get either a 'CobError' or a value in an 'IO' context.
runCob :: CobSession -> Cob a -> IO (Either CobError a)
runCob = runCobT
{-# INLINE runCob #-}

-- | A 'Cob' computation will either succeed or return a 'CobError'.
type CobError = String

-- | The 'CobT' monad transformer.
--
-- A constructed monad made out of an existing monad @m@ such that its
-- computations can be embedded in 'CobT', from which it's also possible to
-- interact with @RecordM@.
--
-- The @w@ parameter stands for writer -- a Cob computation supports writing to 
newtype CobT w m a = Cob { unCob :: CobSession -> m (Either CobError a, DList w) }

instance Functor m => Functor (CobT w m) where
    fmap f = Cob . (fmap . fmap) (first (fmap f)) . unCob
    {-# INLINE fmap #-}
    -- [x] fmap id = id

instance Applicative m => Applicative (CobT w m) where
    pure = Cob . const . pure . (, empty) . Right
    {-# INLINE pure #-}
    (Cob f') <*> (Cob g) = Cob $ \r ->
        (\case (Left e, l) -> const (Left e, l); (Right f, l) -> bimap (fmap f) (l <>)) <$> f' r <*> g r
    {-# INLINE (<*>) #-}
    -- [x] pure id <*> v = v
    -- [x] pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- [x] pure f <*> pure x = pure (f x)
    -- [x] u <*> pure y = pure ($ y) <*> u

instance Monad m => Monad (CobT w m) where
    (Cob x') >>= f' = Cob $ \r ->
        x' r >>= \case
          (Left err, l) -> return (Left err, l)
          (Right x, l)  -> second (l <>) <$> unCob (f' x) r
    {-# INLINE (>>=) #-}
    -- [x] return a >>= k = k a
    -- [x] m >>= return = m
    -- [x] m >>= (\x -> k x >>= h) = (m >>= k) >>= h

instance Monad m => MonadReader CobSession (CobT w m) where
    ask = Cob (pure . (, empty) . Right)
    {-# INLINE ask #-}
    local f m = Cob (unCob m . f)
    {-# INLINE local #-}

instance Monad m => MonadError CobError (CobT w m) where
    throwError = Cob . const . pure . (, empty) . Left
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
instance Monad m => MonadFail (CobT w m) where
    fail = throwError
    {-# INLINE fail #-}
    -- [x] fail s >>= f = fail s

instance MonadIO m => MonadIO (CobT w m) where
    liftIO = Cob . const . fmap ((, empty) . Right) . liftIO
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
instance MonadTrans (CobT w) where
    lift = Cob . const . fmap ((, empty) . Right)
    {-# INLINE lift #-}
    -- [x] lift . return = return
    -- [x] lift (m >>= f) = lift m >>= (lift . f)


-- | The inverse of 'CobT'.
--
-- Run a 'CobT' computation and return either a 'CobError' or a value
-- in the argument monad @m@.
runCobT :: Monad m => CobSession -> CobT w m a -> m (Either CobError a)
runCobT session cob = fst <$> unCob cob session
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


