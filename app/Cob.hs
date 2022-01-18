{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, OverloadedStrings #-}
module Cob where

import Data.ByteString (ByteString)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT, runExceptT, except, throwE)

import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

import Network.HTTP.Conduit as Net (Manager, Cookie(..), Request(..), defaultRequest, newManager, tlsManagerSettings, createCookieJar)
import Network.HTTP.Simple (setRequestManager)

-- | The Cob Monad.
-- A context in which computations that interact with @RecordM@ can be executed.
-- 
-- See 'CobT' for more.
type Cob a = CobT IO a

-- | Run a 'Cob' computation and get either a 'CobError' or a value in an 'IO' context.
runCob :: CobSession -> Cob a -> IO (Either CobError a)
runCob = runCobT

-- | Lift an 'IO' computation to a 'Cob' computation.
--
-- This can be used to do 'IO' from 'Cob'.
--
-- ==== __Example__
--
-- @
-- logic :: Cob ()
-- logic = do
--     liftCob (print "started!")
--     ...
--     ...
--     liftCob (print "finished!")
-- @
liftCob :: IO a -> Cob a
liftCob = liftCobT

-- | A 'Cob' computation will either succeed or return a 'CobError'.
type CobError = String

-- | The 'CobT' monad transformer.
--
-- A constructed monad made out of an existing monad @m@ such that its
-- computations can be embedded in 'CobT', from which it's also possible to
-- interact with @RecordM@.
type CobT m a = ExceptT CobError (ReaderT CobSession m) a

-- | The inverse of 'CobT'.
--
-- Run a 'CobT' computation and return either a 'CobError' or a value
-- in the argument monad @m@.
runCobT :: Monad m => CobSession -> CobT m a -> m (Either CobError a)
runCobT session cob = runReaderT (runExceptT cob) session


-- | Lift a computation from the argument monad to a constructed 'CobT' monad.
--
-- This can be used to do computations in @m@ from 'CobT'.
liftCobT :: Monad m => m a -> CobT m a
liftCobT = lift . lift


--- Sessions

-- | A priviledge granting 'CobToken'
type CobToken  = ByteString
-- | A cob server host such as \"test.cultofbits.com\".
type Host      = ByteString
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
                                              , cookie_value            = tok
                                              , cookie_secure_only      = True
                                              , cookie_path             = "/"
                                              , cookie_domain           = host
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
                       , host      = serverhost session
                       , cookieJar = Just $ createCookieJar [cobtoken session] }


