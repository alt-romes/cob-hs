module Cob.Servant where

import Data.Bifunctor
import Data.String
import Data.Proxy
import Control.Monad ( (<=<) )
import Control.Monad.Except

import Cob
import Servant.Server

type CobServer api = ServerT api (Cob IO)

-- | Natural transformation from Cob IO to Handler to use with hoistServer
--
-- @
-- serve (Proxy @api) $ hoistServer (Proxy @api) (cobServerToHandler session) (cobServer :: CobServer api)
-- @
cobServerToHandler :: CobSession -> Cob IO a -> Handler a
cobServerToHandler session = Handler . liftEither . first (\s -> ServerError 42 "Cob Error" (fromString s) []) <=< liftIO . runCob session
