{-# LANGUAGE GHC2021, UnicodeSyntax, DataKinds, BlockArguments, TypeOperators, RankNTypes #-}
module Cob.Servant where

import Data.Kind
import Data.Bifunctor
import Data.String
import Data.Proxy
import Control.Monad ( (<=<) )
import Control.Monad.Except

import Cob
import Servant.Server

type CobServer api = ServerT api Cob

-- | Natural transformation from Cob to Handler to use with hoistServer
-- 
-- Receives an interpreter to run the Cob computation (e.g.  'runCob' or
-- 'mockCob' applied to a cob session) and returns a natural transformation
-- between a 'Cob' computation and a servant 'Handler'.
--
-- @
-- serve (Proxy @api) $ hoistServer (Proxy @api) (cobServerToHandler (runCob session)) (cobServer :: CobServer api)
-- @
cobServerToHandler :: (Cob ~> IO) -> (Cob ~> Handler)
-- We don't catch exceptions to re-throw as ServantErrors here. That
-- responsibility falls on the programmer writing the cob computation.
-- If exceptions occur in the handler they will be thrown as 500 errors, which
-- is what we want to do for generic exceptions that weren't caught by the
-- programmer
cobServerToHandler runner cob = Handler (ExceptT $ Right <$> runner cob)

-- | Batteries included 'serve' for 'CobServer'
serveCobServer :: ∀ (api :: Type). HasServer api '[] => Proxy api -> (Cob ~> IO) -> CobServer api -> Application
serveCobServer prox runner cobServer = serve prox $ hoistServer prox (cobServerToHandler runner) cobServer

