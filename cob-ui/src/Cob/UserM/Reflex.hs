{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Cob.UserM.Reflex where

import Data.Text (Text, unpack)

import Control.Exception (SomeException)

import Control.Monad.IO.Class
import System.IO

import Cob
import Reflex.Dom

userLogin :: MonadWidget t m => Behavior t Text -> Behavior t Text -> Event t a
          -> Host -> m (Event t (Maybe CobSession))
userLogin u p e host = do
    performEvent ((do
        u' <- sample $ u
        p' <- sample $ p
        liftIO $ runCobEvent u' p') <$ e)
    where
        runCobEvent u' p' = do
            ess <- (emptySession host)
            res <- runCob ess ((Right <$> login (unpack u') (unpack p')) `catch` \(e::SomeException) -> pure (Left (show e)))
            case res of
              Left err -> hPutStrLn stderr err >> return Nothing
              Right tok -> Just <$> updateSessionToken ess tok

