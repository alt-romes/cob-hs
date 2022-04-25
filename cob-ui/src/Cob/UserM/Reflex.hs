{-# LANGUAGE MonoLocalBinds #-}
module Cob.UserM.Reflex where

import Data.Text (Text, unpack)

import Control.Monad.IO.Class
import System.IO

import qualified Cob.UserM as UM
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
            res <- runCob ess (UM.umLogin (unpack u') (unpack p'))
            case res of
              Left err -> hPutStrLn stderr err >> return Nothing
              Right tok -> return $ Just $ updateSessionToken ess tok

