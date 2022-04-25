{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Text hiding (init)
import Control.Monad.Reader

import Cob
import Cob.UserM
import Cob.RecordM
import Cob.RecordM.TH
import Cob.RecordM.Reflex
import Cob.UserM.Reflex
import Reflex.Dom

newtype Todo = Todo { todoText :: Text }
mkRecord ''Todo "ROMES Todos" ["Todo"]

main :: IO ()
main = do
    cobToken <- init <$> readFile "../cob-token.secret"
    session  <- makeSession "mimes8.cultofbits.com" cobToken
    return ()

-- newTodoForm :: MonadWidget t m => m (Event t Todo)
-- newTodoForm = divClass "field has-addons has-addons-centered" $ do
--     newTodoDyn <- fmap Todo . value <$> divClass "control" (inputElement (def & initialAttributes .~ ("type" =: "text" <> "class" =: "input")))
--     (btnEl, _) <- divClass "control" $ elClass' "a" "button" $ text "Add Todo"
--     pure $ tagPromptlyDyn newTodoDyn $ domEvent Click btnEl

-- todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
-- todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)

