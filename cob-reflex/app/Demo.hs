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

-- View

-- | A default <head></head> with metas and a stylesheet
headWidget :: MonadWidget t m => m ()
headWidget = do
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
    -- elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css") blank
    elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank

-- | A styled <section>
section :: DomBuilder t m => m a -> m a
section = elClass "section" "section" . divClass "container has-text-centered"


main :: IO ()
main = do
    cobToken <- init <$> readFile "../cob-token.secret"
    session  <- makeSession "mimes8.cultofbits.com" cobToken
    mainWidgetWithHead headWidget (rootWidget session)

rootWidget :: MonadWidget t m => CobSession -> m ()
rootWidget session = section $ do
    elClass "h2" "title" (text "Todos")

    newTodoEv <- newTodoForm
    todosDyn  <- foldDyn (:) [] newTodoEv

    el "hr" blank
    todoListWidget (todosDyn)

newTodoForm :: MonadWidget t m => m (Event t Todo)
newTodoForm = divClass "field has-addons has-addons-centered" $ do
    newTodoDyn <- fmap Todo . value <$> divClass "control" (inputElement (def & initialAttributes .~ ("type" =: "text" <> "class" =: "input")))
    (btnEl, _) <- divClass "control" $ elClass' "a" "button" $ text "Add Todo"
    pure $ tagPromptlyDyn newTodoDyn $ domEvent Click btnEl

todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)

