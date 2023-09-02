{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Examinator.View where

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Brick
import Cob
import qualified Graphics.Vty as V
import Lens.Micro

import Examinator
import Data.Maybe

-- | The state of the application.
-- The list of tests and the name of the test we're currently taking.
newtype AppState = AppState { _tests :: L.List () Test }
suffixLenses ''AppState

-- We'll likely need a timer event to trigger us to check whether the problem was completed.
examinatorApp :: App AppState e ()
examinatorApp
  = App { appDraw = draw
        , appChooseCursor = chooseCursor
        , appHandleEvent = handleEvent
        , appStartEvent = startEvent
        , appAttrMap = mkAttrMap
        }

draw :: AppState -> [Widget ()]
draw s =
  [ hBox
    [ drawTestList s
    , drawSelectedTest s
    ]
  ]

drawTestList :: AppState -> Widget ()
drawTestList s =
  B.borderWithLabel (str "Tests") $
  hLimit 10 $ C.hCenter $
  L.renderList go True (s ^. _testsL) where
  go s (Test name ts)
    | all snd ts
    = C.hCenter $ withAttr green n'
    | otherwise
    = C.hCenter n'
    where
      n' = if s then withAttr whiteBG (str name) else str name

drawSelectedTest :: AppState -> Widget ()
drawSelectedTest s =
  let Test name prompts = fromMaybe (error "OK") $ s ^? _testsL . L.listSelectedElementL
   in 
    B.borderWithLabel (str name) $
    vBox $ map go prompts where
    go (Prompt text _verify, b)
      | b
      = C.hCenter $ withAttr green n'
      | otherwise
      = C.hCenter n'
      where
        n' = str "* " <+> str text

chooseCursor :: AppState -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor = showFirstCursor

handleEvent :: BrickEvent () e -> EventM () AppState ()
handleEvent (VtyEvent e)
  = case e of
      V.EvKey (V.KChar 'q') [] -> halt
      ev -> zoom _testsL $ L.handleListEventVi L.handleListEvent ev
handleEvent _ = return ()

startEvent :: EventM n s ()
startEvent = pure ()

mkAttrMap :: AppState -> AttrMap
mkAttrMap _ = attrMap V.defAttr
    [ (red, fg V.red)
    , (green, fg V.green)
    , (whiteBG, V.white `on` V.black)
    , (whiteBG <> green, V.green `on` V.white)
    ]

red, green, whiteBG :: AttrName
red = attrName "red"
green = attrName "green"
whiteBG = attrName "whiteBG"
