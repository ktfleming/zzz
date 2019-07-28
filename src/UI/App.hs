{-# LANGUAGE ScopedTypeVariables #-}

module UI.App where

import Prelude hiding (writeFile)

import Brick (BrickEvent(VtyEvent), attrMap, AttrMap, Next, EventM, BrickEvent, CursorLocation, (<+>), str, withBorderStyle, Widget, App(..), continue, halt)
import Brick.Forms
import Brick.Types (Next)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (center)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString, writeFile)
import Lens.Micro.Platform
import Types.AppState
import Types.CustomEvent
import Types.Name
import Types.Screen
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events
import UI.HelpScreen
import UI.Projects.Add

import Debug.Trace

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent = handleEvent
            , appStartEvent = startEvent
            , appAttrMap = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI AppState { _activeForm = ActiveForm (Just form) } = [renderForm form]
drawUI s = case _activeScreen s of
  HelpScreen -> [helpWidget]
  ProjectScreen -> [helpWidget] -- TODO: this is just temporary
-- drawUI s = [withBorderStyle unicode $ borderWithLabel (str "Hello!") $ (center (str "Left") <+> vBorder <+> center (str "Right"))]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent :: forall a. AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey KEsc [])) = trace (show s) $ continue s -- For debugging
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) = liftIO (saveState s) >> continue s

-- When a form is active, use `handleFormEvent` to send events to the form, unless the Enter key is pressed, in which case
-- we activate the form's submit handler
handleEvent s @ AppState { _activeForm = ActiveForm (Just form) } ev =
  case ev of
    VtyEvent (EvKey KEnter []) -> continue $ handleSubmit s form
    _ -> let newForm = fmap (ActiveForm . Just) (handleFormEvent ev form)
             mapper :: ActiveForm -> EventM Name (Next AppState)
             mapper form = continue $ activeForm .~ form $ s
         in newForm >>= mapper
handleEvent s (VtyEvent (EvKey (KChar 'h') [])) = continue $ activeScreen .~ HelpScreen $ s
handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s  -- 'q' to quit
handleEvent s _ = continue s

startEvent :: AppState -> EventM Name AppState
startEvent = return

myMap :: AttrMap
myMap = attrMap V.defAttr []

saveState :: AppState -> IO ()
saveState s =
  let jsonified :: ByteString = encodePretty s
  in trace "writing file" $ writeFile "out.json" jsonified
