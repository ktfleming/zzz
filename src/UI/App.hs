{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module UI.App where

import Brick (BrickEvent(VtyEvent), attrMap, AttrMap, Next, EventM, BrickEvent, CursorLocation, (<+>), str, withBorderStyle, Widget, App(..), continue, halt)
import Brick.Forms
import Brick.Types (Next)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (center)
import Lens.Micro.Platform
import Types.AppState
import Types.CustomEvent
import Types.Name
import Types.Screen
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events
import UI.HelpScreen
import UI.Projects.Add

makeLenses ''AppState

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent = handleEvent
            , appStartEvent = startEvent
            , appAttrMap = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI AppState { _activeForm = Just form } = [renderForm form]
drawUI s = case _activeScreen s of
  HelpScreen -> [helpWidget]
  ProjectScreen -> [helpWidget] -- TODO: this is just temporary
-- drawUI s = [withBorderStyle unicode $ borderWithLabel (str "Hello!") $ (center (str "Left") <+> vBorder <+> center (str "Right"))]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent :: forall a. AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s @ AppState { _activeScreen, _allProjects, _activeForm = Just form } ev =
  let newForm = handleFormEvent ev form
      mapper :: forall a. Form a CustomEvent Name -> EventM Name (Next AppState)
      mapper form = continue $ AppState { _activeScreen, _allProjects, _activeForm = Just form }
  in newForm >>= mapper
handleEvent s (VtyEvent (EvKey (KChar 'h') [])) = continue $ activeScreen .~ HelpScreen $ s
handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s  -- 'q' to quit
handleEvent s _ = continue s

startEvent :: AppState -> EventM Name AppState
startEvent = return

myMap :: AttrMap
myMap = attrMap V.defAttr []
