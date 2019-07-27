{-# LANGUAGE TemplateHaskell #-}

module UI.App where

import Brick (BrickEvent(VtyEvent), attrMap, AttrMap, Next, EventM, BrickEvent, CursorLocation, (<+>), str, withBorderStyle, Widget, App(..), continue, halt)
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
drawUI s =
  let activeScreenWidget = case (_activeScreen s) of
        ProjectScreen -> addProjectWidget
        HelpScreen -> helpWidget
  in [activeScreenWidget]
-- drawUI s = [withBorderStyle unicode $ borderWithLabel (str "Hello!") $ (center (str "Left") <+> vBorder <+> center (str "Right"))]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s  -- 'q' to quit
handleEvent s (VtyEvent (EvKey (KChar 'h') [])) = continue $ activeScreen %~ (const HelpScreen) $ s
handleEvent s _ = continue s
-- TODO: pattern match here. Widget event -> delegate to widget event handler
-- press 'h' key -> show help screen
-- default -> continue

startEvent :: AppState -> EventM Name AppState
startEvent = return

myMap :: AttrMap
myMap = attrMap V.defAttr []
