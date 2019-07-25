module UI.App where

import Brick (attrMap, AttrMap, Next, EventM, BrickEvent, CursorLocation, (<+>), str, withBorderStyle, Widget, App(..), continue, halt)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (center)
import Types.AppState
import Types.CustomEvent
import Types.Name
import qualified Graphics.Vty as V

import UI.Projects.Add

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent = handleEvent
            , appStartEvent = startEvent
            , appAttrMap = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI _ = [addProjectWidget]
-- drawUI s = [withBorderStyle unicode $ borderWithLabel (str "Hello!") $ (center (str "Left") <+> vBorder <+> center (str "Right"))]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s _ = halt s

startEvent :: AppState -> EventM Name AppState
startEvent = return

myMap :: AttrMap
myMap = attrMap V.defAttr []
