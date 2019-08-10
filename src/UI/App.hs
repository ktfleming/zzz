module UI.App where

import           Brick                          ( App(..)
                                                , AttrMap
                                                , EventM
                                                , Widget
                                                , attrMap
                                                , joinBorders
                                                , padBottom
                                                , showFirstCursor
                                                , txt
                                                , withBorderStyle
                                                , (<=>)
                                                )
import           Brick.Types                    ( Padding(Max) )
import           Brick.Util
import           Brick.Widgets.Border           ( border
                                                , hBorder
                                                )
import           Brick.Widgets.Border.Style     ( unicodeRounded )
import           Brick.Widgets.List             ( listSelectedFocusedAttr )
import           Control.Lens
import           Data.Maybe                     ( maybeToList )
import           Graphics.Vty                   ( withForeColor )
import qualified Graphics.Vty                  as V
import           Types.AppState
import           Types.Brick.CustomEvent
import           Types.Brick.Name
import           UI.Attr
import           UI.EventHandler                ( handleEvent )
import           UI.HelpPanel                   ( helpPanel )
import           UI.MainWidget                  ( mainWidget )
import           UI.Modal                       ( renderModal )
import           UI.Title                       ( title )

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw         = drawUI
            , appChooseCursor = showFirstCursor
            , appHandleEvent  = handleEvent
            , appStartEvent   = startEvent
            , appAttrMap      = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI s =
  let titleLine = txt $ title s (s ^. screen)
      everything =
          titleLine
            <=> hBorder
            <=> padBottom Max (mainWidget s)
            <=> hBorder
            <=> helpPanel (s ^. screen)
      borderedEverything =
          withBorderStyle unicodeRounded $ (joinBorders . border) everything
      modalWidget = maybeToList $ renderModal s <$> (s ^. modal)
  in  modalWidget ++ [borderedEverything]

startEvent :: AppState -> EventM Name AppState
startEvent = return

myMap :: AttrMap
myMap = attrMap
  V.defAttr
  [ (listSelectedFocusedAttr, withForeColor (Brick.Util.bg V.green) V.black)
  , (highlighted            , Brick.Util.bg V.blue)
  ]
