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
import           Brick.Forms                    ( invalidFormInputAttr )
import           Brick.Types                    ( Padding(Max) )
import           Brick.Util
import           Brick.Widgets.Border           ( border
                                                , hBorder
                                                )
import           Brick.Widgets.Border.Style     ( unicodeRounded )
import           Brick.Widgets.List             ( listSelectedFocusedAttr )
import           Control.Lens
import           Data.Maybe                     ( maybeToList )
import qualified Graphics.Vty                  as V
import           Types.AppState
import           Types.Brick.CustomEvent
import           Types.Brick.Name
import           UI.Attr
import           UI.Console                     ( console )
import           UI.EventHandler                ( handleEvent )
import           UI.HelpPanel                   ( helpPanel )
import           UI.MainWidget                  ( mainWidget )
import           UI.Modal                       ( renderModal )
import           UI.Title                       ( title )

uiApp :: App AnyAppState CustomEvent Name
uiApp = App { appDraw         = drawUI
            , appChooseCursor = showFirstCursor
            , appHandleEvent  = handleEvent
            , appStartEvent   = startEvent
            , appAttrMap      = const myMap
            }

drawUI :: AnyAppState -> [Widget Name]
drawUI wrapper@(AnyAppState s) =
  let titleLine = txt $ title s
      titleAndMain =
          titleLine <=> hBorder <=> padBottom Max (mainWidget wrapper)
      everything = if s ^. helpPanelVisible . coerced
        then titleAndMain <=> hBorder <=> helpPanel (s ^. screen)
        else titleAndMain
      borderedEverything =
          withBorderStyle unicodeRounded $ (joinBorders . border) everything
      modalWidget  = maybeToList $ renderModal s <$> (s ^. modal)
      maybeConsole = console (s ^. messages)
  in  if s ^. consoleVisible . coerced
        then [maybeConsole]
        else modalWidget ++ [borderedEverything]
startEvent :: AnyAppState -> EventM Name AnyAppState
startEvent = return

myMap :: AttrMap
myMap = attrMap
  V.defAttr
  [ (listSelectedFocusedAttr, V.green `on` V.black)
  , (invalidFormInputAttr   , V.white `on` V.red)
  , (highlighted            , Brick.Util.bg V.blue)
  ]
