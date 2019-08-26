module UI.App where

import           Brick                          ( App(..)
                                                , AttrMap
                                                , EventM
                                                , Widget
                                                , attrMap
                                                , padBottom
                                                , showFirstCursor
                                                , txt
                                                , (<=>)
                                                )
import           Brick.BChan                    ( BChan )
import           Brick.Forms                    ( focusedFormInputAttr
                                                , invalidFormInputAttr
                                                )
import           Brick.Types                    ( Padding(Max) )
import           Brick.Util
import           Brick.Widgets.Border           ( hBorder )
import           Brick.Widgets.Edit             ( editFocusedAttr )
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

uiApp :: BChan CustomEvent -> App AnyAppState CustomEvent Name
uiApp chan = App { appDraw         = drawUI
                 , appChooseCursor = showFirstCursor
                 , appHandleEvent  = handleEvent chan
                 , appStartEvent   = startEvent
                 , appAttrMap      = const myMap
                 }

drawUI :: AnyAppState -> [Widget Name]
drawUI wrapper@(AnyAppState s) =
  let titleLine    = txt $ title s
      titleAndMain = titleLine <=> hBorder <=> padBottom Max (mainWidget wrapper)
      everything   = if s ^. helpPanelVisible . coerced
        then titleAndMain <=> hBorder <=> helpPanel (s ^. screen)
        else titleAndMain
      modalWidget  = maybeToList $ renderModal s <$> (s ^. modal)
      maybeConsole = console (s ^. messages)
  in  if s ^. consoleVisible . coerced then [maybeConsole] else modalWidget ++ [everything]
startEvent :: AnyAppState -> EventM Name AnyAppState
startEvent = return

myMap :: AttrMap
myMap = attrMap
  V.defAttr
  [ (listSelectedFocusedAttr, V.white `on` V.blue)
  , (invalidFormInputAttr   , V.white `on` V.red)
  , (editFocusedAttr        , V.black `on` V.cyan)
  , (focusedFormInputAttr   , V.black `on` V.cyan)
  , (headerNameAttr         , Brick.Util.fg V.blue)
  , (headerValueAttr        , Brick.Util.fg V.green)
  , (disabledAttr           , Brick.Util.fg (V.Color240 252))
  , (jsonKeyAttr            , Brick.Util.fg V.blue)
  , (jsonStringAttr         , Brick.Util.fg V.green)
  , (jsonNumberAttr         , Brick.Util.fg V.cyan)
  , (jsonBoolAttr           , Brick.Util.fg V.magenta)
  , (jsonNullAttr           , V.white `on` V.black)
  ]
