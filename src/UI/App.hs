{-# LANGUAGE ScopedTypeVariables #-}

module UI.App where

import Brick
import Brick.BChan (BChan)
import Brick.Forms
  ( focusedFormInputAttr,
    invalidFormInputAttr,
  )
import Brick.Types (Padding (Max))
import Brick.Util
  ( fg,
    on,
  )
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedFocusedAttr)
import qualified Config
import Control.Lens
import Control.Monad.Reader
import Data.Maybe (maybeToList)
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events
import Types.AppState
import Types.Brick.CustomEvent
import Types.Brick.Name
import Types.Classes.Fields
import Types.Monads
import UI.Attr
import UI.Events.Handler
  ( handleEvent,
    updateCurrentTime,
  )
import UI.HelpPanel (helpPanel)
import UI.MainWidget (mainWidget)
import UI.Modal (renderModal)
import UI.StatusBar (statusBar)

uiApp :: Config.AppConfig -> BChan CustomEvent -> App AnyAppState CustomEvent Name
uiApp config chan = App
  { appDraw = drawUI config,
    appChooseCursor = showFirstCursor,
    appHandleEvent = brickHandleEvent config chan,
    appStartEvent = startEvent,
    appAttrMap = const myMap
  }

drawUI :: Config.AppConfig -> AnyAppState -> [Widget Name]
drawUI config wrapper@(AnyAppState _ s) =
  let main = statusBar s <=> padBottom Max (mainWidget config wrapper)
      everything =
        if s ^. helpPanelVisible . coerced
          then main <=> hBorder <=> helpPanel (s ^. screen)
          else main
      modalWidget = maybeToList $ renderModal s <$> (s ^. modal)
   in modalWidget ++ [everything]

startEvent :: AnyAppState -> EventM Name AnyAppState
startEvent = pure

-- This is the function that's provided to Brick's `App` and must have this exact signature
-- (note AnyAppState instead of AppState; since the input and output state must have the same type,
-- we can't use AppState which is parameterized by a ScreenTag)
brickHandleEvent ::
  Config.AppConfig ->
  BChan CustomEvent ->
  AnyAppState ->
  BrickEvent Name CustomEvent ->
  EventM Name (Next AnyAppState)
-- Intercept the "quit" event at the top level in order to make sure that it halts the program immediately.
-- Otherwise, delegate the handling to our own function. Note that we want to update the currentTime
-- with every event.
brickHandleEvent config chan s ev =
  let doHandle :: AppM AnyAppState = handleEvent chan s ev >>= updateCurrentTime
      Config.AppKey quitKey quitMods = config ^. keymap ^. quit
   in if ev == VtyEvent (EvKey quitKey quitMods)
        then halt s
        else runReaderT (runAppM doHandle) config >>= continue

myMap :: AttrMap
myMap =
  attrMap
    V.defAttr
    [ (errorAttr, V.white `on` V.red),
      (listSelectedFocusedAttr, V.white `on` V.blue),
      (invalidFormInputAttr, V.white `on` V.red),
      (editFocusedAttr, V.black `on` V.cyan),
      (focusedFormInputAttr, V.black `on` V.cyan),
      (keyValueKeyAttr, fg V.blue),
      (keyValueValueAttr, fg V.green),
      (disabledAttr, fg (V.Color240 252)),
      (jsonKeyAttr, fg V.blue),
      (jsonStringAttr, fg V.green),
      (jsonNumberAttr, fg V.cyan),
      (jsonBoolAttr, fg V.magenta),
      (jsonNullAttr, V.white `on` V.black),
      (methodAttr, fg V.magenta),
      (explanationAttr, fg V.cyan),
      (importantExplanationAttr, V.white `on` V.blue),
      (searchPlaceholderAttr, fg V.magenta),
      (searchSectionAttr, fg V.green),
      (templatedVariableAttr, fg V.blue),
      (statusBarAttr, V.blue `on` V.white),
      (environmentNameAttr, fg V.magenta),
      (statusCode200Attr, fg V.green),
      (statusCode300Attr, fg V.yellow),
      (statusCode400Attr, fg V.red),
      (statusCode500Attr, fg V.red),
      (focusedBorderAttr, fg V.yellow)
    ]
