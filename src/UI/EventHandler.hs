{-# LANGUAGE ScopedTypeVariables #-}

module UI.EventHandler
  ( handleEvent
  )
where

import           Brick                          ( BrickEvent(..)
                                                , EventM
                                                , Next
                                                , continue
                                                , halt
                                                )
import           Prelude                 hiding ( writeFile )

import           Types.Brick.CustomEvent        ( CustomEvent )

import           Types.Brick.Name               ( Name(..) )

import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import           Graphics.Vty.Input.Events
import           Types.AppState
import           Types.Constants                ( mainSettingsFile
                                                , responseHistoryFile
                                                )
import           UI.Console                     ( toggleConsole )
import           UI.Modal                       ( dismissModal
                                                , handleConfirm
                                                )

import           Control.Monad.Trans.State.Lazy ( StateT
                                                , execStateT
                                                , get
                                                , modify
                                                )
import           Types.Classes.EventHandler     ( handle )

-- This is the function that's provided to Brick's `App` and must have this exact signature
handleEvent
  :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s
-- Except for a few exceptional cases, delegate the handling to our own function
handleEvent s ev = execStateT (handleEventInState ev) s >>= continue

-- This function does the actual event handling, inside the StateT monad
handleEventInState
  :: BrickEvent Name CustomEvent -> StateT AppState (EventM Name) ()
handleEventInState (VtyEvent (EvKey (KChar 'e') [MCtrl])) = toggleConsole
handleEventInState (VtyEvent (EvKey (KChar 'p') [MCtrl])) =
  modify $ helpPanelVisible . coerced %~ not
handleEventInState ev = do
  s <- get
  case (s ^. modal, ev) of
    (Just _, VtyEvent (EvKey (KChar 'n') [])) -> dismissModal
    (Just m, VtyEvent (EvKey (KChar 'y') [])) ->
      handleConfirm m >> dismissModal
    _ -> handle (s ^. screen) ev

saveState :: AppState -> IO ()
saveState s = do
  _ <- writeFile mainSettingsFile (encodePretty s)
  _ <- writeFile responseHistoryFile (encodePretty (s ^. responses))
  return ()
