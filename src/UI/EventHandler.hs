{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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

import           Types.Classes.EventHandler     ( handle )

handleEvent
  :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s
handleEvent s (VtyEvent (EvKey (KChar 'e') [MCtrl])) =
  continue $ toggleConsole s
handleEvent s (VtyEvent (EvKey (KChar 'p') [MCtrl])) =
  continue $ s & helpPanelVisible . coerced %~ not

handleEvent s@AppState { appStateModal = Just m } (VtyEvent (EvKey key [])) =
  case key of
    KChar 'n' -> continue $ dismissModal s
    KChar 'y' -> continue $ dismissModal $ handleConfirm s m
    _         -> continue s

handleEvent s ev = handle (s ^. screen) s ev

saveState :: AppState -> IO ()
saveState s = do
  _ <- writeFile mainSettingsFile (encodePretty s)
  _ <- writeFile responseHistoryFile (encodePretty (s ^. responses))
  return ()
