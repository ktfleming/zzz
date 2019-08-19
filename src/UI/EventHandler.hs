{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
import           Control.Monad.Indexed          ( ireturn
                                                , (>>>=)
                                                )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , iput
                                                , runIxStateT
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import           Graphics.Vty.Input.Events
import           Types.AppState
import           Types.Constants                ( mainSettingsFile
                                                , responseHistoryFile
                                                )
import           Types.Models.Screen
import           UI.Console                     ( toggleConsole )
import           UI.Events.Projects
import           UI.Events.RequestDefs
import           UI.Modal                       ( dismissModal
                                                , handleConfirm
                                                )
import           Utils.IxState                  ( (>>>)
                                                , (|$|)
                                                )

-- This is the function that's provided to Brick's `App` and must have this exact signature
-- (note AnyAppState instead of AppState, since the input and output state must have the same type,
-- we can't use AppState which is parameterized by a ScreenTag)
handleEvent :: AnyAppState -> BrickEvent Name CustomEvent -> EventM Name (Next AnyAppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) = liftIO (saveState s) >> continue s
-- Except for a few exceptional cases, delegate the handling to our own function

handleEvent s ev = (runIxStateT $ handleEventInState ev) s >>= (continue . snd)

--handleEvent s ev = execStateT (handleEventInState ev) s >>= continue

-- Start:  Any    Any
-- after iput s', we have Any Tagged
-- Next:   Any    Tagged
-- Finish: Tagged Any

-- This function does the actual event handling, inside the IxStateT monad
handleEventInState
  :: BrickEvent Name CustomEvent -> IxStateT (EventM Name) AnyAppState AnyAppState ()
handleEventInState (VtyEvent (EvKey (KChar 'e') [MCtrl])) = toggleConsole
handleEventInState (VtyEvent (EvKey (KChar 'p') [MCtrl])) = iget >>>= \(AnyAppState s) ->
  let updated = s & helpPanelVisible . coerced %~ not in iput $ AnyAppState updated

handleEventInState (VtyEvent (EvKey key [])) = iget >>>= \(AnyAppState s) ->
  case (s ^. modal, key) of
    (Just _ , KChar 'n') -> dismissModal
    (Just m , KChar 'y') -> handleConfirm m >>> dismissModal
    (Just _ , _        ) -> ireturn ()
    (Nothing, _        ) -> case s ^. screen of
      ProjectAddScreen{}        -> handleEventProjectAdd key |$| s
      ProjectEditScreen{}       -> handleEventProjectEdit key |$| s
      ProjectListScreen{}       -> handleEventProjectList key |$| s
      ProjectDetailsScreen{}    -> handleEventProjectDetails key |$| s
      RequestDefDetailsScreen{} -> handleEventRequestDetails key |$| s
      RequestDefEditScreen{}    -> handleEventRequestEdit key |$| s
      RequestDefAddScreen{}     -> handleEventRequestAdd key |$| s
      HelpScreen                -> ireturn ()
handleEventInState _ = ireturn ()

saveState :: AnyAppState -> IO ()
saveState (AnyAppState s) = do
  _ <- writeFile mainSettingsFile (encodePretty s)
  _ <- writeFile responseHistoryFile (encodePretty (s ^. responses))
  return ()
