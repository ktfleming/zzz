{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
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
import           Brick.BChan                    ( BChan )
import           Control.Lens
import           Control.Monad.Indexed          ( ireturn
                                                , (>>>=)
                                                )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , iput
                                                , runIxStateT
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import           Data.String                    ( fromString )
import           Graphics.Vty.Input.Events
import           Language.Haskell.DoNotation
import           Messages.Messages              ( logMessage )
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                , writeFile
                                                )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent(..) )
import           Types.Brick.Name               ( Name(..) )
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
import           Utils.IxState                  ( save
                                                , submerge
                                                , (>>>)
                                                , (|$|)
                                                )

-- This is the function that's provided to Brick's `App` and must have this exact signature
-- (note AnyAppState instead of AppState, since the input and output state must have the same type,
-- we can't use AppState which is parameterized by a ScreenTag)
handleEvent
  :: BChan CustomEvent
  -> AnyAppState
  -> BrickEvent Name CustomEvent
  -> EventM Name (Next AnyAppState)
handleEvent _ s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately

-- Otherwise, delegate the handling to our own function
handleEvent chan s ev = (runIxStateT $ handleEventInState ev chan) s >>= (continue . snd)

-- This function does the actual event handling, inside the IxStateT monad
handleEventInState
  :: BrickEvent Name CustomEvent
  -> BChan CustomEvent
  -> IxStateT (EventM Name) AnyAppState AnyAppState ()

handleEventInState (AppEvent Save) _ =
  iget >>>= \(AnyAppState s) -> iput s >>> saveState >>> submerge

handleEventInState (VtyEvent (EvKey (KChar 'e') [MCtrl])) _ = toggleConsole
handleEventInState (VtyEvent (EvKey (KChar 'p') [MCtrl])) _ = iget >>>= \(AnyAppState s) ->
  let updated = s & helpPanelVisible . coerced %~ not in iput $ AnyAppState updated

handleEventInState (VtyEvent (EvKey key mods)) chan = iget >>>= \(AnyAppState s) ->
  case (s ^. modal, key) of
    (Just _ , KChar 'n') -> dismissModal
    (Just m , KChar 'y') -> handleConfirm m >>> save chan >>> dismissModal
    (Just _ , _        ) -> ireturn ()
    (Nothing, _        ) -> case s ^. screen of
      ProjectAddScreen{}        -> handleEventProjectAdd key mods chan |$| s
      ProjectEditScreen{}       -> handleEventProjectEdit key mods chan |$| s
      ProjectListScreen{}       -> handleEventProjectList key mods chan |$| s
      ProjectDetailsScreen{}    -> handleEventProjectDetails key mods chan |$| s
      RequestDefDetailsScreen{} -> handleEventRequestDetails key mods chan |$| s
      RequestDefEditScreen{}    -> handleEventRequestEdit key mods chan |$| s
      RequestDefAddScreen{}     -> handleEventRequestAdd key mods chan |$| s
      HelpScreen                -> ireturn ()
handleEventInState _ _ = ireturn ()

saveState :: MonadIO m => IxStateT m (AppState a) (AppState a) ()
saveState = do
  s <- iget
  _ <- logMessage "Saving..."
  _ <- (ilift . liftIO) $ writeFile mainSettingsFile (encodePretty s)
  (ilift . liftIO) $ writeFile responseHistoryFile (encodePretty (s ^. responses))
