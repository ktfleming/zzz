{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.Events.Handler
  ( updateCurrentTime
  , handleEventInState
  )
where

import           Brick                          ( BrickEvent(..)
                                                , vScrollToEnd
                                                , viewportScroll
                                                )
import           Brick.BChan                    ( BChan )
import           Control.Lens
import           Control.Monad.Indexed          ( ireturn )
import           Control.Monad.Indexed.State    ( IxMonadState
                                                , iget
                                                , imodify
                                                , iput
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Graphics.Vty.Input.Events
import           Language.Haskell.DoNotation
import           Messages.Messages              ( logMessage )
import           Prelude                 hiding ( Monad(return, (>>), (>>=))
                                                , pure
                                                , writeFile
                                                )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent(..) )
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Fields
import           Types.Constants                ( mainSettingsFile
                                                , responseHistoryFile
                                                )
import           Types.Models.RequestDef        ( RequestDefContext(..)
                                                , RequestError(..)
                                                )
import           Types.Models.Screen
import           Types.Models.Screen.Optics     ( lastError )
import           Types.Monads
import           UI.Environments.List           ( showEnvironmentListScreen )
import           UI.Events.Environments
import           UI.Events.Messages             ( handleEventMessages )
import           UI.Events.Projects
import           UI.Events.RequestDefs
import           UI.Modal                       ( dismissModal
                                                , handleConfirm
                                                )
import           UI.RequestDefs.Details         ( refreshResponseList )
import           UI.Search                      ( handleEventSearch
                                                , showSearchScreen
                                                )

-- Every event should also update the currentTime inside the AppState
updateCurrentTime :: (IxMonadState m, IxMonadIO m) => m AnyAppState AnyAppState ()
updateCurrentTime = do
  time <- iliftIO getCurrentTime
  imodify $ currentTime ?~ time

handleCustomEvent :: (IxMonadState m, IxMonadIO m) => CustomEvent -> m (AppState a) (AppState a) ()
handleCustomEvent Save = saveState

-- Note: this is for errors that happen on the background thread that handles sending the
-- request. Errors that happen _prior_ to that (failure to parse URL, unmatched variable, etc)
-- will short-circuit the ExceptT in `sendRequest'` and will not need to use a custom event.
handleCustomEvent (ResponseError (RequestDefContext _ rid) msg) = do
  s <- iget
  logMessage ("Error: " <> (T.pack msg))
  now <- iliftIO getCurrentTime
  case s ^. screen of
    RequestDefDetailsScreen{} -> imodify $ screen . lastError ?~ RequestFailed now (T.pack msg)
    _                         -> return ()
  imodify $ activeRequests . at rid .~ Nothing
  saveState

handleCustomEvent (ResponseSuccess (RequestDefContext _ rid) response) = do
  s <- iget
  let ekey = currentEnvironmentKey s
  logMessage "Received response"
  imodify $ responses . at rid . non Map.empty . at ekey . non S.empty %~ (response <|)
  imodify $ activeRequests . at rid .~ Nothing
  saveState
  case s ^. screen of
    RequestDefDetailsScreen{} -> (imodify $ screen . lastError .~ Nothing) >> refreshResponseList -- Only need to refresh the list if they're looking at it
    _                         -> return ()

handleCustomEvent RefreshResponseList = do
  s <- iget
  case s ^. screen of
    RequestDefDetailsScreen{} -> refreshResponseList
    _                         -> return ()

saveState :: (IxMonadState m, IxMonadIO m) => m (AppState a) (AppState a) ()
saveState = do
  s <- iget
  logMessage "Saving..."
  iliftIO $ writeFile mainSettingsFile (encodePretty s)
  iliftIO $ writeFile responseHistoryFile (encodePretty (s ^. responses))


-- This function does the actual event handling, inside the AppM monad
handleEventInState
  :: (IxMonadState m, IxMonadEvent m, IxMonadIO m)
  => BrickEvent Name CustomEvent
  -> BChan CustomEvent
  -> m AnyAppState AnyAppState ()

handleEventInState (AppEvent customEvent) _ = do
  (AnyAppState s) <- iget
  iput s
  handleCustomEvent customEvent
  submerge

handleEventInState (VtyEvent (EvKey (KChar 'o') [MCtrl])) _ = do
  (AnyAppState s) <- iget
  iput s
  case s ^. screen of
    MessagesScreen -> unstashScreen
    _              -> sm $ do
      stashScreen
      imodify (screen .~ MessagesScreen)
      iliftEvent (vScrollToEnd (viewportScroll MessagesViewport))

handleEventInState (VtyEvent (EvKey (KChar 'p') [MCtrl])) _ = do
  (AnyAppState s) <- iget
  let updated = s & helpPanelVisible . coerced %~ not
  iput $ AnyAppState updated

-- Have to stash the screen before giving the user the chance to select an Environment (either via the
-- global search or the environment list screen) since that necessitates a screen unstash.
handleEventInState (VtyEvent (EvKey (KChar 'f') [MCtrl])) _ = sm $ do
  (AnyAppState s) <- iget
  iput s
  stashScreen
  showSearchScreen

handleEventInState (VtyEvent (EvKey (KChar 'e') [MCtrl])) _ = sm $ do
  (AnyAppState s) <- iget
  iput s
  stashScreen
  showEnvironmentListScreen

handleEventInState (VtyEvent (EvKey key mods)) chan = do
  (AnyAppState s) <- iget
  case (s ^. modal, key) of
    (Just _, KChar 'n') -> dismissModal
    (Just m, KChar 'y') -> do
      handleConfirm m
      sendEvent Save chan
      dismissModal
    (Just _ , _) -> ireturn ()
    (Nothing, _) -> case s ^. screen of
      ProjectAddScreen{}        -> handleEventProjectAdd key mods chan |$| s
      ProjectEditScreen{}       -> handleEventProjectEdit key mods chan |$| s
      ProjectListScreen{}       -> handleEventProjectList key mods chan |$| s
      ProjectDetailsScreen{}    -> handleEventProjectDetails key mods chan |$| s
      RequestDefDetailsScreen{} -> handleEventRequestDetails key mods chan |$| s
      RequestDefEditScreen{}    -> handleEventRequestEdit key mods chan |$| s
      RequestDefAddScreen{}     -> handleEventRequestAdd key mods chan |$| s
      EnvironmentListScreen{}   -> handleEventEnvironmentList key mods chan |$| s
      EnvironmentEditScreen{}   -> handleEventEnvironmentEdit key mods chan |$| s
      EnvironmentAddScreen{}    -> handleEventEnvironmentAdd key mods chan |$| s
      SearchScreen{}            -> handleEventSearch key mods chan |$| s
      MessagesScreen            -> handleEventMessages key mods |$| s
      HelpScreen                -> ireturn ()
handleEventInState _ _ = ireturn ()
