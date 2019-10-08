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
                                                , vScrollToEnd
                                                , viewportScroll
                                                )
import           Brick.BChan                    ( BChan )
import           Control.Lens
import           Control.Monad.Indexed          ( ireturn )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                , iput
                                                , runIxStateT
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
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
import           Types.Constants                ( mainSettingsFile
                                                , responseHistoryFile
                                                )
import           Types.Models.Project           ( requestDefs )
import           Types.Models.RequestDef        ( LastError(..)
                                                , RequestDefContext(..)
                                                , lastError
                                                )
import           Types.Models.Screen
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
import           Utils.IxState                  ( save
                                                , stashScreen
                                                , submerge
                                                , unstashScreen
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

-- Ctrl-C always exits immediately
handleEvent _ s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s

-- Otherwise, delegate the handling to our own function. Note that want to update the currentTime
-- with every event.
handleEvent chan s ev =
  (runIxStateT $ handleEventInState ev chan >>> updateCurrentTime) s >>= (continue . snd)

-- Every event should also update the currentTime inside the AppState
updateCurrentTime :: MonadIO m => IxStateT m AnyAppState AnyAppState ()
updateCurrentTime = do
  (AnyAppState s) <- iget
  iput s
  time <- (ilift . liftIO) getCurrentTime
  imodify $ currentTime ?~ time
  submerge

-- This function does the actual event handling, inside the IxStateT monad
handleEventInState
  :: BrickEvent Name CustomEvent
  -> BChan CustomEvent
  -> IxStateT (EventM Name) AnyAppState AnyAppState ()

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
    _              -> do
      stashScreen
      imodify (screen .~ MessagesScreen)
      ilift (vScrollToEnd (viewportScroll MessagesViewport))
      submerge

handleEventInState (VtyEvent (EvKey (KChar 'p') [MCtrl])) _ = do
  (AnyAppState s) <- iget
  let updated = s & helpPanelVisible . coerced %~ not
  iput $ AnyAppState updated

-- Have to stash the screen before giving the user the chance to select an Environment (either via the
-- global search or the environment list screen) since that necessitates a screen unstash.
handleEventInState (VtyEvent (EvKey (KChar 'f') [MCtrl])) _ = do
  (AnyAppState s) <- iget
  iput s
  stashScreen
  showSearchScreen
  submerge

handleEventInState (VtyEvent (EvKey (KChar 'e') [MCtrl])) _ = do
  (AnyAppState s) <- iget
  iput s
  stashScreen
  showEnvironmentListScreen
  submerge

handleEventInState (VtyEvent (EvKey key mods)) chan = do
  (AnyAppState s) <- iget
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
      EnvironmentListScreen{}   -> handleEventEnvironmentList key mods chan |$| s
      EnvironmentEditScreen{}   -> handleEventEnvironmentEdit key mods chan |$| s
      EnvironmentAddScreen{}    -> handleEventEnvironmentAdd key mods chan |$| s
      SearchScreen{}            -> handleEventSearch key mods chan |$| s
      MessagesScreen            -> handleEventMessages key mods |$| s
      HelpScreen                -> ireturn ()
handleEventInState _ _ = ireturn ()

handleCustomEvent :: CustomEvent -> IxStateT (EventM Name) (AppState a) (AppState a) ()
handleCustomEvent Save = saveState
handleCustomEvent (ResponseError (RequestDefContext pid rid) e) = do
  logMessage ("Error: " <> T.pack e)
  now <- liftIO getCurrentTime
  imodify $ projects . at pid . _Just . requestDefs . at rid . _Just . lastError ?~ LastError now
  imodify $ activeRequests . at rid .~ Nothing
  saveState

handleCustomEvent (ResponseSuccess (RequestDefContext pid rid) response) = do
  s <- iget
  let ekey = currentEnvironmentKey s
  logMessage "Received response"
  imodify $ projects . at pid . _Just . requestDefs . at rid . _Just . lastError .~ Nothing
  imodify $ responses . at rid . non Map.empty . at ekey . non S.empty %~ (response <|)
  imodify $ activeRequests . at rid .~ Nothing
  saveState
  case s ^. screen of
    RequestDefDetailsScreen{} -> refreshResponseList -- Only need to refresh the list if they're looking at it
    _                         -> ireturn ()

handleCustomEvent RefreshResponseList = do
  s <- iget
  case s ^. screen of
    RequestDefDetailsScreen{} -> refreshResponseList
    _                         -> ireturn ()

saveState :: MonadIO m => IxStateT m (AppState a) (AppState a) ()
saveState = do
  s <- iget
  logMessage "Saving..."
  (ilift . liftIO) $ writeFile mainSettingsFile (encodePretty s)
  (ilift . liftIO) $ writeFile responseHistoryFile (encodePretty (s ^. responses))
