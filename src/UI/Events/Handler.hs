{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Events.Handler
  ( updateCurrentTime,
    handleEvent,
  )
where

import Brick
import Brick.BChan (BChan)
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Singletons (withSingI)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Graphics.Vty.Input.Events
import Messages.Messages (logMessage)
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Brick.Name (Name (..))
import Types.Classes.Fields
import Types.Constants
  ( mainSettingsFile,
    responseHistoryFile,
  )
import Types.Models.RequestDef
  ( RequestDefContext (..),
    RequestError (..),
  )
import Types.Models.Screen
import Types.Models.Screen.Optics (lastError)
import Types.Monads
import UI.Environments.List (showEnvironmentListScreen)
import UI.Events.Environments
import UI.Events.Projects
import UI.Events.RequestDefs
import UI.Modal
  ( dismissModal,
    handleConfirm,
  )
import UI.RequestDefs.Details (refreshResponseList)
import UI.Search
  ( handleEventSearch,
    showSearchScreen,
  )
import Prelude hiding (writeFile)

-- Every event should also update the currentTime inside the AppState
updateCurrentTime :: MonadIO m => AnyAppState -> m AnyAppState
updateCurrentTime s = do
  time <- liftIO getCurrentTime
  pure $ s & currentTime .~ time

handleCustomEvent :: MonadIO m => CustomEvent -> AppState a -> m (AppState a)
handleCustomEvent Save s = saveState s
-- Note: this is for errors that happen on the background thread that handles sending the
-- request. Errors that happen _prior_ to that (failure to parse URL, unmatched variable, etc)
-- will short-circuit the ExceptT in `sendRequest'` and will not need to use a custom event.
handleCustomEvent (ResponseError (RequestDefContext _ rid) msg) s =
  -- Building a monadic pipeline via Kleisli composition that the initial state will be sent through.
  -- The steps are...
  let -- If the current screen is the RequestDefDetailsScreen, need to update `lastError`
      -- so the user will see the error message
      updateError :: MonadIO m => AppState a -> m (AppState a)
      updateError s' = case s' ^. screen of
        RequestDefDetailsScreen {} -> do
          now <- liftIO getCurrentTime
          pure $ s' & screen . lastError ?~ RequestFailed now (T.pack msg)
        _ -> pure s'
      -- The request has completed, so clear its handle
      clearRequest :: AppState a -> AppState a
      clearRequest = activeRequests . at rid .~ Nothing
   in do
        logMessage $ "Error: " <> T.pack msg
        saveState <=< (pure . clearRequest) <=< updateError $ s
handleCustomEvent (ResponseSuccess (RequestDefContext _ rid) response) s =
  let ekey = currentEnvironmentKey s
      -- Append the response to the list of responses for this RequestDef, and clear the request handler
      updateState :: AppState a -> AppState a
      updateState = (responses . at rid . non Map.empty . at ekey . non Seq.empty %~ (response <|)) . (activeRequests . at rid .~ Nothing)
      -- If the RequestDefDetailsScreen is active, clear the last error (if there is one)
      -- and refresh the response list
      updateDetails :: AppState a -> AppState a
      updateDetails s' = case s' ^. screen of
        RequestDefDetailsScreen {} -> (screen . lastError .~ Nothing) . refreshResponseList $ s'
        _ -> s'
   in do
        logMessage "Received response"
        saveState <=< (pure . updateDetails) <=< (pure . updateState) $ s

saveState :: MonadIO m => AppState a -> m (AppState a)
saveState s = do
  logMessage "Saving..."
  liftIO $ writeFile mainSettingsFile (encodePretty s)
  liftIO $ writeFile responseHistoryFile (encodePretty (s ^. responses))
  pure s

-- This function does the actual event handling, inside the AppM monad
handleEvent ::
  MonadEvent m =>
  BChan CustomEvent ->
  AnyAppState ->
  BrickEvent Name CustomEvent ->
  m AnyAppState
handleEvent _ (AnyAppState tag s) (AppEvent customEvent) = AnyAppState tag <$> handleCustomEvent customEvent s
handleEvent _ (AnyAppState tag s) (VtyEvent (EvKey (KChar 'p') [MCtrl])) =
  pure . AnyAppState tag . (helpPanelVisible . coerced %~ not) $ s
-- Have to stash the screen before giving the user the chance to select an Environment (either via the
-- global search or the environment list screen) since that necessitates a screen unstash.
handleEvent _ (AnyAppState tag s) (VtyEvent (EvKey (KChar 'f') [MCtrl])) =
  pure . wrap . showSearchScreen . withSingI tag stashScreen $ s
handleEvent _ (AnyAppState tag s) (VtyEvent (EvKey (KChar 'e') [MCtrl])) =
  pure . wrap . showEnvironmentListScreen . withSingI tag stashScreen $ s
handleEvent chan outer@(AnyAppState _ s) (VtyEvent (EvKey key mods)) =
  case (s ^. modal, key) of
    (Just _, KChar 'n') -> pure . dismissModal $ outer
    (Just m, KChar 'y') -> saveAfter chan . pure . dismissModal . handleConfirm m $ outer
    (Just _, _) -> pure outer
    (Nothing, _) -> case s ^. screen of
      ProjectAddScreen {} -> handleEventProjectAdd key mods chan s
      ProjectEditScreen {} -> handleEventProjectEdit key mods chan s
      ProjectListScreen {} -> handleEventProjectList key mods chan s
      ProjectDetailsScreen {} -> handleEventProjectDetails key mods s
      RequestDefDetailsScreen {} -> handleEventRequestDetails key mods chan s
      RequestDefEditScreen {} -> handleEventRequestEdit key mods chan s
      RequestDefAddScreen {} -> handleEventRequestAdd key mods chan s
      EnvironmentListScreen {} -> handleEventEnvironmentList key mods chan s
      EnvironmentEditScreen {} -> handleEventEnvironmentEdit key mods chan s
      EnvironmentAddScreen {} -> handleEventEnvironmentAdd key mods chan s
      SearchScreen {} -> handleEventSearch key mods chan s
      HelpScreen -> pure outer
handleEvent _ s _ = pure s
