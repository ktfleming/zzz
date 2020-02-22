{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Events.Handler
  ( updateCurrentTime,
    handleEvent,
  )
where

import Brick
import Brick.BChan (BChan)
import Brick.Forms (handleFormEvent)
import qualified Config
import Control.Lens
import Control.Monad.Reader
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as HashSet
import qualified Data.Sequence as Seq
import Data.Singletons (withSingI)
import qualified Data.Text as T
import Data.Time
import Graphics.Vty.Input.Events
import Messages.Messages (logMessage)
import Request.Request (sendRequest)
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Brick.Name (Name (..))
import Types.Classes.Fields
import Types.Constants
  ( mainSettingsFile,
    responseHistoryFile,
  )
import Types.Modal
import Types.Models.RequestDef
  ( RequestDefContext (..),
    RequestError (..),
  )
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import Types.Time
import UI.Environments.List (showEnvironmentListScreen)
import UI.Events.Environments
import UI.Events.Keys (matchKey)
import UI.Events.Projects
import UI.Events.RequestDefs
import UI.Form
import UI.Modal
  ( dismissModal,
    handleConfirm,
  )
import UI.RequestDefs.Details (refreshResponseList)
import UI.Search.Common
import UI.Search.SearchScreen
import Prelude hiding (writeFile)

-- Every event should also update the currentTime inside the AppState
updateCurrentTime :: MonadIO m => AnyAppState -> m AnyAppState
updateCurrentTime s = do
  time <- liftIO getCurrentTime
  pure $ s & currentTime .~ time

handleCustomEvent :: (MonadReader Config.AppConfig m, MonadIO m) => BChan CustomEvent -> CustomEvent -> AppState a -> m (AppState a)
handleCustomEvent _ Save s = saveState s
-- Note: this is for errors that happen on the background thread that handles sending the
-- request. Errors that happen _prior_ to that (failure to parse URL, unmatched variable, etc)
-- will short-circuit the ExceptT in `sendRequest'` and will not need to use a custom event.
handleCustomEvent _ (ResponseError (RequestDefContext _ rid) msg) s =
  -- Building a monadic pipeline via Kleisli composition that the initial state will be sent through.
  -- The steps are...
  let -- If the current screen is the RequestDefDetailsScreen, need to update `lastError`
      -- so the user will see the error message
      updateError :: (MonadReader Config.AppConfig m, MonadIO m) => AppState a -> m (AppState a)
      updateError s' = case s' ^. screen of
        RequestDefDetailsScreen {} -> do
          now <- liftIO getCurrentTime
          tz <- asks (view Config.timeZone)
          pure $ s' & screen . lastError ?~ RequestFailed (AppTime (utcToZonedTime tz now)) (T.pack msg)
        _ -> pure s'
      -- The request has completed, so clear its handle
      clearRequest :: AppState a -> AppState a
      clearRequest = activeRequests . at rid .~ Nothing
   in do
        logMessage $ "Error: " <> T.pack msg
        saveState <=< (pure . clearRequest) <=< updateError $ s
handleCustomEvent _ (ResponseSuccess (RequestDefContext _ rid) response) s =
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
handleCustomEvent chan (SendRequest c) s =
  -- This check should not be necessary since the SendRequest event is only dispatched
  -- from the RequestDefDetailsScreen, and it should always be the next event processed
  -- (no chance for the user to move to another screen first). Would like to figure out how
  -- to make this check unnecessary.
  case s ^. screen of
    RequestDefDetailsScreen {} ->
      -- Have to clear the local variables after sending the request
      fmap (screen . rdVariables .~ HashSet.empty) (sendRequest c chan s)
    _ -> pure s

saveState :: (MonadReader Config.AppConfig m, MonadIO m) => AppState a -> m (AppState a)
saveState s = do
  logMessage "Saving..."
  liftIO $ writeFile mainSettingsFile (encodePretty s)
  liftIO $ writeFile responseHistoryFile (encodePretty (s ^. responses))
  pure s

-- This function does the actual event handling, inside the AppM monad
handleEvent ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  BChan CustomEvent ->
  AnyAppState ->
  BrickEvent Name CustomEvent ->
  m AnyAppState
handleEvent chan (AnyAppState tag s) (AppEvent customEvent) = AnyAppState tag <$> handleCustomEvent chan customEvent s
handleEvent chan outer@(AnyAppState tag s) (VtyEvent (EvKey key mods)) = do
  km <- asks (view keymap)
  if  | matchKey (km ^. showHelp) key mods ->
        pure . AnyAppState tag . (helpPanelVisible . coerced %~ not) $ s
      | matchKey (km ^. searchAll) key mods ->
        -- Have to stash the screen before giving the user the chance to select an Environment (either via the
        -- global search or the environment list screen) since that necessitates a screen unstash.
        pure . wrap . showSearchScreen . withSingI tag stashScreen $ s
      | matchKey (km ^. showEnvironments) key mods ->
        pure . wrap . showEnvironmentListScreen . withSingI tag stashScreen $ s
      | otherwise -> case (s ^. modal, key) of
        (Just mdl@(VariablePromptModal c (AppForm fs) needsPrompt), _) ->
          -- This modal has custom handling since it's a form. You can submit, cancel, or
          -- type into the text field.
          if  | matchKey (km ^. submit) key mods ->
                fmap (modal .~ nextModal mdl) . handleConfirm mdl chan $ outer
              | matchKey (km ^. back) key mods -> pure $ dismissModal mdl outer
              | otherwise -> do
                updatedForm <- AppForm <$> liftEvent fs (handleFormEvent (VtyEvent (EvKey key [])) fs)
                pure $ outer & modal ?~ VariablePromptModal c updatedForm needsPrompt
        (Just mdl, KChar 'n') -> pure . dismissModal mdl $ outer
        (Just mdl, KChar 'y') -> saveAfter chan . fmap (modal .~ nextModal mdl) . handleConfirm mdl chan $ outer
        (Just _, _) -> pure outer
        (Nothing, _) -> case s ^. screen of
          ProjectAddScreen {} -> handleEventProjectAdd key mods chan s
          ProjectEditScreen {} -> handleEventProjectEdit key mods chan s
          ProjectListScreen {} -> handleEventProjectList key mods chan s
          ProjectDetailsScreen {} -> handleEventProjectDetails key mods chan s
          RequestDefDetailsScreen {} -> handleEventRequestDetails key mods chan s
          RequestDefEditScreen {} -> handleEventRequestEdit key mods chan s
          RequestDefAddScreen {} -> handleEventRequestAdd key mods chan s
          EnvironmentListScreen {} -> handleEventEnvironmentList key mods chan s
          EnvironmentEditScreen {} -> handleEventEnvironmentEdit key mods chan s
          EnvironmentAddScreen {} -> handleEventEnvironmentAdd key mods chan s
          SearchScreen {} -> handleEventSearch key mods chan s
          HelpScreen -> pure outer
handleEvent _ s _ = pure s
