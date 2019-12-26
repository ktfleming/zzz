{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Events.Environments where

import Brick.BChan (BChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Singletons
import Data.UUID.V4 (nextRandom)
import Graphics.Vty.Input.Events
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Classes.Fields
import Types.Modal
import Types.Models.Environment
  ( EnvironmentContext,
    EnvironmentListItem (..),
  )
import Types.Models.Id (EnvironmentId (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import UI.Environments.Add
  ( finishAddingEnvironment,
    showEnvironmentAddScreen,
  )
import UI.Environments.Edit
  ( finishEditingEnvironment,
    showEnvironmentEditScreen,
  )
import UI.Environments.List (showEnvironmentListScreen)
import UI.List (AppList (..))
import UI.RequestDefs.Details (refreshResponseList)

handleEventEnvironmentAdd ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'EnvironmentAddTag ->
  m AnyAppState
handleEventEnvironmentAdd key mods chan =
  let doAdd s = do
        eid <- liftIO $ EnvironmentId <$> nextRandom
        saveAfter chan $ pure . wrap . showEnvironmentListScreen . finishAddingEnvironment eid $ s
   in case (key, mods) of
        (KChar 's', [MCtrl]) -> ifValid doAdd
        (KEsc, []) -> pure . wrap . showEnvironmentListScreen
        _ -> pure . wrap <=< updateBrickForm key

-- Sets the provided environment (or no environment, in the case of Nothing) as the active one.
-- This necessitates a reset of the currently displayed error, if the active screen happens to be
-- the RequestDef details screen, and the environment is actually changing.
selectEnvironment ::
  (MonadIO m, SingI a) =>
  Maybe EnvironmentContext ->
  BChan CustomEvent ->
  AppState a ->
  m AnyAppState
selectEnvironment c chan s = do
  let currentEnv = s ^. environmentContext
      isChanging = currentEnv /= c
      action = if isChanging then pure . refreshIfNecessary else pure
  saveAfter chan . action . unstashScreen . (environmentContext .~ c) $ s

-- Selecting a new environment necessitates a refresh of the screen if the current screen happens to be the
-- request def details screen
refreshIfNecessary :: AnyAppState -> AnyAppState
refreshIfNecessary outer@(AnyAppState _ s) =
  case s ^. screen of
    RequestDefDetailsScreen {} ->
      wrap $ refreshResponseList (s & screen . lastError .~ Nothing)
    _ -> outer

handleEventEnvironmentList ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'EnvironmentListTag ->
  m AnyAppState
handleEventEnvironmentList key mods chan s =
  let AppList list = s ^. screen ^. listLens
      selectedEnv = snd <$> listSelectedElement list
   in case (key, mods) of
        (KEnter, []) -> case selectedEnv of
          Just (AnEnvironment c _) -> selectEnvironment (Just c) chan
          Just NoEnvironment -> selectEnvironment Nothing chan
          Nothing -> pure . wrap
        (KChar 'd', []) -> case selectedEnv of
          Just (AnEnvironment c _) -> pure . wrap . (modal ?~ DeleteEnvironmentModal c)
          _ -> pure . wrap
        (KChar 'e', []) -> case selectedEnv of
          Just (AnEnvironment c _) -> pure . wrap . showEnvironmentEditScreen c
          _ -> pure . wrap
        (KChar 'a', []) -> pure . wrap . showEnvironmentAddScreen
        _ -> pure . wrap <=< updateBrickList key
        $ s

handleEventEnvironmentEdit ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'EnvironmentEditTag ->
  m AnyAppState
handleEventEnvironmentEdit key mods chan s =
  let doEdit = saveAfter chan . pure . wrap . showEnvironmentListScreen . finishEditingEnvironment
   in case (key, mods) of
        (KChar 's', [MCtrl]) -> ifValid doEdit
        (KEsc, []) -> pure . wrap . showEnvironmentListScreen
        _ -> pure . wrap <=< updateBrickForm key
        $ s
