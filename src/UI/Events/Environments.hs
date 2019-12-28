{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Events.Environments where

import Brick.BChan (BChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.UUID.V4 (nextRandom)
import Graphics.Vty.Input.Events
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Classes.Fields
import Types.Modal
import Types.Models.Id (EnvironmentId (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import Types.Search
import UI.Environments.Add
import UI.Environments.Edit
import UI.Environments.List (showEnvironmentListScreen)
import UI.List
import UI.Search.Common

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

handleEventEnvironmentList ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'EnvironmentListTag ->
  m AnyAppState
handleEventEnvironmentList key mods chan s =
  let AppList list = s ^. screen ^. searchTools ^. appList
      selectedEnv = snd <$> listSelectedElement list
   in case (key, mods) of
        (KChar 'd', [MCtrl]) -> case selectedEnv of
          Just (SelectableResult (AnEnvironmentResult c _)) -> pure . wrap . (modal ?~ DeleteEnvironmentModal c)
          _ -> pure . wrap
        (KChar 'e', [MCtrl]) -> case selectedEnv of
          Just (SelectableResult (AnEnvironmentResult c _)) -> pure . wrap . showEnvironmentEditScreen c
          _ -> pure . wrap
        (KChar 'a', [MCtrl]) -> pure . wrap . showEnvironmentAddScreen
        _ -> handleEventSearch key mods chan
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
