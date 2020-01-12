{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module UI.Events.Projects where

import Brick.BChan (BChan)
import qualified Config
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.UUID.V4 (nextRandom)
import Graphics.Vty.Input.Events
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent)
import Types.Classes.Fields
import Types.Modal (Modal (..))
import Types.Models.Id
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import UI.Events.Keys (matchKey)
import UI.Projects.Add
  ( finishAddingProject,
    showProjectAddScreen,
  )
import UI.Projects.Details (showProjectDetails)
import UI.Projects.Edit
  ( finishEditingProject,
    showEditProjectScreen,
  )
import UI.Projects.List (showProjectListScreen)
import UI.RequestDefs.Add (showAddRequestDefScreen)
import UI.Search.Common

-- Since each branch of the case expression can lead to a different phantom type
-- parameterizing the state, we can only say that the ultimate output type will be
-- `AnyAppState` (as expected by `handleEventInState`, which calls all of these functions),
-- and we have to apply `wrap` on every branch to ensure the output type is `AnyAppState`.
handleEventProjectAdd ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectAddTag ->
  m AnyAppState
handleEventProjectAdd key mods chan s = do
  km <- asks (view keymap)
  let doAdd s' = do
        pid <- liftIO $ ProjectId <$> nextRandom
        saveAfter chan . pure . wrap . showProjectListScreen . finishAddingProject pid $ s'
  if  | matchKey (km ^. save) key mods -> ifValid doAdd s
      | matchKey (km ^. back) key mods -> pure . wrap . showProjectListScreen $ s
      | otherwise -> pure . wrap <=< updateBrickForm key $ s

handleEventProjectEdit ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectEditTag ->
  m AnyAppState
handleEventProjectEdit key mods chan s = do
  km <- asks (view keymap)
  let c = s ^. screen ^. context
      doEdit = saveAfter chan . pure . wrap . showProjectDetails c . finishEditingProject
  if  | matchKey (km ^. save) key mods -> ifValid doEdit s
      | matchKey (km ^. back) key mods -> pure . wrap . showProjectDetails c $ s
      | otherwise -> pure . wrap <=< updateBrickForm key $ s

handleEventProjectDetails ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectDetailsTag ->
  m AnyAppState
handleEventProjectDetails key mods chan s = do
  km <- asks (view keymap)
  let c = s ^. screen ^. context
  if  | matchKey (km ^. edit) key mods -> pure . wrap . showEditProjectScreen c $ s
      | matchKey (km ^. add) key mods -> pure . wrap . showAddRequestDefScreen c $ s
      | matchKey (km ^. delete) key mods -> pure . wrap . (modal ?~ DeleteProjectModal c) $ s
      | matchKey (km ^. back) key mods -> pure . wrap . showProjectListScreen $ s
      | otherwise -> handleEventSearch key mods chan s

handleEventProjectList ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectListTag ->
  m AnyAppState
handleEventProjectList key mods chan s = do
  km <- asks (view keymap)
  if  | matchKey (km ^. add) key mods -> pure . wrap . showProjectAddScreen $ s
      | otherwise -> handleEventSearch key mods chan s
