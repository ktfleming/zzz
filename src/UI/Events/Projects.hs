{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Events.Projects where

import Brick.BChan (BChan)
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
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
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectAddTag ->
  m AnyAppState
handleEventProjectAdd key mods chan =
  let doAdd s = do
        pid <- liftIO $ ProjectId <$> nextRandom
        saveAfter chan . pure . wrap . showProjectListScreen . finishAddingProject pid $ s
   in case (key, mods) of
        (KChar 's', [MCtrl]) -> ifValid doAdd
        (KEsc, []) -> pure . wrap . showProjectListScreen
        _ -> pure . wrap <=< updateBrickForm key

handleEventProjectEdit ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectEditTag ->
  m AnyAppState
handleEventProjectEdit key mods chan s =
  let c = s ^. screen ^. context
      doEdit = saveAfter chan . pure . wrap . showProjectDetails c . finishEditingProject
   in case (key, mods) of
        (KChar 's', [MCtrl]) -> ifValid doEdit
        (KEsc, []) -> pure . wrap . showProjectDetails c
        _ -> pure . wrap <=< updateBrickForm key
        $ s

handleEventProjectDetails ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectDetailsTag ->
  m AnyAppState
handleEventProjectDetails key mods chan s =
  let c = s ^. screen ^. context
   in case (key, mods) of
        (KChar 'e', [MCtrl]) -> pure . wrap . showEditProjectScreen c
        (KChar 'a', [MCtrl]) -> pure . wrap . showAddRequestDefScreen c
        (KChar 'd', [MCtrl]) -> pure . wrap . (modal ?~ DeleteProjectModal c)
        (KEsc, []) -> pure . wrap . showProjectListScreen
        _ -> handleEventSearch key mods chan
        $ s

handleEventProjectList ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'ProjectListTag ->
  m AnyAppState
handleEventProjectList key mods chan =
  case (key, mods) of
    (KChar 'a', [MCtrl]) -> pure . wrap . showProjectAddScreen
    _ -> handleEventSearch key mods chan
