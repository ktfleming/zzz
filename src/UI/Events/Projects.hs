{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Events.Projects where

import Brick.BChan (BChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import Graphics.Vty.Input.Events
import Language.Haskell.DoNotation
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (Save))
import Types.Classes.Fields
import Types.Modal (Modal (..))
import Types.Models.Project (ProjectListItem (..))
import Types.Models.RequestDef (RequestDefListItem (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
  ( ifValid,
    updateBrickForm,
    updateBrickList,
  )
import Types.Monads
import UI.List (AppList (..))
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
import UI.RequestDefs.Details (showRequestDefDetails)
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

-- Since each branch of the case expression can lead to a different phantom type
-- parameterizing the state, we can only say that the ultimate output type will be
-- `AnyAppState` (as expected by `handleEventInState`, which calls all of these functions),
-- and we have to apply `submerge` on every branch to ensure the output type is `AnyAppState`.
handleEventProjectAdd ::
  (IxMonadState m, IxMonadIO m, IxMonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'ProjectAddTag) AnyAppState ()
handleEventProjectAdd key mods chan = do
  s <- iget
  case (key, mods) of
    (KChar 's', [MCtrl]) ->
      ifValid $ sm $ do
        finishAddingProject
        sendEvent Save chan
        showProjectListScreen
    (KEsc, []) -> sm showProjectListScreen
    _ -> sm $ do
      extractScreen
      updateBrickForm key
      wrapScreen s

handleEventProjectEdit ::
  (IxMonadState m, IxMonadIO m, IxMonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'ProjectEditTag) AnyAppState ()
handleEventProjectEdit key mods chan = do
  s <- iget
  let ProjectEditScreen c _ = s ^. screen
  case (key, mods) of
    (KChar 's', [MCtrl]) ->
      ifValid $ sm $ do
        finishEditingProject
        sendEvent Save chan
        showProjectDetails c
    (KEsc, []) -> sm $ showProjectDetails c
    _ -> sm $ do
      extractScreen
      updateBrickForm key
      wrapScreen s

handleEventProjectDetails ::
  (IxMonadState m, IxMonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent -> -- TODO: unnecessary
  m (AppState 'ProjectDetailsTag) AnyAppState ()
handleEventProjectDetails key mods _ = do
  s <- iget
  let ProjectDetailsScreen c (AppList list) = s ^. screen
  case (key, mods) of
    (KRight, []) -> case listSelectedElement list of
      Just (_, RequestDefListItem reqContext _) -> sm $ showRequestDefDetails reqContext
      Nothing -> submerge
    (KChar 'e', []) -> sm $ showEditProjectScreen c
    (KChar 'a', []) -> sm $ showAddRequestDefScreen c
    (KChar 'd', []) -> sm $ imodify (modal ?~ DeleteProjectModal c)
    (KLeft, []) -> sm showProjectListScreen
    _ -> sm $ do
      extractScreen
      updateBrickList key
      wrapScreen s

handleEventProjectList ::
  (IxMonadState m, IxMonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'ProjectListTag) AnyAppState ()
handleEventProjectList key mods _ = do
  s <- iget
  let ProjectListScreen (AppList list) = s ^. screen
  case (key, mods) of
    (KRight, []) -> case listSelectedElement list of
      Just (_, ProjectListItem context _) -> sm $ showProjectDetails context
      Nothing -> submerge
    (KChar 'a', []) -> sm showProjectAddScreen
    _ -> sm $ do
      extractScreen
      updateBrickList key
      wrapScreen s
