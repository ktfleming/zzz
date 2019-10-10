{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax    #-}

module UI.Events.Projects where
import           Brick.BChan                    ( BChan )
import           Brick.Types                    ( EventM )
import           Brick.Widgets.List             ( listSelectedElement )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Graphics.Vty.Input.Events
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(return, (>>), (>>=))
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name               ( Name )
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectListItem(..) )
import           Types.Models.RequestDef        ( RequestDefListItem(..) )
import           Types.Models.Screen
import           Types.Models.Screen.Optics         ( updateBrickForm
                                                , updateBrickList
                                                )
import           UI.Projects.Add                ( finishAddingProject
                                                , showProjectAddScreen
                                                )
import           UI.Projects.Details            ( showProjectDetails )
import           UI.Projects.Edit               ( finishEditingProject
                                                , showEditProjectScreen
                                                )
import           UI.Projects.List               ( showProjectListScreen )
import           UI.RequestDefs.Add             ( showAddRequestDefScreen )
import           UI.RequestDefs.Details         ( showRequestDefDetails )
import           Utils.IxState                  ( extractScreen
                                                , save
                                                , submerge
                                                , wrapScreen
                                                , (>>>)
                                                )

-- Since each branch of the case expression can lead to a different phantom type
-- parameterizing the state, we can only say that the ultimate output type will be
-- `AnyAppState` (as expected by `handleEventInState`, which calls all of these functions),
-- and we have to apply `submerge` on every branch to ensure the output type is `AnyAppState`.
handleEventProjectAdd
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'ProjectAddTag) AnyAppState ()
handleEventProjectAdd key mods chan = do
  s <- iget
  case (key, mods) of
    (KChar 's', [MCtrl]) -> do
      finishAddingProject
      save chan
      showProjectListScreen
      submerge
    (KEsc, []) -> showProjectListScreen >>> submerge
    _ -> do
      extractScreen
      updateBrickForm key
      wrapScreen s
      submerge

handleEventProjectEdit
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'ProjectEditTag) AnyAppState ()
handleEventProjectEdit key mods chan = do
  s <- iget
  let ProjectEditScreen c _ = s ^. screen
  case (key, mods) of
    (KChar 's', [MCtrl]) -> do
      finishEditingProject
      save chan
      showProjectDetails c
      submerge
    (KEsc, []) -> showProjectDetails c >>> submerge
    _  -> do
      extractScreen
      updateBrickForm key
      wrapScreen s
      submerge

handleEventProjectDetails
  :: Key
  -> [Modifier]
  -> BChan CustomEvent -- TODO: unnecessary
  -> IxStateT (EventM Name) (AppState 'ProjectDetailsTag) AnyAppState ()
handleEventProjectDetails key mods _ = do
  s <- iget
  let ProjectDetailsScreen c list = s ^. screen
  case (key, mods) of
    (KRight, []) -> case listSelectedElement list of
      Just (_, RequestDefListItem reqContext _) ->
        showRequestDefDetails reqContext >>> submerge
      Nothing -> submerge
    (KChar 'e', []) -> showEditProjectScreen c >>> submerge
    (KChar 'a', []) -> showAddRequestDefScreen c >>> submerge
    (KChar 'd', []) -> imodify (modal ?~ DeleteProjectModal c) >>> submerge
    (KLeft    , []) -> showProjectListScreen >>> submerge
    _ -> do
      extractScreen
      updateBrickList key
      wrapScreen s
      submerge

handleEventProjectList
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'ProjectListTag) AnyAppState ()
handleEventProjectList key mods _ = do
  s <- iget
  let ProjectListScreen list = s ^. screen
  case (key, mods) of
    (KRight, []) -> case listSelectedElement list of
      Just (_, ProjectListItem context _) -> showProjectDetails context >>> submerge
      Nothing -> submerge
    (KChar 'a', []) -> showProjectAddScreen >>> submerge
    _ -> do
      extractScreen
      updateBrickList key
      wrapScreen s
      submerge
