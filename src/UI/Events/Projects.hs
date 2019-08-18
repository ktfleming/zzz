{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Events.Projects where

import           Brick                          ( BrickEvent(VtyEvent) )
import           Brick.Widgets.List             ( listSelectedElement )
import           Control.Lens
import           Graphics.Vty.Input.Events
import           Types.AppState
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectListItem(..) )
import           Types.Models.RequestDef        ( RequestDefListItem(..) )
import           UI.Projects.Add                ( finishAddingProject
                                                , showProjectAddScreen
                                                , updateProjectAddForm
                                                )
import           UI.Projects.Details            ( showProjectDetails
                                                , updateProjectDetailsList
                                                )
import           UI.Projects.Edit               ( finishEditingProject
                                                , showEditProjectScreen
                                                , updateEditProjectForm
                                                )
import           UI.Projects.List               ( showProjectListScreen
                                                , updateProjectList
                                                )
import           UI.RequestDefs.Add             ( showAddRequestDefScreen )
import           UI.RequestDefs.Details         ( showRequestDefDetails )

import           Brick.Types                    ( EventM )
import           Types.Brick.Name               ( Name )
import           Types.Models.Screen
import           Utils.IxState                  ( submerge
                                                , (>>>)
                                                )

import           Control.Monad.Indexed          ( ireturn
                                                , (>>>=)
                                                )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )

handleEventProjectAdd
  :: Key -> IxStateT (EventM Name) (AppState 'ProjectAddTag) AnyAppState ()
handleEventProjectAdd key = case key of
  KEnter -> submerge finishAddingProject
  KEsc   -> submerge showProjectListScreen
  _      -> submerge $ updateProjectAddForm (VtyEvent (EvKey key []))

handleEventProjectEdit
  :: Key -> IxStateT (EventM Name) (AppState 'ProjectEditTag) AnyAppState ()
handleEventProjectEdit key = iget >>>= \s ->
  let ProjectEditScreen c _ = s ^. screen
  in  case key of
        KEnter -> finishEditingProject >>> submerge (showProjectDetails c)
        KEsc   -> submerge $ showProjectDetails c
        _      -> submerge $ updateEditProjectForm (VtyEvent (EvKey key []))

handleEventProjectDetails
  :: Key -> IxStateT (EventM Name) (AppState 'ProjectDetailsTag) AnyAppState ()
handleEventProjectDetails key = iget >>>= \s ->
  let ProjectDetailsScreen c list = s ^. screen
  in  case key of
        KEnter -> case listSelectedElement list of
          Just (_, RequestDefListItem reqContext _) ->
            submerge $ showRequestDefDetails reqContext
          Nothing -> submerge $ ireturn ()
        KChar 'e' -> submerge $ showEditProjectScreen c
        KChar 'a' -> submerge $ showAddRequestDefScreen c
        KChar 'd' -> submerge $ imodify $ modal ?~ DeleteProjectModal c
        KLeft     -> submerge showProjectListScreen
        _         -> submerge $ updateProjectDetailsList key

handleEventProjectList
  :: Key -> IxStateT (EventM Name) (AppState 'ProjectListTag) AnyAppState ()
handleEventProjectList key = iget >>>= \s ->
  let ProjectListScreen list = s ^. screen
  in  case key of
        KEnter -> case listSelectedElement list of
          Just (_, ProjectListItem context _) ->
            submerge $ showProjectDetails context
          Nothing -> submerge $ ireturn ()
        KChar 'a' -> submerge showProjectAddScreen
        _         -> submerge $ updateProjectList key
