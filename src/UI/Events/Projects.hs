module UI.Events.Projects where

import           Brick                          ( BrickEvent(VtyEvent) )
import           Brick.Widgets.List             ( listSelectedElement )
import           Control.Lens
import           Graphics.Vty.Input.Events
import           Types.Aliases                  ( EventHandlerFunction )
import           Types.AppState
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectListItem(..) )
import           Types.Models.RequestDef        ( RequestDefListItem(..) )
import           Types.Models.Screen
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

import           Control.Monad.Trans.State      ( get
                                                , modify
                                                )
import           Types.Classes.HasId            ( model )

handleEventProjectAdd :: EventHandlerFunction
handleEventProjectAdd ev@(VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    ProjectAddScreen form -> case key of
      KEnter -> finishAddingProject form
      KEsc   -> showProjectListScreen
      _      -> updateProjectAddForm form ev
    _ -> return ()

handleEventProjectAdd _ = return ()

handleEventProjectEdit :: EventHandlerFunction
handleEventProjectEdit ev@(VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    ProjectEditScreen c form -> case key of
      KEnter -> finishEditingProject c (model s c) form >> showProjectDetails c
      KEsc   -> showProjectDetails c
      _      -> updateEditProjectForm form ev
    _ -> return ()

handleEventProjectEdit _ = return ()

handleEventProjectDetails :: EventHandlerFunction
handleEventProjectDetails (VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    ProjectDetailsScreen c list -> case key of
      KEnter -> case listSelectedElement list of
        Just (_, RequestDefListItem reqContext _) ->
          showRequestDefDetails reqContext
        Nothing -> return ()
      KChar 'e' -> showEditProjectScreen c
      KChar 'a' -> showAddRequestDefScreen c
      KChar 'd' -> modify $ modal ?~ DeleteProjectModal c
      KLeft     -> showProjectListScreen
      _         -> updateProjectDetailsList list key
    _ -> return ()

handleEventProjectDetails _ = return ()

handleEventProjectList :: EventHandlerFunction
handleEventProjectList (VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    ProjectListScreen list -> case key of
      KEnter -> case listSelectedElement list of
        Just (_, ProjectListItem context _) -> showProjectDetails context
        Nothing                             -> return ()
      KChar 'a' -> showProjectAddScreen
      _         -> updateProjectList list key
    _ -> return ()

handleEventProjectList _ = return ()
