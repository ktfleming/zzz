module UI.Events.Projects where

import           Brick                          ( BrickEvent(VtyEvent)
                                                , continue
                                                )
import           Brick.Forms                    ( formState
                                                , handleFormEvent
                                                )
import           Brick.Widgets.List             ( handleListEvent
                                                , listSelectedElement
                                                )
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Graphics.Vty.Input.Events
import           Types.Aliases                  ( EventHandlerFunction )
import           Types.AppState
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectListItem(..) )
import           Types.Models.RequestDefinition ( RequestDefinitionListItem(..)
                                                )
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
import           UI.RequestDefinitions.Add      ( showAddRequestDefinitionScreen
                                                )
import           UI.RequestDefinitions.Details  ( showRequestDefinitionDetails )

handleEventProjectAdd :: EventHandlerFunction
handleEventProjectAdd s@AppState { appStateScreen = ProjectAddScreen form } ev@(VtyEvent (EvKey key []))
  = case key of
    KEnter -> liftIO (finishAddingProject s (formState form)) >>= continue
    KEsc   -> continue $ showProjectListScreen s
    _      -> updateProjectAddForm s <$> handleFormEvent ev form >>= continue
handleEventProjectAdd s _ = continue s

handleEventProjectEdit :: EventHandlerFunction
handleEventProjectEdit s@AppState { appStateScreen = ProjectEditScreen c form } ev@(VtyEvent (EvKey key []))
  = case key of
    KEnter ->
      let updatedState = finishEditingProject s c (formState form)
      in  continue $ showProjectDetails updatedState c
    KEsc -> continue $ showProjectDetails s c
    _    -> updateEditProjectForm s <$> handleFormEvent ev form >>= continue
handleEventProjectEdit s _ = continue s

handleEventProjectDetails :: EventHandlerFunction
handleEventProjectDetails s@AppState { appStateScreen = ProjectDetailsScreen c list } (VtyEvent (EvKey key []))
  = case key of
    KEnter -> case listSelectedElement list of
      Just (_, RequestDefinitionListItem reqContext _) ->
        continue $ showRequestDefinitionDetails s reqContext
      Nothing -> continue s
    KChar 'e' -> continue $ showEditProjectScreen s c
    KChar 'a' -> continue $ showAddRequestDefinitionScreen s c
    KChar 'd' -> continue $ (modal ?~ DeleteProjectModal c) s
    KLeft     -> continue $ showProjectListScreen s
    _ ->
      updateProjectDetailsList s
        <$> handleListEvent (EvKey key []) list
        >>= continue
handleEventProjectDetails s _ = continue s


handleEventProjectList :: EventHandlerFunction
handleEventProjectList s@AppState { appStateScreen = ProjectListScreen list } (VtyEvent (EvKey key []))
  = case key of
    KEnter -> case listSelectedElement list of
      Just (_, ProjectListItem context _) ->
        continue $ showProjectDetails s context
      Nothing -> continue s
    KChar 'a' -> continue $ showProjectAddScreen s
    _ ->
      updateProjectList s <$> handleListEvent (EvKey key []) list >>= continue
handleEventProjectList s _ = continue s
