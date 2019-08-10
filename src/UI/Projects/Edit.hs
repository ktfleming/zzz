{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Edit where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , newForm
                                                , (@@=)
                                                )
import           Control.Lens
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.WithID           ( model )
import           Types.Models.Project
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

finishEditingProject
  :: AppState -> ProjectContext -> ProjectFormState -> AppState
finishEditingProject appState context@(ProjectContext pid) editState =
  let base     = model appState context
      newModel = updateProject base editState
  in  appState & (projects . at pid . _Just .~ newModel)

updateProject :: Project -> ProjectFormState -> Project
updateProject base ProjectFormState { _projectFormName } =
  (projectName .~ _projectFormName) base

makeEditProjectForm :: AppState -> ProjectContext -> ZZZForm ProjectFormState
makeEditProjectForm s c =
  let Project { _projectName } = model s c
      editState = ProjectFormState { _projectFormName = _projectName }
  in  newForm
        [ (txt "Project Name: " <+>)
            @@= editTextField projectFormName ProjectFormNameField (Just 1)
        ]
        editState

showEditProjectScreen :: AppState -> ProjectContext -> AppState
showEditProjectScreen s c =
  s & activeScreen .~ ProjectEditScreen c (makeEditProjectForm s c)

updateEditProjectForm
  :: AppState -> ProjectContext -> ZZZForm ProjectFormState -> AppState
updateEditProjectForm s c f = s & activeScreen .~ ProjectEditScreen c f
