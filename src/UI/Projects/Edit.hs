{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Edit where

import           Brick                (txt, (<+>))
import           Brick.Forms          (editTextField, newForm, (@@=))
import           Lens.Micro.Platform  (at, (&), (.~), _Just)
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.WithID (model)
import           Types.Models.Project
import           Types.Models.Screen
import           UI.Form              (ZZZForm)

finishEditingProject :: AppState -> ProjectContext -> ProjectEditState -> AppState
finishEditingProject appState context@(ProjectContext pid) editState =
  let base     = model appState context
      newModel = updateProject base editState
  in  appState & (projects . at pid . _Just .~ newModel)

updateProject :: Project -> ProjectEditState -> Project
updateProject base ProjectEditState { _projectEditName } =
  (projectName .~ _projectEditName) base

makeEditProjectForm :: AppState -> ProjectContext -> ZZZForm ProjectEditState
makeEditProjectForm s c =
  let Project { _projectName } = model s c
      editState = ProjectEditState { _projectEditName = _projectName }
  in  newForm
        [ (txt "Project Name: " <+>)
            @@= editTextField projectEditName ProjectEditNameField (Just 1)
        ]
        editState

showEditProjectScreen :: AppState -> ProjectContext -> AppState
showEditProjectScreen s c =
  s & activeScreen .~ ProjectEditScreen c (makeEditProjectForm s c)

updateEditProjectForm
  :: AppState -> ProjectContext -> ZZZForm ProjectEditState -> AppState
updateEditProjectForm s c f = s & activeScreen .~ ProjectEditScreen c f
