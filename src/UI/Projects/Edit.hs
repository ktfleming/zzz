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
import           Types.Models.RequestDefinition ( name )
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

finishEditingProject
  :: AppState -> ProjectContext -> ProjectFormState -> AppState
finishEditingProject appState context@(ProjectContext pid) editState =
  let base     = model appState context
      newModel = updateProject base editState
  in  appState & (projects . ix pid .~ newModel)

updateProject :: Project -> ProjectFormState -> Project
updateProject base formState = (name .~ (formState ^. name)) base

makeEditProjectForm :: AppState -> ProjectContext -> ZZZForm ProjectFormState
makeEditProjectForm s c =
  let p         = model s c
      editState = ProjectFormState { projectFormStateName = p ^. name }
  in  newForm
        [ (txt "Project Name: " <+>)
            @@= editTextField name ProjectFormNameField (Just 1)
        ]
        editState

showEditProjectScreen :: AppState -> ProjectContext -> AppState
showEditProjectScreen s c =
  s & screen .~ ProjectEditScreen c (makeEditProjectForm s c)

updateEditProjectForm
  :: AppState -> ProjectContext -> ZZZForm ProjectFormState -> AppState
updateEditProjectForm s c f = s & screen .~ ProjectEditScreen c f
