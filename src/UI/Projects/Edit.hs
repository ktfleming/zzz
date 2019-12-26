{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Projects.Edit
  ( finishEditingProject,
    showEditProjectScreen,
  )
where

import Brick.Forms
import Control.Lens
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.Project
import Types.Models.Screen
import UI.Form
import UI.Projects.Common (makeProjectForm)

finishEditingProject :: AppState 'ProjectEditTag -> AppState 'ProjectEditTag
finishEditingProject s =
  let ProjectEditScreen c@(ProjectContext pid) (AppForm form) = s ^. screen
      base = model s c
      newModel = updateProject (formState form) base
   in s & projects . ix pid .~ newModel

updateProject :: ProjectFormState -> Project -> Project
updateProject form = name .~ (form ^. name)

showEditProjectScreen :: ProjectContext -> AppState a -> AppState 'ProjectEditTag
showEditProjectScreen c s =
  let p = model s c
      fs = ProjectFormState {projectFormStateName = p ^. name}
   in s & screen .~ ProjectEditScreen c (makeProjectForm fs)
