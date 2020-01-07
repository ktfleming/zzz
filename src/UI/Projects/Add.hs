{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Add
  ( finishAddingProject,
    showProjectAddScreen,
  )
where

import Brick.Forms
import Control.Lens
import qualified Data.HashMap.Strict as Map
import Types.AppState
import Types.Classes.Fields
import Types.Models.Id (ProjectId (..))
import Types.Models.Project
import Types.Models.Screen
import UI.Form
import UI.Projects.Common (makeProjectForm)

finishAddingProject ::
  ProjectId -> AppState 'ProjectAddTag -> AppState 'ProjectAddTag
finishAddingProject pid s = do
  let ProjectAddScreen (AppForm form) = s ^. screen
  let project = Project {projectName = formState form ^. name, projectRequestDefs = Map.empty}
   in s & projects . at pid ?~ project

showProjectAddScreen :: AppState a -> AppState 'ProjectAddTag
showProjectAddScreen s =
  let fs = ProjectFormState {projectFormStateName = ProjectName ""}
   in s & screen .~ ProjectAddScreen (makeProjectForm fs)
