{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module UI.Projects.Add where

import           Brick                          ( Widget
                                                , str
                                                , (<+>)
                                                )
import           Brick.Forms
import qualified Data.Text                     as T
import           Lens.Micro.Platform
import           Types.AppState
import           Types.CustomEvent
import           Types.Name
import           Types.Project

data ProjectAddState = ProjectAddState { _projectName :: T.Text }

makeLenses ''ProjectAddState

instance FormState ProjectAddState where
  submitValid :: AppState -> ProjectAddState -> AppState
  submitValid appState newProjectState =
    let newProject = Project $ _projectName newProjectState
    in  (allProjects <>~ [newProject]) appState

mkForm :: ProjectAddState -> Form ProjectAddState CustomEvent Name
mkForm = newForm
  [ (str "Project Name: " <+>)
      @@= editTextField UI.Projects.Add.projectName ProjectNameField (Just 1)
  ]
