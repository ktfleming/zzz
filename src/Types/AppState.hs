module Types.AppState where

import Brick.Forms
import Types.CustomEvent
import Types.Name
import Types.Project
import Types.Screen
import UI.Projects.Add

data AppState = AppState { _activeScreen :: Screen
                         , _allProjects :: [Project]
                         , _addProjectForm :: Maybe (Form ProjectAddState CustomEvent Name)
                         }
