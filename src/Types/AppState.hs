module Types.AppState where

import Types.Project
import Types.Screen

data AppState = AppState { _activeScreen :: Screen
                         , _allProjects :: [Project]
                         }
