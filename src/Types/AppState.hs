module Types.AppState where

import Types.Project

data AppState = AppState { allProjects :: [Project] }
