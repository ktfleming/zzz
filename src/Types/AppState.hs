{-# LANGUAGE ExistentialQuantification #-}

module Types.AppState where

import Brick.Forms
import Types.CustomEvent
import Types.Name
import Types.Project
import Types.Screen
import UI.Projects.Add

data AppState = forall a. AppState { _activeScreen :: Screen
                                   , _allProjects :: [Project]
                                   , _activeForm :: Maybe (Form a CustomEvent Name) -- The form to send events to, if one is currently active
                                   }
