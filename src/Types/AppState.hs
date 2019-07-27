{-# LANGUAGE ExistentialQuantification #-}

module Types.AppState where

import Brick.Forms
import Types.CustomEvent
import Types.Name
import Types.Project
import Types.Screen
import UI.Projects.Add

-- The `x` here is the form's state; it's not exposed in the `ActiveForm` type since
-- `ActiveForm` is used generically in `handleEvent` to send events to the form, regardless
-- of which form it is.
data ActiveForm = forall x. ActiveForm (Maybe (Form x CustomEvent Name))

data AppState = AppState { _activeScreen :: Screen
                                   , _allProjects :: [Project]
                                   , _activeForm :: ActiveForm -- The form to send events to, if one is currently active
                                   }
