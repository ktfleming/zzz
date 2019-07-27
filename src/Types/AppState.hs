{-# LANGUAGE ExistentialQuantification #-}

module Types.AppState where

import Brick.Forms
import Classes.FormState
import Types.CustomEvent
import Types.Name
import Types.Project
import Types.Screen
import UI.Projects.Add

-- The `x` here is the form's state; it's not exposed in the `ActiveForm` type since
-- `ActiveForm` is used generically in `handleEvent` to send events to the form, regardless
-- of which form it is.
data ActiveForm = forall x. FormState x => ActiveForm (Maybe (Form x CustomEvent Name))

handleSubmit :: forall x. FormState x => Form x CustomEvent Name -> IO ()
handleSubmit form = submitValid $ formState form

data AppState = AppState { _activeScreen :: Screen
                                   , _allProjects :: [Project]
                                   , _activeForm :: ActiveForm -- The form to send events to, if one is currently active
                                   }
