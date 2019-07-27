{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types.AppState where

import Brick.Forms
import Lens.Micro.Platform
import Types.CustomEvent
import Types.Name
import Types.Project
import Types.Screen

class FormState a where
  -- Use the valid model contained in the form to modify the global AppState
  submitValid :: AppState -> a -> AppState

-- The `x` here is the form's state; it's not exposed in the `ActiveForm` type since
-- `ActiveForm` is used generically in `handleEvent` to send events to the form, regardless
-- of which form it is.
data ActiveForm = forall x. FormState x => ActiveForm (Maybe (Form x CustomEvent Name))
instance Show ActiveForm where show a = "(ActiveForm)"

data AppState = AppState { _activeScreen :: Screen
                         , _allProjects :: [Project]
                         , _activeForm :: ActiveForm -- The form to send events to, if one is currently active
                         } deriving (Show)
makeLenses ''AppState

handleSubmit :: forall x. FormState x => AppState -> Form x CustomEvent Name -> AppState
handleSubmit appState form = submitValid appState (formState form)
