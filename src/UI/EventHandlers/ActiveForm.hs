{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module UI.EventHandlers.ActiveForm where

import           Lens.Micro.Platform            ( (<>~) )
import           Types.AppState
import           Types.Project
import           UI.Form                        ( ZZZForm )
import           UI.Projects.Add

class FormState a where
  -- Use the valid model contained in the form to modify the global AppState
  onSubmit :: AppState -> a -> AppState

-- The `x` here is the form's state; it's not exposed in the `ActiveForm` type since
-- `ActiveForm` is used generically in `handleEvent` to send events to the form, regardless
-- of which form it is.
data ActiveForm = forall x. FormState x => ActiveForm (ZZZForm x)

instance FormState ProjectAddState where
  onSubmit :: AppState -> ProjectAddState -> AppState
  onSubmit appState ProjectAddState { _projectName = newName } =
    let newProject = Project { _projectName = newName }
    in  (allProjects <>~ [newProject]) appState

instance Show ActiveForm where
  show _ = "(Form is active)"
