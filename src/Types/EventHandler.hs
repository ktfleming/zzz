{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Types.EventHandler where

import           Brick.Forms                    ( formState )
import           Lens.Micro.Platform            ( (.~)
                                                , (<>~)
                                                )
import           Types.AppState                 ( AppState(..)
                                                , activeScreen
                                                , allProjects
                                                )
import           Types.Project
import           Types.Screen
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )
import           UI.Projects.Add                ( ProjectAddState(..) )

class FormState a where
  -- Use the valid model contained in the form to modify the global AppState
  submitValid :: AppState -> a -> AppState

class ListSelectable a where
  onSelect :: AppState -> a -> AppState

handleSubmit :: forall x . FormState x => AppState -> ZZZForm x -> AppState
handleSubmit appState form = submitValid appState (formState form)

-- The `x` here is the form's state; it's not exposed in the `ActiveForm` type since
-- `ActiveForm` is used generically in `handleEvent` to send events to the form, regardless
-- of which form it is.
data ActiveForm = forall x. FormState x => ActiveForm (ZZZForm x)

-- Similar to `ActiveForm`, but for Brick lists
data ActiveList = forall x. ListSelectable x => ActiveList (ZZZList x)

instance FormState ProjectAddState where
  submitValid :: AppState -> ProjectAddState -> AppState
  submitValid appState ProjectAddState { _projectName = newName } =
    let newProject = Project { _projectName = newName }
    in  (allProjects <>~ [newProject]) appState

instance ListSelectable Project where
  onSelect :: AppState -> Project -> AppState
  onSelect s p = (activeScreen .~ ProjectDetailsScreen p) s

instance Show ActiveForm where
  show _ = "(Form is active)"

instance Show ActiveList where
  show _ = "(List is active)"
