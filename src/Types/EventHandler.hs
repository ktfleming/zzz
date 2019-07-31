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

type SelectItemHandler a = AppState -> a -> AppState

handleSubmit :: forall x . FormState x => AppState -> ZZZForm x -> AppState
handleSubmit appState form = submitValid appState (formState form)

-- The `x` here is the form's state; it's not exposed in the `ActiveForm` type since
-- `ActiveForm` is used generically in `handleEvent` to send events to the form, regardless
-- of which form it is.
data ActiveForm = forall x. FormState x => ActiveForm (ZZZForm x)

-- Similar to `ActiveForm`, but for Brick lists
data ActiveList = forall x. Show x => ActiveList (SelectItemHandler x) (ZZZList x)

instance Show ActiveForm where
  show _ = "(Form is active)"

instance Show ActiveList where
  show _ = "(List is active)"

-- Represents the widget that should receive input events
data EventHandler = FormHandler ActiveForm | ListHandler ActiveList | NoHandler deriving (Show)

-- TODO: this is defined here instead of in the UI.Projects.List module
-- to void cyclic imports for now. Have to figure out how to restructure modules
-- to avoid this.
projectListSelectHandler :: SelectItemHandler Project
projectListSelectHandler s p = (activeScreen .~ ProjectDetailsScreen p) s

instance FormState ProjectAddState where
  submitValid :: AppState -> ProjectAddState -> AppState
  submitValid appState ProjectAddState { _projectName = newName } =
    let newProject = Project { _projectName = newName }
    in  (allProjects <>~ [newProject]) appState
