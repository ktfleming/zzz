{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Environments.Edit
  ( finishEditingEnvironment,
    showEnvironmentEditScreen,
  )
where

import Brick.Forms
import Control.Lens
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.Environment
import Types.Models.Screen
import UI.Environments.Common (makeEnvironmentForm)
import UI.Form

finishEditingEnvironment ::
  AppState 'EnvironmentEditTag -> AppState 'EnvironmentEditTag
finishEditingEnvironment s =
  let EnvironmentEditScreen c@(EnvironmentContext eid) (AppForm form) = s ^. screen
      newModel = updateEnvironment (formState form) (model s c)
   in s & environments . ix eid .~ newModel

updateEnvironment :: EnvironmentFormState -> Environment -> Environment
updateEnvironment form = (name .~ form ^. name) . (variables .~ form ^. variables)

showEnvironmentEditScreen ::
  EnvironmentContext -> AppState a -> AppState 'EnvironmentEditTag
showEnvironmentEditScreen c s =
  let e = model s c
      fs =
        EnvironmentFormState
          { environmentFormStateName = e ^. name,
            environmentFormStateVariables = e ^. variables
          }
   in s & screen .~ EnvironmentEditScreen c (makeEnvironmentForm fs)
