{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Environments.Add
  ( finishAddingEnvironment,
    showEnvironmentAddScreen,
  )
where

import Brick.Forms
import Control.Lens
import qualified Data.Sequence as Seq
import Types.AppState
import Types.Classes.Fields
import Types.Models.Environment
  ( Environment (..),
    EnvironmentFormState (..),
    EnvironmentName (..),
  )
import Types.Models.Id (EnvironmentId (..))
import Types.Models.Screen
import Types.SafetyLevel
import UI.Environments.Common (makeEnvironmentForm)
import UI.Form

finishAddingEnvironment ::
  EnvironmentId ->
  AppState 'EnvironmentAddTag ->
  AppState 'EnvironmentAddTag
finishAddingEnvironment eid s =
  let EnvironmentAddScreen (AppForm form) = s ^. screen
      fs = formState form
      e =
        Environment
          { environmentName = fs ^. name,
            environmentVariables = fs ^. variables,
            environmentSafetyLevel = fs ^. safetyLevel
          }
   in s & environments . at eid ?~ e

showEnvironmentAddScreen :: AppState a -> AppState 'EnvironmentAddTag
showEnvironmentAddScreen s =
  let fs =
        EnvironmentFormState
          { environmentFormStateName = EnvironmentName "",
            environmentFormStateVariables = Seq.empty,
            environmentFormStateSafetyLevel = NeverPrompt
          }
   in s & screen .~ EnvironmentAddScreen (makeEnvironmentForm fs)
