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
import UI.Environments.Common (makeEnvironmentForm)
import UI.Form

finishAddingEnvironment ::
  EnvironmentId ->
  AppState 'EnvironmentAddTag ->
  AppState 'EnvironmentAddTag
finishAddingEnvironment eid s =
  let EnvironmentAddScreen (AppForm form) = s ^. screen
      e = Environment
        { environmentName = formState form ^. name,
          environmentVariables = formState form ^. variables
        }
   in s & environments . at eid ?~ e

showEnvironmentAddScreen :: AppState a -> AppState 'EnvironmentAddTag
showEnvironmentAddScreen s =
  let fs = EnvironmentFormState
        { environmentFormStateName = EnvironmentName "",
          environmentFormStateVariables = Seq.empty
        }
   in s & screen .~ EnvironmentAddScreen (makeEnvironmentForm fs)
