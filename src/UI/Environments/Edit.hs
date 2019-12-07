{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module UI.Environments.Edit
  ( finishEditingEnvironment,
    showEnvironmentEditScreen,
  )
where

import Brick.Forms
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import Language.Haskell.DoNotation
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Forms (FormMode (..))
import Types.Models.Environment
import Types.Models.Screen
import UI.Environments.Common (makeEnvironmentForm)
import UI.Form
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

finishEditingEnvironment ::
  IxMonadState m => m (AppState 'EnvironmentEditTag) (AppState 'EnvironmentEditTag) ()
finishEditingEnvironment = do
  s <- iget
  let EnvironmentEditScreen c@(EnvironmentContext eid) (AppForm form) = s ^. screen
      newModel = updateEnvironment (model s c) (formState form)
  imodify $ environments . ix eid .~ newModel

updateEnvironment :: Environment -> EnvironmentFormState 'Editing -> Environment
updateEnvironment base form = base & name .~ (form ^. name) & variables .~ (form ^. variables)

showEnvironmentEditScreen ::
  IxMonadState m => EnvironmentContext -> m (AppState a) (AppState 'EnvironmentEditTag) ()
showEnvironmentEditScreen c = do
  s <- iget
  let e = model s c
      fs = EnvironmentFormState
        { environmentFormStateName = e ^. name,
          environmentFormStateVariables = e ^. variables
        }
  imodify $ screen .~ EnvironmentEditScreen c (makeEnvironmentForm fs)
