{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module UI.Environments.Edit
  ( finishEditingEnvironment,
    showEnvironmentEditScreen,
    makeEnvironmentEditForm,
  )
where

import Brick
  ( (<+>),
    (<=>),
    txt,
  )
import Brick.Forms
  ( (@@=),
    formState,
    newForm,
    setFormConcat,
  )
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import Data.String (fromString)
import Language.Haskell.DoNotation
import Types.AppState
import Types.Brick.Name
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Forms (FormMode (..))
import Types.Models.Environment
import Types.Models.Screen
import UI.Form
  ( AppForm (..),
    nonEmptyTextField,
    spacedConcat,
  )
import UI.Forms.KeyValueList (makeKeyValueForm)
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

makeEnvironmentEditForm :: EnvironmentFormState 'Editing -> AppForm (EnvironmentFormState 'Editing)
makeEnvironmentEditForm fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [ (txt "Environment Name: " <+>)
          @@= nonEmptyTextField (name . coerced) EnvironmentFormNameField,
        (txt "Variables:" <=>) @@= makeKeyValueForm variables VariablesField
      ]
      fs

showEnvironmentEditScreen ::
  IxMonadState m => EnvironmentContext -> m (AppState a) (AppState 'EnvironmentEditTag) ()
showEnvironmentEditScreen c = do
  s <- iget
  let e = model s c
      fs = EnvironmentFormState
        { environmentFormStateName = e ^. name,
          environmentFormStateVariables = e ^. variables
        }
  imodify $ screen .~ EnvironmentEditScreen c (makeEnvironmentEditForm fs)
