{-# LANGUAGE OverloadedStrings #-}

module UI.Environments.Common
  ( makeEnvironmentForm,
  )
where

import Brick
import Brick.Forms
import Control.Lens
import Types.Brick.Name
import Types.Classes.Fields
import Types.Models.Environment
import UI.Form
import UI.Forms.KeyValueList (makeKeyValueForm)

makeEnvironmentForm :: EnvironmentFormState a -> AppForm (EnvironmentFormState a)
makeEnvironmentForm fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [ (txt "Environment Name: " <+>)
          @@= nonEmptyTextField (name . coerced) EnvironmentFormNameField,
        (txt "Variables:" <=>) @@= makeKeyValueForm variables VariablesField
      ]
      fs
