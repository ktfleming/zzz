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
import Types.SafetyLevel
import UI.Form
import UI.Forms.KeyValueList (makeKeyValueForm)

makeEnvironmentForm :: EnvironmentFormState -> AppForm EnvironmentFormState
makeEnvironmentForm fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [ (txt "Environment Name: " <+>)
          @@= nonEmptyTextField (name . coerced) EnvironmentFormNameField,
        (txt "Variables:" <=>) @@= makeKeyValueForm variables VariablesField,
        (txt "Safety level:" <=>) @@= radioField safetyLevel allSafetyLevelsRadio
      ]
      fs
