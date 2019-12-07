{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Common
  ( makeProjectForm,
  )
where

import Brick
import Brick.Forms
import Control.Lens
import Types.Brick.Name
import Types.Classes.Fields
import Types.Models.Project
import UI.Form

makeProjectForm :: ProjectFormState a -> AppForm (ProjectFormState a)
makeProjectForm fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [(txt "Project Name: " <+>) @@= nonEmptyTextField (name . coerced) ProjectFormNameField]
      fs
