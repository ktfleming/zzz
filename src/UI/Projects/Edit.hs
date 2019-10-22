{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module UI.Projects.Edit
  ( finishEditingProject,
    showEditProjectScreen,
    makeProjectEditForm,
  )
where

import Brick
  ( (<+>),
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
import Types.Models.Project
import Types.Models.Screen
import UI.Form
  ( AppForm (..),
    nonEmptyTextField,
    spacedConcat,
  )
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

finishEditingProject ::
  IxMonadState m => m (AppState 'ProjectEditTag) (AppState 'ProjectEditTag) ()
finishEditingProject = do
  s <- iget
  let ProjectEditScreen c@(ProjectContext pid) (AppForm form) = s ^. screen
      base = model s c
      newModel = updateProject base (formState form)
  imodify $ projects . ix pid .~ newModel

updateProject :: Project -> ProjectFormState 'Editing -> Project
updateProject base form = base & name .~ (form ^. name)

makeProjectEditForm :: ProjectFormState 'Editing -> AppForm (ProjectFormState 'Editing)
makeProjectEditForm fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [ (txt "Project Name: " <+>)
          @@= nonEmptyTextField (name . coerced) ProjectFormNameField
      ]
      fs

showEditProjectScreen ::
  IxMonadState m => ProjectContext -> m (AppState a) (AppState 'ProjectEditTag) ()
showEditProjectScreen c = do
  s <- iget
  let p = model s c
      fs = ProjectFormState {projectFormStateName = p ^. name}
  imodify $ screen .~ ProjectEditScreen c (makeProjectEditForm fs)
