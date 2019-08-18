{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.Projects.Edit where

import           Brick                          ( BrickEvent
                                                , EventM
                                                , txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , formState
                                                , handleFormEvent
                                                , newForm
                                                , (@@=)
                                                )
import           Control.Lens
import           Control.Monad.Trans.Class      ( lift )
import           Data.Generics.Product.Typed    ( typed )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.HasId            ( model )
import           Types.Models.Project
import           Types.Models.RequestDef        ( name )
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

import           Control.Monad.Trans.State      ( StateT
                                                , get
                                                , modify
                                                )
import           Types.Brick.CustomEvent        ( CustomEvent )

finishEditingProject
  :: Monad m
  => ProjectContext
  -> ZZZForm ProjectFormState
  -> StateT AppState m ()
finishEditingProject c@(ProjectContext pid) form = do
  s <- get
  let base     = model s c
      newModel = updateProject base (formState form)
  modify $ projects . ix pid .~ newModel

updateProject :: Project -> ProjectFormState -> Project
updateProject base form = (name .~ (form ^. name)) base

makeEditProjectForm :: AppState -> ProjectContext -> ZZZForm ProjectFormState
makeEditProjectForm s c =
  let p         = model s c
      editState = ProjectFormState { projectFormStateName = p ^. name }
  in  newForm
        [ (txt "Project Name: " <+>)
            @@= editTextField (name . coerced) ProjectFormNameField (Just 1)
        ]
        editState

showEditProjectScreen :: Monad m => ProjectContext -> StateT AppState m ()
showEditProjectScreen c =
  modify $ \s -> s & screen .~ ProjectEditScreen c (makeEditProjectForm s c)

updateEditProjectForm
  :: ZZZForm ProjectFormState
  -> BrickEvent Name CustomEvent
  -> StateT AppState (EventM Name) ()
updateEditProjectForm form ev = do
  updatedForm <- lift $ handleFormEvent ev form
  modify
    $  screen
    .  _ProjectEditScreen
    .  typed @(ZZZForm ProjectFormState)
    .~ updatedForm
