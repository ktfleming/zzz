{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.Projects.Edit where

import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )

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
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Data.String                    ( fromString )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name
import           Types.Classes.HasId            ( model )
import           Types.Models.Project
import           Types.Models.RequestDef        ( name )
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

finishEditingProject
  :: Monad m => IxStateT m (AppState 'ProjectEditTag) (AppState 'ProjectEditTag) ()
finishEditingProject = do
  s <- iget
  let ProjectEditScreen c@(ProjectContext pid) form = s ^. screen
      base     = model s c
      newModel = updateProject base (formState form)
  imodify $ projects . ix pid .~ newModel

updateProject :: Project -> ProjectFormState -> Project
updateProject base form = (name .~ (form ^. name)) base

makeEditProjectForm :: AppState a -> ProjectContext -> ZZZForm ProjectFormState
makeEditProjectForm s c =
  let p         = model s c
      editState = ProjectFormState { projectFormStateName = p ^. name }
  in  newForm
        [ (txt "Project Name: " <+>)
            @@= editTextField (name . coerced) ProjectFormNameField (Just 1)
        ]
        editState

showEditProjectScreen
  :: Monad m => ProjectContext -> IxStateT m (AppState a) (AppState 'ProjectEditTag) ()
showEditProjectScreen c =
  imodify $ \s -> s & screen .~ ProjectEditScreen c (makeEditProjectForm s c)

updateEditProjectForm
  :: BrickEvent Name CustomEvent
  -> IxStateT (EventM Name) (AppState 'ProjectEditTag) (AppState 'ProjectEditTag) ()
updateEditProjectForm ev = do
  s <- iget
  let ProjectEditScreen c form = s ^. screen
  updatedForm <- ilift $ handleFormEvent ev form
  imodify $ screen .~ ProjectEditScreen c updatedForm
