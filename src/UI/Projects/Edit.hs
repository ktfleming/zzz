{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.Projects.Edit
  ( finishEditingProject
  , showEditProjectScreen
  )
where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , formState
                                                , newForm
                                                , setFormConcat
                                                , (@@=)
                                                )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxMonadState
                                                , iget
                                                , imodify
                                                )
import           Data.String                    ( fromString )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(return, (>>), (>>=))
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Models.Project
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm
                                                , spacedConcat
                                                )

finishEditingProject
  :: IxMonadState m => m (AppState 'ProjectEditTag) (AppState 'ProjectEditTag) ()
finishEditingProject = do
  s <- iget
  let ProjectEditScreen c@(ProjectContext pid) form = s ^. screen
      base     = model s c
      newModel = updateProject base (formState form)
  imodify $ projects . ix pid .~ newModel

updateProject :: Project -> ProjectFormState -> Project
updateProject base form = base & name .~ (form ^. name)

makeEditProjectForm :: AppState a -> ProjectContext -> ZZZForm ProjectFormState
makeEditProjectForm s c =
  let p         = model s c
      editState = ProjectFormState { projectFormStateName = p ^. name }
  in  setFormConcat spacedConcat $ newForm
        [ (txt "Project Name: " <+>)
            @@= editTextField (name . coerced) ProjectFormNameField (Just 1)
        ]
        editState

showEditProjectScreen
  :: IxMonadState m => ProjectContext -> m (AppState a) (AppState 'ProjectEditTag) ()
showEditProjectScreen c =
  imodify $ \s -> s & screen .~ ProjectEditScreen c (makeEditProjectForm s c)
