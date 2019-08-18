{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Add where

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
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State      ( StateT
                                                , modify
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.UUID.V4                   ( nextRandom )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name
import           Types.Models.Id                ( ProjectId(..) )
import           Types.Models.Project
import           Types.Models.RequestDef        ( name )
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

finishAddingProject
  :: MonadIO m => ZZZForm ProjectFormState -> StateT AppState m ()
finishAddingProject form = do
  pid <- liftIO $ ProjectId <$> nextRandom
  let project = Project { projectName        = formState form ^. name
                        , projectRequestDefs = Map.empty
                        }
  modify $ projects . at pid ?~ project

makeProjectAddForm :: ZZZForm ProjectFormState
makeProjectAddForm = newForm
  [ (txt "Project Name: " <+>)
      @@= editTextField (name . coerced) ProjectFormNameField (Just 1)
  ]
  ProjectFormState { projectFormStateName = ProjectName "New Project" }

showProjectAddScreen :: Monad m => StateT AppState m ()
showProjectAddScreen = modify $ screen .~ ProjectAddScreen makeProjectAddForm

updateProjectAddForm
  :: ZZZForm ProjectFormState
  -> BrickEvent Name CustomEvent
  -> StateT AppState (EventM Name) ()
updateProjectAddForm form ev = do
  updatedForm <- lift $ handleFormEvent ev form
  modify $ screen . _ProjectAddScreen .~ updatedForm
