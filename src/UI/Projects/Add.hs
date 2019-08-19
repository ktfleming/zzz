{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
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
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
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
  :: MonadIO m => IxStateT m (AppState 'ProjectAddTag) (AppState 'ProjectAddTag) ()
finishAddingProject = do
  s <- iget
  let AppState { appStateScreen = ProjectAddScreen form } = s
  pid <- liftIO $ ProjectId <$> nextRandom
  let project = Project { projectName = formState form ^. name, projectRequestDefs = Map.empty }
  imodify $ projects . at pid ?~ project

makeProjectAddForm :: ZZZForm ProjectFormState
makeProjectAddForm = newForm
  [(txt "Project Name: " <+>) @@= editTextField (name . coerced) ProjectFormNameField (Just 1)]
  ProjectFormState { projectFormStateName = ProjectName "New Project" }

showProjectAddScreen :: Monad m => IxStateT m (AppState a) (AppState 'ProjectAddTag) ()
showProjectAddScreen = imodify $ screen .~ ProjectAddScreen makeProjectAddForm

updateProjectAddForm
  :: BrickEvent Name CustomEvent
  -> IxStateT (EventM Name) (AppState 'ProjectAddTag) (AppState 'ProjectAddTag) ()
updateProjectAddForm ev = do
  s <- iget
  let ProjectAddScreen form = s ^. screen
  updatedForm <- ilift $ handleFormEvent ev form
  imodify $ screen .~ ProjectAddScreen updatedForm -- TODO: can do this without resetting the whole screen?
