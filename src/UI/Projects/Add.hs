{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Projects.Add
  ( finishAddingProject
  , showProjectAddScreen
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
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.UUID.V4                   ( nextRandom )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Models.Id                ( ProjectId(..) )
import           Types.Models.Project
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm
                                                , spacedConcat
                                                )

finishAddingProject
  :: MonadIO m => IxStateT m (AppState 'ProjectAddTag) (AppState 'ProjectAddTag) ()
finishAddingProject = do
  s <- iget
  let ProjectAddScreen form = s ^. screen
  pid <- liftIO $ ProjectId <$> nextRandom
  let project = Project { projectName = formState form ^. name, projectRequestDefs = Map.empty }
  imodify $ projects . at pid ?~ project

makeProjectAddForm :: ZZZForm ProjectFormState
makeProjectAddForm = setFormConcat spacedConcat $ newForm
  [(txt "Project Name: " <+>) @@= editTextField (name . coerced) ProjectFormNameField (Just 1)]
  ProjectFormState { projectFormStateName = ProjectName "New Project" }

showProjectAddScreen :: Monad m => IxStateT m (AppState a) (AppState 'ProjectAddTag) ()
showProjectAddScreen = imodify $ screen .~ ProjectAddScreen makeProjectAddForm
