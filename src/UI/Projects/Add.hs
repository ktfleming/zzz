{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Add where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , newForm
                                                , (@@=)
                                                )
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Data.UUID.V4                   ( nextRandom )
import           Types.AppState
import           Types.Brick.Name
import           Types.Models.ID                ( ProjectID(..) )
import           Types.Models.Project
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

finishAddingProject :: AppState -> ProjectFormState -> IO AppState
finishAddingProject s ProjectFormState { _projectFormName = newName } = do
  pid <- ProjectID <$> nextRandom
  let project =
        Project { _projectName = newName, _requestDefinitions = Map.empty }
      projectMap = Map.singleton pid project
  return $ (projects <>~ projectMap) s

makeProjectAddForm :: ZZZForm ProjectFormState
makeProjectAddForm = newForm
  [ (txt "Project Name: " <+>)
      @@= editTextField projectFormName ProjectFormNameField (Just 1)
  ]
  ProjectFormState { _projectFormName = "New Project" }

showProjectAddScreen :: AppState -> AppState
showProjectAddScreen = activeScreen .~ ProjectAddScreen makeProjectAddForm

updateProjectAddForm :: AppState -> ZZZForm ProjectFormState -> AppState
updateProjectAddForm s f = s & activeScreen .~ ProjectAddScreen f
