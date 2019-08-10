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
import           Types.Models.RequestDefinition ( name )
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

finishAddingProject :: AppState -> ProjectFormState -> IO AppState
finishAddingProject s formState = do
  pid <- ProjectID <$> nextRandom
  let project = Project { projectName               = formState ^. name
                        , projectRequestDefinitions = Map.empty
                        }
      projectMap = Map.singleton pid project
  return $ (projects <>~ projectMap) s

makeProjectAddForm :: ZZZForm ProjectFormState
makeProjectAddForm = newForm
  [ (txt "Project Name: " <+>)
      @@= editTextField name ProjectFormNameField (Just 1)
  ]
  ProjectFormState { projectFormStateName = "New Project" }

showProjectAddScreen :: AppState -> AppState
showProjectAddScreen = screen .~ ProjectAddScreen makeProjectAddForm

updateProjectAddForm :: AppState -> ZZZForm ProjectFormState -> AppState
updateProjectAddForm s f = s & screen .~ ProjectAddScreen f
