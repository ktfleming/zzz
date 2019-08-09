{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Add where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , newForm
                                                , (@@=)
                                                )
import qualified Data.Map.Strict               as Map
import           Data.UUID.V4                   ( nextRandom )
import           Lens.Micro.Platform            ( (&)
                                                , (.~)
                                                , (<>~)
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Models.ID                ( ProjectID(..) )
import           Types.Models.Project
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )

finishAddingProject :: AppState -> ProjectAddState -> IO AppState
finishAddingProject s ProjectAddState { _projectAddName = newName } = do
  pid <- ProjectID <$> nextRandom
  let project =
        Project { _projectName = newName, _requestDefinitions = Map.empty }
      projectMap = Map.singleton pid project
  return $ (projects <>~ projectMap) s

makeProjectAddForm :: ZZZForm ProjectAddState
makeProjectAddForm = newForm
  [ (txt "Project Name: " <+>)
      @@= editTextField projectAddName ProjectAddNameField (Just 1)
  ]
  ProjectAddState { _projectAddName = "New Project" }

showProjectAddScreen :: AppState -> AppState
showProjectAddScreen = activeScreen .~ ProjectAddScreen makeProjectAddForm

updateProjectAddForm :: AppState -> ZZZForm ProjectAddState -> AppState
updateProjectAddForm s f = s & activeScreen .~ ProjectAddScreen f
