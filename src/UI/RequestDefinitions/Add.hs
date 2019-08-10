{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefinitions.Add where

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
import           Types.Methods
import           Types.Models.ID                ( RequestDefinitionID(..) )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )


finishAddingRequestDefinition
  :: AppState -> ProjectContext -> RequestDefinitionFormState -> IO AppState
finishAddingRequestDefinition s (ProjectContext pid) formState = do
  rid <- RequestDefinitionID <$> nextRandom
  let req = RequestDefinition { requestDefinitionName   = formState ^. name
                              , requestDefinitionUrl    = formState ^. url
                              , requestDefinitionMethod = formState ^. method
                              }
      reqMap = Map.singleton rid req
  return $ (projects . at pid . _Just . requestDefinitions <>~ reqMap) s

makeAddRequestDefinitionForm :: ZZZForm RequestDefinitionFormState
makeAddRequestDefinitionForm = newForm
  [ (txt "Request Definition Name: " <+>)
    @@= editTextField name RequestDefinitionFormNameField (Just 1)
  , (txt "URL: " <+>)
    @@= editTextField url RequestDefinitionFormUrlField (Just 1)
  ]
  RequestDefinitionFormState
    { requestDefinitionFormStateName   = "New Request Definition"
    , requestDefinitionFormStateUrl    = "http://example.com"
    , requestDefinitionFormStateMethod = Get
    }

showAddRequestDefinitionScreen :: AppState -> ProjectContext -> AppState
showAddRequestDefinitionScreen s c =
  s & screen .~ RequestAddScreen c makeAddRequestDefinitionForm

updateAddRequestDefinitionForm
  :: AppState
  -> ProjectContext
  -> ZZZForm RequestDefinitionFormState
  -> AppState
updateAddRequestDefinitionForm s c f = s & screen .~ RequestAddScreen c f
