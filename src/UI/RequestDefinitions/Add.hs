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
finishAddingRequestDefinition s (ProjectContext pid) RequestDefinitionFormState { _requestDefinitionFormName = newName, _requestDefinitionFormURL = u, _requestDefinitionFormMethod = m }
  = do
    rid <- RequestDefinitionID <$> nextRandom
    let req = RequestDefinition { _requestDefinitionName   = newName
                                , _requestDefinitionURL    = u
                                , _requestDefinitionMethod = m
                                }
        reqMap = Map.singleton rid req
    return $ (projects . at pid . _Just . requestDefinitions <>~ reqMap) s

makeAddRequestDefinitionForm :: ZZZForm RequestDefinitionFormState
makeAddRequestDefinitionForm = newForm
  [ (txt "Request Definition Name: " <+>) @@= editTextField
    requestDefinitionFormName
    RequestDefinitionFormNameField
    (Just 1)
  , (txt "URL: " <+>) @@= editTextField requestDefinitionFormURL
                                        RequestDefinitionFormURLField
                                        (Just 1)
  ]
  RequestDefinitionFormState
    { _requestDefinitionFormName   = "New Request Definition"
    , _requestDefinitionFormURL    = "http://example.com"
    , _requestDefinitionFormMethod = Get
    }

showAddRequestDefinitionScreen :: AppState -> ProjectContext -> AppState
showAddRequestDefinitionScreen s c =
  s & activeScreen .~ RequestAddScreen c makeAddRequestDefinitionForm

updateAddRequestDefinitionForm
  :: AppState
  -> ProjectContext
  -> ZZZForm RequestDefinitionFormState
  -> AppState
updateAddRequestDefinitionForm s c f = s & activeScreen .~ RequestAddScreen c f
