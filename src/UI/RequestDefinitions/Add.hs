{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefinitions.Add where

import           Brick                          (txt, (<+>))
import           Brick.Forms                    (editTextField, newForm, (@@=))
import qualified Data.Map.Strict                as Map
import           Data.UUID.V4                   (nextRandom)
import           Lens.Micro.Platform            (at, (&), (.~), (<>~), _Just)
import           Types.AppState
import           Types.Brick.Name
import           Types.Models.ID                (RequestDefinitionID (..))
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           UI.Form                        (ZZZForm)


finishAddingRequestDefinition
  :: AppState -> ProjectContext -> RequestDefinitionAddState -> IO AppState
finishAddingRequestDefinition s (ProjectContext pid) RequestDefinitionAddState { _requestDefinitionAddName = newName }
  = do
    rid <- RequestDefinitionID <$> nextRandom
    let req    = RequestDefinition { _requestDefinitionName = newName }
        reqMap = Map.singleton rid req
    return $ (projects . at pid . _Just . requestDefinitions <>~ reqMap) s

makeAddRequestDefinitionForm :: ZZZForm RequestDefinitionAddState
makeAddRequestDefinitionForm = newForm
  [ (txt "Request Definition Name: " <+>) @@= editTextField
      requestDefinitionAddName
      RequestDefinitionNameAddField
      (Just 1)
  ]
  RequestDefinitionAddState
    { _requestDefinitionAddName = "New Request Definition"
    }

showAddRequestDefinitionScreen :: AppState -> ProjectContext -> AppState
showAddRequestDefinitionScreen s c = s & activeScreen .~ RequestAddScreen c makeAddRequestDefinitionForm

updateAddRequestDefinitionForm
  :: AppState
  -> ProjectContext
  -> ZZZForm RequestDefinitionAddState
  -> AppState
updateAddRequestDefinitionForm s c f = s & activeScreen .~ RequestAddScreen c f
