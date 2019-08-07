{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilyDependencies    #-}

module Types.Addable where

import           Brick               (txt, (<+>))
import           Brick.Forms         (editTextField, newForm, (@@=))
import qualified Data.Map.Strict     as Map
import           Data.UUID.V4        (nextRandom)
import           Lens.Micro.Platform ((.~), (<>~), at, _Just)
import           Types.AppState
import           Types.ID
import           Types.Name
import           Types.Project
import           Types.Screen
import           Types.WithID        (HasID (..))
import           UI.Form             (ZZZForm)
import           UI.Projects.Add     (ProjectAddState (..), projectAddName)
import UI.RequestDefinitions.Add (RequestDefinitionAddState, RequestDefinitionAddState(..), requestDefinitionAddName)
import Types.RequestDefinition (RequestDefinition(..))

-- This is the "context" required when adding a Project or other top-level model, which doesn't actually
-- need a context at all since these models exist at the roots of the model map(s).
data NoContext = NoContext

class HasID a => Addable a where
  type family AddState a = b | b -> a
  type family AddContext a = b | b -> a -- The context necessary to complete the add action.
                                        -- Note that this is different than the "regular" context;
                                        -- generally it will refer to the context of the parent resource
                                        -- (since we need to know where to insert the new model in the global model map)

  -- Given a valid add state, add the resulting model to the global AppState.
  -- Needs IO since this involves generating a new random UUID.
  finishAdding :: AppState -> AddContext a -> AddState a -> IO AppState

  makeAddForm :: ZZZForm (AddState a)

  updateAddForm :: AppState -> AddContext a -> ZZZForm (AddState a) -> AppState

instance Addable Project where
  type AddState Project = ProjectAddState
  type AddContext Project = NoContext

  finishAdding :: AppState -> NoContext -> ProjectAddState -> IO AppState
  finishAdding s _ ProjectAddState { _projectAddName = newName } = do
    pid <- ProjectID <$> nextRandom
    let
      project = Project
        { _projectName        = newName
        , _requestDefinitions = Map.empty
        }
      projectMap = Map.singleton pid project
    return $ (projects <>~ projectMap) s

  makeAddForm :: ZZZForm ProjectAddState
  makeAddForm = newForm
    [ (txt "Project Name: " <+>)
        @@= editTextField projectAddName ProjectAddNameField (Just 1)
    ]
    ProjectAddState { _projectAddName = "New Project" }

  updateAddForm :: AppState -> NoContext -> ZZZForm ProjectAddState -> AppState
  updateAddForm s _ f = (activeScreen .~ ProjectAddScreen f) s

instance Addable RequestDefinition where
  type AddState RequestDefinition = RequestDefinitionAddState
  type AddContext RequestDefinition = ProjectContext

  finishAdding :: AppState -> ProjectContext -> RequestDefinitionAddState -> IO AppState
  finishAdding s (ProjectContext pid) RequestDefinitionAddState { _requestDefinitionAddName = newName } = do
    rid <- RequestDefinitionID <$> nextRandom
    let req = RequestDefinition { _requestDefinitionName = newName }
        reqMap = Map.singleton rid req
    return $ (projects . at pid . _Just . requestDefinitions <>~ reqMap) s

  makeAddForm :: ZZZForm RequestDefinitionAddState
  makeAddForm = newForm
    [ (txt "Request Definition Name: " <+>) @@= editTextField requestDefinitionAddName RequestDefinitionNameAddField (Just 1)
    ]
    RequestDefinitionAddState { _requestDefinitionAddName = "New Request Definition"}

  updateAddForm :: AppState -> ProjectContext -> ZZZForm RequestDefinitionAddState -> AppState
  updateAddForm s c f = (activeScreen .~ RequestAddScreen c f) s