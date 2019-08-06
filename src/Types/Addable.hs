{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilyDependencies    #-}

module Types.Addable where

import           Brick               (txt, (<+>))
import           Brick.Forms         (editTextField, newForm, (@@=))
import qualified Data.Map.Strict     as Map
import           Data.UUID.V4        (nextRandom)
import           Lens.Micro.Platform ((.~), (<>~))
import           Types.AppState
import           Types.ID            (ProjectID (..))
import           Types.Name
import           Types.Project
import           Types.Screen
import           Types.WithID        (HasID (..))
import           UI.Form             (ZZZForm)
import           UI.Projects.Add     (ProjectAddState (..), projectAddName)


class HasID a => Addable a where
  type family AddState a = b | b -> a

  -- Given a valid add state, add the resulting model to the global AppState.
  -- Needs IO since this involves generating a new random UUID.
  finishAdding :: AppState -> AddState a -> IO AppState

  makeAddForm :: ZZZForm (AddState a)

  -- Similar to Editable's `updateEditForm`, but doesn't need a context
  -- (a model that hasn't been created yet doesn't have a context)
  updateAddForm :: AppState -> ZZZForm (AddState a) -> AppState

instance Addable Project where
  type AddState Project = ProjectAddState

  finishAdding :: AppState -> ProjectAddState -> IO AppState
  finishAdding s ProjectAddState { _projectAddName = newName } = do
    pid <- ProjectID <$> nextRandom
    let
      project = Project
        { projectID           = pid
        , _projectName        = newName
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

  updateAddForm :: AppState -> ZZZForm ProjectAddState -> AppState
  updateAddForm s f = (activeScreen .~ ProjectAddScreen f) s
