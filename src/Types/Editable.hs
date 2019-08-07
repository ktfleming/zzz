{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilyDependencies    #-}

module Types.Editable where

import           Brick                      (txt, (<+>))
import           Brick.Forms                (editTextField, newForm, (@@=))
import           Lens.Micro.Platform        (at, (&), (.~), _Just)
import           Types.AppState
import           Types.Name
import           Types.Project
import           Types.RequestDefinition
import           Types.Screen
import           Types.WithID               (HasID (..))
import           UI.Form                    (ZZZForm)
import           UI.Projects.Edit
import           UI.RequestDefinitions.Edit

class HasID a => Editable a where
  type family EditState a = b | b -> a -- The model that's used to edit an `a`

  -- Use a valid edit state to modify the global AppState.
  finishEditing :: AppState -> Context a -> EditState a -> AppState

  -- Given a base model and a valid edit state, produce an updated model
  update :: a -> EditState a -> a

  -- Given the context that uniquely identifies a model, create the form used to edit it
  makeEditForm :: AppState -> Context a -> ZZZForm (EditState a)

  -- Given the context that uniquely identifies a model, update the AppState
  -- to display its "edit" screen
  showEditScreen :: AppState -> Context a -> AppState

  -- Given a context and a ZZZForm (in practice, one returned from Brick's `handleFormEvent`,
  -- update the AppState by update the active screen
  updateEditForm :: AppState -> Context a -> ZZZForm (EditState a) -> AppState

instance Editable Project where
  type EditState Project = ProjectEditState

  finishEditing :: AppState -> ProjectContext -> ProjectEditState -> AppState
  finishEditing appState context@(ProjectContext pid) editState =
    let
      base     = model appState context
      newModel = update base editState
    in appState & (projects . at pid . _Just .~ newModel)

  update :: Project -> ProjectEditState -> Project
  update base ProjectEditState { _projectEditName } =
    (projectName .~ _projectEditName) base

  makeEditForm :: AppState -> ProjectContext -> ZZZForm ProjectEditState
  makeEditForm s c =
    let
      Project { _projectName } = model s c
      editState = ProjectEditState { _projectEditName = _projectName }
    in newForm
      [ (txt "Project Name: " <+>)
          @@= editTextField projectEditName ProjectEditNameField (Just 1)
      ]
      editState

  showEditScreen :: AppState -> ProjectContext -> AppState
  showEditScreen s c =
    (activeScreen .~ ProjectEditScreen c (makeEditForm s c)) s

  updateEditForm
    :: AppState -> ProjectContext -> ZZZForm ProjectEditState -> AppState
  updateEditForm s c f = (activeScreen .~ ProjectEditScreen c f) s

instance Editable RequestDefinition where
  type EditState RequestDefinition = RequestDefinitionEditState

  finishEditing
    :: AppState
    -> RequestDefinitionContext
    -> RequestDefinitionEditState
    -> AppState
  finishEditing appState c@(RequestDefinitionContext pid rid) editState =
    let
      base     = model appState c
      newModel = update base editState
    in
      appState
        & (  projects
          .  at pid
          .  _Just
          .  requestDefinitions
          .  at rid
          .  _Just
          .~ newModel
          )

  update :: RequestDefinition -> RequestDefinitionEditState -> RequestDefinition
  update base RequestDefinitionEditState { _requestDefinitionEditName } =
    (requestDefinitionName .~ _requestDefinitionEditName) base

  makeEditForm
    :: AppState
    -> RequestDefinitionContext
    -> ZZZForm RequestDefinitionEditState
  makeEditForm s c =
    let
      RequestDefinition { _requestDefinitionName } = model s c
      editState = RequestDefinitionEditState
        { _requestDefinitionEditName = _requestDefinitionName
        }
    in newForm
      [ (txt "Request Definition Name: " <+>) @@= editTextField
          requestDefinitionEditName
          RequestDefinitionNameEditField
          (Just 1)
      ]
      editState

  showEditScreen :: AppState -> RequestDefinitionContext -> AppState
  showEditScreen s c =
    (activeScreen .~ RequestEditScreen c (makeEditForm s c)) s

  updateEditForm
    :: AppState
    -> RequestDefinitionContext
    -> ZZZForm RequestDefinitionEditState
    -> AppState
  updateEditForm s c f = (activeScreen .~ RequestEditScreen c f) s
