{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefinitions.Edit where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , newForm
                                                , radioField
                                                , (@@=)
                                                )
import           Lens.Micro.Platform            ( at
                                                , (&)
                                                , (.~)
                                                , _Just
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.WithID           ( model )
import           Types.Methods                  ( allMethodsRadio )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm )


finishEditingRequestDefinition
  :: AppState
  -> RequestDefinitionContext
  -> RequestDefinitionEditState
  -> AppState
finishEditingRequestDefinition appState c@(RequestDefinitionContext pid rid) editState
  = let base     = model appState c
        newModel = updateRequestDefinition base editState
    in  appState
          & (  projects
            .  at pid
            .  _Just
            .  requestDefinitions
            .  at rid
            .  _Just
            .~ newModel
            )

updateRequestDefinition
  :: RequestDefinition -> RequestDefinitionEditState -> RequestDefinition
updateRequestDefinition base RequestDefinitionEditState { _requestDefinitionEditName, _requestDefinitionEditURL, _requestDefinitionEditMethod }
  = base
    &  requestDefinitionName
    .~ _requestDefinitionEditName
    &  requestDefinitionURL
    .~ _requestDefinitionEditURL
    &  requestDefinitionMethod
    .~ _requestDefinitionEditMethod

makeEditRequestDefinitionForm
  :: AppState -> RequestDefinitionContext -> ZZZForm RequestDefinitionEditState
makeEditRequestDefinitionForm s c =
  let
    RequestDefinition { _requestDefinitionName, _requestDefinitionURL, _requestDefinitionMethod }
      = model s c
    editState = RequestDefinitionEditState
      { _requestDefinitionEditName   = _requestDefinitionName
      , _requestDefinitionEditURL    = _requestDefinitionURL
      , _requestDefinitionEditMethod = _requestDefinitionMethod
      }
  in
    newForm
      [ (txt "Name:   " <+>) @@= editTextField requestDefinitionEditName
                                               RequestDefinitionNameEditField
                                               (Just 1)
      , (txt "URL:    " <+>) @@= editTextField requestDefinitionEditURL
                                               RequestDefinitionURLEditField
                                               (Just 1)
      , (txt "Method: " <+>)
        @@= radioField requestDefinitionEditMethod allMethodsRadio
      ]
      editState

showEditRequestDefinitionScreen
  :: AppState -> RequestDefinitionContext -> AppState
showEditRequestDefinitionScreen s c =
  (activeScreen .~ RequestEditScreen c (makeEditRequestDefinitionForm s c)) s

updateEditRequestDefinitionForm
  :: AppState
  -> RequestDefinitionContext
  -> ZZZForm RequestDefinitionEditState
  -> AppState
updateEditRequestDefinitionForm s c f =
  (activeScreen .~ RequestEditScreen c f) s
