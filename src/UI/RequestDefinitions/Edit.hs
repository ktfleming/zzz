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
  -> RequestDefinitionFormState
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
  :: RequestDefinition -> RequestDefinitionFormState -> RequestDefinition
updateRequestDefinition base RequestDefinitionFormState { _requestDefinitionFormName, _requestDefinitionFormURL, _requestDefinitionFormMethod }
  = base
    &  requestDefinitionName
    .~ _requestDefinitionFormName
    &  requestDefinitionURL
    .~ _requestDefinitionFormURL
    &  requestDefinitionMethod
    .~ _requestDefinitionFormMethod

makeEditRequestDefinitionForm
  :: AppState -> RequestDefinitionContext -> ZZZForm RequestDefinitionFormState
makeEditRequestDefinitionForm s c =
  let
    RequestDefinition { _requestDefinitionName, _requestDefinitionURL, _requestDefinitionMethod }
      = model s c
    editState = RequestDefinitionFormState
      { _requestDefinitionFormName   = _requestDefinitionName
      , _requestDefinitionFormURL    = _requestDefinitionURL
      , _requestDefinitionFormMethod = _requestDefinitionMethod
      }
  in
    newForm
      [ (txt "Name:   " <+>) @@= editTextField requestDefinitionFormName
                                               RequestDefinitionFormNameField
                                               (Just 1)
      , (txt "URL:    " <+>) @@= editTextField requestDefinitionFormURL
                                               RequestDefinitionFormURLField
                                               (Just 1)
      , (txt "Method: " <+>)
        @@= radioField requestDefinitionFormMethod allMethodsRadio
      ]
      editState

showEditRequestDefinitionScreen
  :: AppState -> RequestDefinitionContext -> AppState
showEditRequestDefinitionScreen s c =
  (activeScreen .~ RequestEditScreen c (makeEditRequestDefinitionForm s c)) s

updateEditRequestDefinitionForm
  :: AppState
  -> RequestDefinitionContext
  -> ZZZForm RequestDefinitionFormState
  -> AppState
updateEditRequestDefinitionForm s c f =
  (activeScreen .~ RequestEditScreen c f) s
