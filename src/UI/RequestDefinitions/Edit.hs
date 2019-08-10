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
import           Control.Lens
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.HasId            ( model )
import           Types.Methods                  ( allMethodsRadio )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.Form                        ( ZZZForm )


finishEditingRequestDefinition
  :: AppState
  -> RequestDefinitionContext
  -> RequestDefinitionFormState
  -> AppState
finishEditingRequestDefinition appState c@(RequestDefinitionContext pid rid) editState
  = let base     = model appState c
        newModel = updateRequestDefinition base editState
    in  appState & (projects . ix pid . requestDefinitions . ix rid .~ newModel)

updateRequestDefinition
  :: RequestDefinition -> RequestDefinitionFormState -> RequestDefinition
updateRequestDefinition base formState =
  base
    &  name
    .~ (formState ^. name)
    &  url
    .~ (formState ^. url)
    &  method
    .~ (formState ^. method)

makeEditRequestDefinitionForm
  :: AppState -> RequestDefinitionContext -> ZZZForm RequestDefinitionFormState
makeEditRequestDefinitionForm s c =
  let
    r         = model s c
    editState = RequestDefinitionFormState
      { requestDefinitionFormStateName   = r ^. name
      , requestDefinitionFormStateUrl    = r ^. url
      , requestDefinitionFormStateMethod = r ^. method
      }
  in
    newForm
      [ (txt "Name:   " <+>) @@= editTextField (name . coerced)
                                               RequestDefinitionFormNameField
                                               (Just 1)
      , (txt "URL:    " <+>) @@= editTextField (url . coerced)
                                               RequestDefinitionFormUrlField
                                               (Just 1)
      , (txt "Method: " <+>) @@= radioField method allMethodsRadio
      ]
      editState

showEditRequestDefinitionScreen
  :: AppState -> RequestDefinitionContext -> AppState
showEditRequestDefinitionScreen s c =
  s & screen .~ RequestEditScreen c (makeEditRequestDefinitionForm s c)

updateEditRequestDefinitionForm
  :: AppState
  -> RequestDefinitionContext
  -> ZZZForm RequestDefinitionFormState
  -> AppState
updateEditRequestDefinitionForm s c f = s & screen .~ RequestEditScreen c f
