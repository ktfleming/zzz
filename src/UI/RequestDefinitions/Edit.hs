{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.RequestDefinitions.Edit where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editField
                                                , editTextField
                                                , newForm
                                                , radioField
                                                , (@@=)
                                                )
import           Control.Lens
import           Data.Coerce                    ( coerce )
import           Data.Generics.Product.Typed    ( typed )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.HasId            ( model )
import           Types.Methods                  ( allMethodsRadio )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..)
                                                , validateUrl
                                                )
import           UI.Form                        ( ZZZForm
                                                , renderText
                                                )

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
      , (txt "URL:    " <+>)
        @@= editField (url . coerced)
                      RequestDefinitionFormUrlField
                      (Just 1)
                      coerce
                      validateUrl
                      renderText
                      id
      , (txt "Method: " <+>) @@= radioField method allMethodsRadio
      ]
      editState

showEditRequestDefinitionScreen
  :: AppState -> RequestDefinitionContext -> AppState
showEditRequestDefinitionScreen s c =
  s & screen .~ RequestEditScreen c (makeEditRequestDefinitionForm s c)

updateEditRequestDefinitionForm
  :: AppState -> ZZZForm RequestDefinitionFormState -> AppState
updateEditRequestDefinitionForm s f =
  s
    &  screen
    .  _RequestEditScreen
    .  typed @(ZZZForm RequestDefinitionFormState)
    .~ f
