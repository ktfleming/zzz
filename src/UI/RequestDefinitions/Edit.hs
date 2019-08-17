{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.RequestDefinitions.Edit where

import           Brick                          ( BrickEvent
                                                , EventM
                                                , txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editField
                                                , editTextField
                                                , formState
                                                , handleFormEvent
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

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State      ( StateT
                                                , modify
                                                )
import           Types.Brick.CustomEvent        ( CustomEvent )

finishEditingRequestDefinition
  :: Monad m
  => RequestDefinitionContext
  -> RequestDefinition
  -> ZZZForm RequestDefinitionFormState
  -> StateT AppState m ()
finishEditingRequestDefinition (RequestDefinitionContext pid rid) base form =
  let newModel = updateRequestDefinition base (formState form)
  in  modify $ projects . ix pid . requestDefinitions . ix rid .~ newModel

updateRequestDefinition
  :: RequestDefinition -> RequestDefinitionFormState -> RequestDefinition
updateRequestDefinition base form =
  base
    &  name
    .~ (form ^. name)
    &  url
    .~ (form ^. url)
    &  method
    .~ (form ^. method)

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
  :: Monad m => RequestDefinitionContext -> StateT AppState m ()
showEditRequestDefinitionScreen c = modify $ \s ->
  s & screen .~ RequestEditScreen c (makeEditRequestDefinitionForm s c)

updateEditRequestDefinitionForm
  :: ZZZForm RequestDefinitionFormState
  -> BrickEvent Name CustomEvent
  -> StateT AppState (EventM Name) ()
updateEditRequestDefinitionForm form ev = do
  updatedForm <- lift $ handleFormEvent ev form
  modify
    $  screen
    .  _RequestEditScreen
    .  typed @(ZZZForm RequestDefinitionFormState)
    .~ updatedForm
