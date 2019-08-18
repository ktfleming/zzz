{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.RequestDefs.Edit where

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
import           Types.Models.RequestDef
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

finishEditingRequestDef
  :: Monad m
  => RequestDefContext
  -> RequestDef
  -> ZZZForm RequestDefFormState
  -> StateT AppState m ()
finishEditingRequestDef (RequestDefContext pid rid) base form =
  let newModel = updateRequestDef base (formState form)
  in  modify $ projects . ix pid . requestDefs . ix rid .~ newModel

updateRequestDef :: RequestDef -> RequestDefFormState -> RequestDef
updateRequestDef base form =
  base
    &  name
    .~ (form ^. name)
    &  url
    .~ (form ^. url)
    &  method
    .~ (form ^. method)

makeEditRequestDefForm
  :: AppState -> RequestDefContext -> ZZZForm RequestDefFormState
makeEditRequestDefForm s c =
  let r         = model s c
      editState = RequestDefFormState { requestDefFormStateName   = r ^. name
                                      , requestDefFormStateUrl    = r ^. url
                                      , requestDefFormStateMethod = r ^. method
                                      }
  in  newForm
        [ (txt "Name:   " <+>)
          @@= editTextField (name . coerced) RequestDefFormNameField (Just 1)
        , (txt "URL:    " <+>)
          @@= editField (url . coerced)
                        RequestDefFormUrlField
                        (Just 1)
                        coerce
                        validateUrl
                        renderText
                        id
        , (txt "Method: " <+>) @@= radioField method allMethodsRadio
        ]
        editState

showEditRequestDefScreen :: Monad m => RequestDefContext -> StateT AppState m ()
showEditRequestDefScreen c = modify
  $ \s -> s & screen .~ RequestDefEditScreen c (makeEditRequestDefForm s c)

updateEditRequestDefForm
  :: ZZZForm RequestDefFormState
  -> BrickEvent Name CustomEvent
  -> StateT AppState (EventM Name) ()
updateEditRequestDefForm form ev = do
  updatedForm <- lift $ handleFormEvent ev form
  modify
    $  screen
    .  _RequestDefEditScreen
    .  typed @(ZZZForm RequestDefFormState)
    .~ updatedForm
