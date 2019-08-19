{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.RequestDefs.Edit where

import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )

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

import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Data.String                    ( fromString )
import           Types.Brick.CustomEvent        ( CustomEvent )

finishEditingRequestDef
  :: Monad m => IxStateT m (AppState 'RequestDefEditTag) (AppState 'RequestDefEditTag) ()
finishEditingRequestDef = do
  s <- iget
  let RequestDefEditScreen c@(RequestDefContext pid rid) form = s ^. screen
      base     = model s c
      newModel = updateRequestDef base (formState form)
  imodify $ projects . ix pid . requestDefs . ix rid .~ newModel

updateRequestDef :: RequestDef -> RequestDefFormState -> RequestDef
updateRequestDef base form =
  base & name .~ (form ^. name) & url .~ (form ^. url) & method .~ (form ^. method)

makeEditRequestDefForm :: AppState a -> RequestDefContext -> ZZZForm RequestDefFormState
makeEditRequestDefForm s c =
  let
    r         = model s c
    editState = RequestDefFormState { requestDefFormStateName   = r ^. name
                                    , requestDefFormStateUrl    = r ^. url
                                    , requestDefFormStateMethod = r ^. method
                                    }
  in
    newForm
      [ (txt "Name:   " <+>) @@= editTextField (name . coerced) RequestDefFormNameField (Just 1)
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

showEditRequestDefScreen
  :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState 'RequestDefEditTag) ()
showEditRequestDefScreen c =
  imodify $ \s -> s & screen .~ RequestDefEditScreen c (makeEditRequestDefForm s c)

updateEditRequestDefForm
  :: BrickEvent Name CustomEvent
  -> IxStateT (EventM Name) (AppState 'RequestDefEditTag) (AppState 'RequestDefEditTag) ()
updateEditRequestDefForm ev = do
  s <- iget
  let RequestDefEditScreen c form = s ^. screen
  updatedForm <- ilift $ handleFormEvent ev form
  imodify $ screen .~ RequestDefEditScreen c updatedForm
