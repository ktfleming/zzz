{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.RequestDefs.Edit where

import           Brick                          ( txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editField
                                                , editTextField
                                                , formState
                                                , newForm
                                                , radioField
                                                , setFormConcat
                                                , (@@=)
                                                )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Data.Coerce                    ( coerce )
import           Data.String                    ( fromString )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Methods                  ( allMethodsRadio )
import           Types.Models.Project           ( requestDefs )
import           Types.Models.RequestDef
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..)
                                                , validateUrl
                                                )
import           UI.Form                        ( ZZZForm
                                                , renderText
                                                , spacedConcat
                                                )
import           UI.Forms.Headers               ( makeHeadersForm )
import           UI.Forms.RequestBody           ( requestBodyForm )

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
  base
    &  name
    .~ (form ^. name)
    &  url
    .~ (form ^. url)
    &  method
    .~ (form ^. method)
    &  body
    .~ (form ^. body)
    &  headers
    .~ (form ^. headers)

makeEditRequestDefForm :: AppState a -> RequestDefContext -> ZZZForm RequestDefFormState
makeEditRequestDefForm s c =
  let
    r         = model s c
    editState = RequestDefFormState { requestDefFormStateName    = r ^. name
                                    , requestDefFormStateUrl     = r ^. url
                                    , requestDefFormStateMethod  = r ^. method
                                    , requestDefFormStateBody    = r ^. body
                                    , requestDefFormStateHeaders = r ^. headers
                                    }
  in
    setFormConcat spacedConcat $ newForm
      [ (txt "Name:     " <+>) @@= editTextField (name . coerced) RequestDefFormNameField (Just 1)
      , (txt "URL:      " <+>)
        @@= editField (url . coerced)
                      RequestDefFormUrlField
                      (Just 1)
                      coerce
                      validateUrl
                      renderText
                      id
      , (txt "Method:   " <+>) @@= radioField method allMethodsRadio
      , (txt "Body      " <+>) @@= requestBodyForm
      , (txt "Headers:  " <+>) @@= makeHeadersForm
      ]
      editState

showEditRequestDefScreen
  :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState 'RequestDefEditTag) ()
showEditRequestDefScreen c =
  imodify $ \s -> s & screen .~ RequestDefEditScreen c (makeEditRequestDefForm s c)
