{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.RequestDefs.Edit where

import           Brick                          ( Widget
                                                , emptyWidget
                                                , txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( FormFieldState
                                                , editTextField
                                                , formState
                                                , newForm
                                                , radioField
                                                , setFormConcat
                                                , (@@=)
                                                )
import           Control.Lens
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Maybe                     ( maybe )
import           Data.Sequence                  ( Seq )
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Safe                           ( headMay )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Methods                  ( allMethodsRadio )
import           Types.Models.Environment       ( Variable )
import           Types.Models.Project           ( requestDefs )
import           Types.Models.RequestDef
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.Form                        ( ZZZForm
                                                , renderText
                                                , spacedConcat
                                                )
import           UI.Forms.FocusAwareEditor      ( focusAwareEditField )
import           UI.Forms.KeyValueList          ( makeKeyValueForm )
import           UI.Forms.RequestBody           ( requestBodyForm )
import           UI.Url                         ( colorizedUrl )

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

urlField
  :: Seq Variable -> RequestDefFormState -> FormFieldState RequestDefFormState CustomEvent Name
urlField vars s =
  let validate :: [T.Text] -> Maybe Url
      validate = headMay >=> Just . Url

      readOnlyRender :: [T.Text] -> Widget Name
      readOnlyRender ts = maybe emptyWidget (colorizedUrl vars) (Url <$> headMay ts)
  in  focusAwareEditField url
                          RequestDefFormUrlField
                          (Just 1)
                          coerce
                          validate
                          renderText
                          id
                          (Just readOnlyRender)
                          s

makeEditRequestDefForm :: AppState a -> RequestDefContext -> ZZZForm RequestDefFormState
makeEditRequestDefForm s c =
  let r         = model s c
      editState = RequestDefFormState { requestDefFormStateName    = r ^. name
                                      , requestDefFormStateUrl     = r ^. url
                                      , requestDefFormStateMethod  = r ^. method
                                      , requestDefFormStateBody    = r ^. body
                                      , requestDefFormStateHeaders = r ^. headers
                                      }
  in  setFormConcat spacedConcat $ newForm
        [ (txt "Name:     " <+>) @@= editTextField (name . coerced) RequestDefFormNameField (Just 1)
        , (txt "URL:      " <+>) @@= urlField (currentVariables s)
        , (txt "Method:   " <+>) @@= radioField method allMethodsRadio
        , (txt "Body      " <+>) @@= requestBodyForm
        , (txt "Headers:  " <+>) @@= makeKeyValueForm headers HeadersField
        ]
        editState

showEditRequestDefScreen
  :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState 'RequestDefEditTag) ()
showEditRequestDefScreen c =
  imodify $ \s -> s & screen .~ RequestDefEditScreen c (makeEditRequestDefForm s c)
