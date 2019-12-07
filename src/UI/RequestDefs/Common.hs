{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefs.Common
  ( makeRequestDefForm,
  )
where

import Brick
import Brick.Forms
import Control.Lens
import Data.Coerce (coerce)
import Data.Sequence (Seq)
import qualified Data.Text as T
import Safe (headMay)
import Types.Brick.CustomEvent (CustomEvent)
import Types.Brick.Name
import Types.Classes.Fields
import Types.Methods
import Types.Models.Environment (Variable)
import Types.Models.RequestDef
import Types.Models.Url
import UI.Form
import UI.Forms.FocusAwareEditor (focusAwareEditField)
import UI.Forms.KeyValueList (makeKeyValueForm)
import UI.Forms.RequestBody (requestBodyForm)
import UI.Url (colorizedUrl)

-- The forms for adding and editing are the same
makeRequestDefForm :: Seq Variable -> RequestDefFormState a -> AppForm (RequestDefFormState a)
makeRequestDefForm vars fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [ (txt "Name:     " <+>) @@= nonEmptyTextField (name . coerced) RequestDefFormNameField,
        (txt "URL:      " <+>) @@= urlField vars,
        (txt "Method:   " <+>) @@= radioField method allMethodsRadio,
        (txt "Body      " <+>) @@= requestBodyForm,
        (txt "Headers:  " <+>) @@= makeKeyValueForm headers HeadersField
      ]
      fs

urlField ::
  Seq Variable ->
  RequestDefFormState a ->
  FormFieldState (RequestDefFormState a) CustomEvent Name
urlField vars s =
  let validate :: [T.Text] -> Maybe Url
      validate ts = headMay ts >>= \u -> if T.null u then Nothing else Just (Url u)
      readOnlyRender :: [T.Text] -> Widget Name
      readOnlyRender ts = maybe emptyWidget (colorizedUrl vars) (Url <$> headMay ts)
   in focusAwareEditField
        url
        RequestDefFormUrlField
        (Just 1)
        coerce
        validate
        renderText
        id
        (Just readOnlyRender)
        s
