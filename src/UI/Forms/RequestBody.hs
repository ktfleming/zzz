{-# LANGUAGE OverloadedStrings #-}

module UI.Forms.RequestBody where

import Brick
  ( Widget,
    vLimit,
    withBorderStyle,
  )
import Brick.Forms (FormFieldState (..))
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.Maybe (fromMaybe)
import Data.Text as Text
import Types.Brick.CustomEvent (CustomEvent)
import Types.Brick.Name (Name (..))
import Types.Classes.Fields
import Types.Models.RequestDef
import UI.Form (renderText)
import UI.Forms.FocusAwareEditor (focusAwareEditField)
import UI.Json (readOnlyJson)
import Utils.Text (tryPretty)

requestBodyForm :: RequestDefFormState -> FormFieldState RequestDefFormState CustomEvent Name
requestBodyForm s =
  let initFn :: RequestBody -> Text
      initFn (RequestBody t) = fromMaybe t (tryPretty t)
      validate :: [Text] -> Maybe RequestBody
      validate = Just . RequestBody . Text.intercalate "\n"
      readOnlyRender :: [Text] -> Widget Name
      readOnlyRender = vLimit 20 . readOnlyJson . Text.intercalate "\n"
      augment :: Widget Name -> Widget Name
      augment = vLimit 20 . withBorderStyle unicodeRounded . border
   in focusAwareEditField
        body
        RequestBodyField
        Nothing
        initFn
        validate
        renderText
        augment
        (Just readOnlyRender)
        s
