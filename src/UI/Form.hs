{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.Form where

import Brick
import Brick.Forms
import Brick.Widgets.Center (hCenter)
import Control.Lens (Lens')
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Safe (headMay)
import Types.Brick.CustomEvent
import Types.Brick.Name
import UI.Attr

newtype AppForm a = AppForm (Form a CustomEvent Name)

instance Eq a => Eq (AppForm a) where
  -- just compare form states for now
  (AppForm form1) == (AppForm form2) = formState form1 == formState form2

instance Show a => Show (AppForm a) where
  show (AppForm f) = show $ formState f

-- Just a simple way to render multi-line Text as a widget,
-- taken from Brick's code
renderText :: [Text] -> Widget Name
renderText = txt . T.intercalate "\n"

-- Custom concat function for rendering forms so that each field
-- has a blank line after it
spacedConcat :: [Widget Name] -> Widget Name
spacedConcat = vBox . fmap (padBottom (Pad 1))

-- Text field with an included validation function that ensures the text is non-empty
nonEmptyTextField :: Lens' a Text -> Name -> a -> FormFieldState a CustomEvent Name
nonEmptyTextField lens name s =
  let validate :: [Text] -> Maybe T.Text
      validate ts = headMay ts >>= \h -> if T.null h then Nothing else Just h
   in editField
        lens
        name
        (Just 1)
        id
        validate
        renderText
        id
        s

renderAppForm :: Form s e Name -> Widget Name
renderAppForm form =
  let errorMessage =
        if allFieldsValid form
          then emptyWidget
          else
            let fields = T.intercalate ", " . mapMaybe label $ invalidFields form
                msg = "The form contains invalid fields: " <> fields
             in padBottom (Pad 1) . withAttr errorAttr . hCenter $ txt msg
   in errorMessage <=> renderForm form
