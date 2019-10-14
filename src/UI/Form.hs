{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Form where

import Brick
  ( Padding (Pad),
    Widget,
    padBottom,
    txt,
    vBox,
  )
import Brick.Forms
  ( Form,
    formState,
  )
import qualified Data.Text as T
import Types.Brick.CustomEvent
import Types.Brick.Name

newtype AppForm a = AppForm (Form a CustomEvent Name)

instance Eq a => Eq (AppForm a) where
  -- just compare form states for now
  (AppForm form1) == (AppForm form2) = formState form1 == formState form2

instance Show a => Show (AppForm a) where
  show (AppForm f) = show $ formState f

-- Just a simple way to render multi-line Text as a widget,
-- taken from Brick's code
renderText :: [T.Text] -> Widget Name
renderText = txt . T.intercalate "\n"

-- Custom concat function for rendering forms so that each field
-- has a blank line after it
spacedConcat :: [Widget Name] -> Widget Name
spacedConcat ws = vBox $ padBottom (Pad 1) <$> ws
