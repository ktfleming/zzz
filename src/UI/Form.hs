{-# LANGUAGE OverloadedStrings #-}

module UI.Form where

import           Brick                          ( Padding(Pad)
                                                , Widget
                                                , padBottom
                                                , txt
                                                , vBox
                                                )
import           Brick.Forms                    ( Form )
import qualified Data.Text                     as T
import           Types.Brick.CustomEvent
import           Types.Brick.Name

type ZZZForm a = Form a CustomEvent Name

-- Just a simple way to render multi-line Text as a widget,
-- taken from Brick's code
renderText :: [T.Text] -> Widget Name
renderText = txt . T.intercalate "\n"

-- Custom concat function for rendering forms so that each field
-- has a blank line after it
spacedConcat :: [Widget Name] -> Widget Name
spacedConcat ws = vBox $ padBottom (Pad 1) <$> ws
