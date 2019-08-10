{-# LANGUAGE OverloadedStrings #-}

module UI.Form where

import           Brick                          ( Widget
                                                , txt
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
