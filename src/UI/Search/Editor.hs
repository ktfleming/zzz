{-# LANGUAGE OverloadedStrings #-}

module UI.Search.Editor where

import Brick.Widgets.Edit
import Types.Brick.Name
import UI.Editor (ZZZEditor (..))

searchEditor :: ZZZEditor
searchEditor = ZZZEditor $ editorText SearchField (Just 1) ""
