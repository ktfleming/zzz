module UI.Editor where

import Brick.Widgets.Edit
  ( Editor,
    getEditContents,
  )
import Data.Text (Text)
import Types.Brick.Name (Name)

newtype ZZZEditor = ZZZEditor (Editor Text Name) deriving (Show)

instance Eq ZZZEditor where
  (ZZZEditor e1) == (ZZZEditor e2) = getEditContents e1 == getEditContents e2
