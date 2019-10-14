module UI.Editor where

import Brick.Widgets.Edit
  ( Editor,
    getEditContents,
  )
import qualified Data.Text as T
import Types.Brick.Name (Name)

newtype ZZZEditor = ZZZEditor (Editor T.Text Name) deriving (Show)

instance Eq ZZZEditor where
  (ZZZEditor e1) == (ZZZEditor e2) = getEditContents e1 == getEditContents e2
