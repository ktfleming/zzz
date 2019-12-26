module Types.Classes.Displayable where

import Brick (Widget)
import Types.Brick.Name (Name)

-- A type that's displayable as a Brick widget
class Displayable a where
  display :: a -> Widget Name
