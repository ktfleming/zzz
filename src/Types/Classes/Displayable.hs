module Types.Classes.Displayable where

import Brick (Widget)
import Types.Brick.Name (Name)

class Displayable a where
  display :: a -> Widget Name
