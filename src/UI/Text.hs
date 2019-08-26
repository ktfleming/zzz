module UI.Text where

import Brick (Widget, withAttr, txt)

import Types.Brick.Name (Name)
import Types.Methods (Method)
import UI.Attr (methodAttr)
import Types.Classes.Displayable (display)

methodWidget :: Method -> Widget Name
methodWidget m = withAttr methodAttr $ txt (display m)
