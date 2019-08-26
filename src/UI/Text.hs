module UI.Text where

import           Brick                          ( Widget
                                                , txt
                                                , withAttr
                                                )

import           Types.Brick.Name               ( Name )
import           Types.Classes.Displayable      ( display )
import           Types.Methods                  ( Method )
import           UI.Attr                        ( methodAttr )

methodWidget :: Method -> Widget Name
methodWidget m = withAttr methodAttr $ txt (display m)
