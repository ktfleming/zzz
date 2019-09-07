{-# LANGUAGE OverloadedStrings #-}

module UI.Text where

import           Brick                          ( AttrName
                                                , Padding(Pad)
                                                , Widget
                                                , padBottom
                                                , padTop
                                                , txt
                                                , withAttr
                                                , (<+>)
                                                )

import           Brick.Widgets.Center           ( hCenter )
import qualified Data.Text                     as T
import           Types.Brick.Name               ( Name )
import           Types.Classes.Displayable      ( display )
import           Types.Methods                  ( Method )
import           UI.Attr                        ( methodAttr )

-- Displays a method name (GET, POST, etc) in color
methodWidget :: Method -> Widget Name
methodWidget m = withAttr methodAttr $ txt (display m)

-- Displays a centered and highlighted "explanation" message
explanationWithAttr :: AttrName -> T.Text -> Widget Name
explanationWithAttr attr t =
  let leftArrows  = withAttr attr $ txt ">> "
      rightArrows = withAttr attr $ txt " <<"
      text        = withAttr attr (txt t)
  in  (padTop (Pad 1) . padBottom (Pad 1) . hCenter) (leftArrows <+> text <+> rightArrows)
