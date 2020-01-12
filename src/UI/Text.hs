{-# LANGUAGE OverloadedStrings #-}

module UI.Text where

import Brick
import Brick.Widgets.Center (hCenter)
import Data.Text (Text)
import Types.Brick.Name (Name)

-- Displays a centered and highlighted "explanation" message
explanationWithAttr :: AttrName -> Text -> Widget Name
explanationWithAttr attr t =
  let leftArrows = withAttr attr $ txt ">> "
      rightArrows = withAttr attr $ txt " <<"
      text = withAttr attr (txt t)
   in (padTop (Pad 1) . padBottom (Pad 1) . hCenter) (leftArrows <+> text <+> rightArrows)
