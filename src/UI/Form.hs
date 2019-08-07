module UI.Form where

import           Brick.Forms       (Form)
import           Types.Brick.CustomEvent
import           Types.Brick.Name

type ZZZForm a = Form a CustomEvent Name
