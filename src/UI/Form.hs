module UI.Form where

import           Brick.Forms       (Form)
import           Types.CustomEvent
import           Types.Name

type ZZZForm a = Form a CustomEvent Name
