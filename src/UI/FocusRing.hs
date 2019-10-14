module UI.FocusRing where

import Brick.Focus
  ( FocusRing,
    focusGetCurrent,
  )

newtype AppFocusRing a = AppFocusRing (FocusRing a)

instance Show (AppFocusRing a) where
  show _ = "(FocusRing)"

instance Eq a => Eq (AppFocusRing a) where
  (AppFocusRing r1) == (AppFocusRing r2) = focusGetCurrent r1 == focusGetCurrent r2
