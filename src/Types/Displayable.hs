module Types.Displayable where

import qualified Data.Text                     as T

class Displayable a where
  display :: a -> T.Text
