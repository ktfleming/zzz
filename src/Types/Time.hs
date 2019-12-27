module Types.Time where

import Data.Time

-- newtyped so I can make a non-orphan Eq instance
newtype AppTime = AppTime ZonedTime deriving (Show)

instance Eq AppTime where
  (AppTime z1) == (AppTime z2) = zonedTimeToLocalTime z1 == zonedTimeToLocalTime z2
