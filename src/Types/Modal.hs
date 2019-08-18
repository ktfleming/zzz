module Types.Modal where

import           Types.Models.Project           ( ProjectContext )
import           Types.Models.RequestDef        ( RequestDefContext )

data Modal =
    DeleteProjectModal ProjectContext
  | DeleteRequestDefModal RequestDefContext
    deriving (Show)
