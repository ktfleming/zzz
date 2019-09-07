module Types.Modal where

import           Types.Models.Environment       ( EnvironmentContext )
import           Types.Models.Project           ( ProjectContext )
import           Types.Models.RequestDef        ( RequestDefContext )

data Modal =
    DeleteProjectModal ProjectContext
  | DeleteRequestDefModal RequestDefContext
  | DeleteEnvironmentModal EnvironmentContext
    deriving (Show)
