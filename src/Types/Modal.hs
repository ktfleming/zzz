module Types.Modal where

import           Types.Models.Project           ( ProjectContext )
import           Types.Models.RequestDefinition ( RequestDefinitionContext )

data Modal =
    DeleteProjectModal ProjectContext
  | DeleteRequestDefinitionModal RequestDefinitionContext
    deriving (Show)
