module Types.Modal where

import Types.Models.Environment (EnvironmentContext)
import Types.Models.Project (ProjectContext)
import Types.Models.RequestDef (RequestDefContext)
import Types.Models.Response (ResponseIndex)

data Modal
  = DeleteProjectModal ProjectContext
  | DeleteRequestDefModal RequestDefContext
  | DeleteEnvironmentModal EnvironmentContext
  | DeleteResponseModal RequestDefContext ResponseIndex
  deriving (Show, Eq)
