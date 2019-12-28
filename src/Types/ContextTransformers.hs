module Types.ContextTransformers where

import Types.Models.Id (RequestDefId)
import Types.Models.Project (ProjectContext (..))
import Types.Models.RequestDef

requestDefContext :: ProjectContext -> RequestDefId -> RequestDefContext
requestDefContext (ProjectContext pid) = RequestDefContext pid
