module Types.ContextTransformers where

import           Control.Lens
import           Types.Models.Id                ( RequestDefinitionId )
import           Types.Models.Project
import           Types.Models.RequestDefinition

requestDefinitionContext
  :: ProjectContext -> RequestDefinitionId -> RequestDefinitionContext
requestDefinitionContext (ProjectContext pid) = RequestDefinitionContext pid

requestDefinitionListItem
  :: RequestDefinitionContext -> RequestDefinition -> RequestDefinitionListItem
requestDefinitionListItem c r = RequestDefinitionListItem c (r ^. name)
