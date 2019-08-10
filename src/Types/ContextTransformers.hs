module Types.ContextTransformers where

import           Control.Lens
import           Types.Models.ID                ( RequestDefinitionID )
import           Types.Models.Project
import           Types.Models.RequestDefinition

requestDefinitionContext
  :: ProjectContext -> RequestDefinitionID -> RequestDefinitionContext
requestDefinitionContext (ProjectContext pid) = RequestDefinitionContext pid

requestDefinitionListItem
  :: RequestDefinitionContext -> RequestDefinition -> RequestDefinitionListItem
requestDefinitionListItem c r = RequestDefinitionListItem c (r ^. name)
