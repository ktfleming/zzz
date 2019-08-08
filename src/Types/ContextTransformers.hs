{-# LANGUAGE NamedFieldPuns #-}

module Types.ContextTransformers where

import           Types.Models.ID                ( RequestDefinitionID )
import           Types.Models.Project
import           Types.Models.RequestDefinition

requestDefinitionContext
  :: ProjectContext -> RequestDefinitionID -> RequestDefinitionContext
requestDefinitionContext (ProjectContext pid) = RequestDefinitionContext pid

requestDefinitionListItem
  :: RequestDefinitionContext -> RequestDefinition -> RequestDefinitionListItem
requestDefinitionListItem c RequestDefinition { _requestDefinitionName } =
  RequestDefinitionListItem c _requestDefinitionName
