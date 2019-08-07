{-# LANGUAGE NamedFieldPuns #-}

module Types.ContextTransformers where

import           Types.Project
import           Types.RequestDefinition
import Types.ID (RequestDefinitionID, ProjectID)

projectListItem :: ProjectID -> Project -> ProjectListItem
projectListItem pid Project { _projectName } =
  ProjectListItem (ProjectContext pid) _projectName

requestDefinitionContext
  :: ProjectContext -> RequestDefinitionID -> RequestDefinitionContext
requestDefinitionContext (ProjectContext pid) = RequestDefinitionContext pid

requestDefinitionListItem
  :: RequestDefinitionContext -> RequestDefinition -> RequestDefinitionListItem
requestDefinitionListItem c RequestDefinition { _requestDefinitionName } =
  RequestDefinitionListItem c _requestDefinitionName
