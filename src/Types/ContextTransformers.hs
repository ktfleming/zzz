{-# LANGUAGE NamedFieldPuns #-}

module Types.ContextTransformers where

import           Types.Project
import           Types.RequestDefinition

projectContext :: Project -> ProjectContext
projectContext Project { projectID } = ProjectContext projectID

projectListItem :: Project -> ProjectListItem
projectListItem p@Project { _projectName } =
  ProjectListItem (projectContext p) _projectName

requestDefinitionContext
  :: ProjectContext -> RequestDefinition -> RequestDefinitionContext
requestDefinitionContext (ProjectContext pid) RequestDefinition { requestDefinitionID }
  = RequestDefinitionContext pid requestDefinitionID

requestDefinitionListItem
  :: RequestDefinitionContext -> RequestDefinition -> RequestDefinitionListItem
requestDefinitionListItem c RequestDefinition { _requestDefinitionName } =
  RequestDefinitionListItem c _requestDefinitionName
