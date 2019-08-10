{-# LANGUAGE TypeFamilyDependencies #-}

module Types.Classes.HasId where

import           Types.AppState
import           Types.Models.Id
import           Types.Models.Project
import           Types.Models.RequestDefinition

class HasId a where
  -- All these type families are injective, so that, for example, only one type can have an ID of type `ProjectId`
  -- (in this case, `Project`)
  type family ID a = b | b -> a
  type family Context a = b | b -> a   -- The set of IDs necessary to traverse the state and obtain the specified `a`

  model :: AppState -> Context a -> a

instance HasId Project where
  type ID Project = ProjectId
  type Context Project = ProjectContext
  model = lookupProject

instance HasId RequestDefinition where
  type ID RequestDefinition = RequestDefinitionId
  type Context RequestDefinition = RequestDefinitionContext
  model = lookupRequestDefinition
