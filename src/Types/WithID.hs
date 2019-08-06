{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Types.WithID where

import           Types.AppState
import           Types.ID
import           Types.Project
import           Types.RequestDefinition

class HasID a where
  -- All these type families are injective, so that, for example, only one type can have an ID of type `ProjectID`
  -- (in this case, `Project`)
  type family ID a = b | b -> a
  type family Context a = b | b -> a   -- The set of IDs necessary to traverse the state and obtain the specified `a`

  model :: AppState -> Context a -> a

instance HasID Project where
  type ID Project = ProjectID
  type Context Project = ProjectContext
  model = lookupProject

instance HasID RequestDefinition where
  type ID RequestDefinition = RequestDefinitionID
  type Context RequestDefinition = RequestDefinitionContext
  model = lookupRequestDefinition
