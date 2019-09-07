{-# LANGUAGE TypeFamilyDependencies #-}

module Types.Classes.HasId where

import           Types.AppState
import           Types.Models.Environment       ( Environment
                                                , EnvironmentContext
                                                )
import           Types.Models.Id
import           Types.Models.Project
import           Types.Models.RequestDef

class HasId a where
  -- All these type families are injective, so that, for example, only one type can have an ID of type `ProjectId`
  -- (in this case, `Project`)
  type family ID a = b | b -> a
  type family Context a = b | b -> a   -- The set of IDs necessary to traverse the state and obtain the specified `a`

  model :: AppState x -> Context a -> a

instance HasId Project where
  type ID Project = ProjectId
  type Context Project = ProjectContext
  model = lookupProject

instance HasId RequestDef where
  type ID RequestDef = RequestDefId
  type Context RequestDef = RequestDefContext
  model = lookupRequestDef

instance HasId Environment where
  type ID Environment = EnvironmentId
  type Context Environment = EnvironmentContext
  model = lookupEnvironment
