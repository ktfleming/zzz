{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module UI.ShowDetails where

import           Types.WithID
import           Types.RequestDefinition        ( RequestDefinition
                                                , RequestDefinitionContext
                                                , RequestDefinitionListItem
                                                )

import           Types.Project
import qualified Data.Map.Strict               as Map
import           Types.Name
import           Brick.Widgets.List             ( list )
import           Data.Vector                    ( fromList )
import           Types.AppState
import           Lens.Micro.Platform            ( (.~) )
import           Types.Screen
import           Types.ContextTransformers


-- Types in this typeclass are able to have their details shown to the
-- user, usually by updating the AppState's active Screen to be the
-- details Screen for the given value of the type
class HasID a => ShowDetails a where
  -- here the Context uniquely identifies the model being selected
  showDetails :: AppState -> Context a -> AppState


instance ShowDetails Project where
  showDetails :: AppState -> ProjectContext -> AppState
  showDetails s c =
    let Project { _requestDefinitions } = model s c
        rds = fromList $ Map.elems _requestDefinitions
        fn :: RequestDefinition -> RequestDefinitionListItem
        fn r =
            let rc :: RequestDefinitionContext = requestDefinitionContext c r
            in  requestDefinitionListItem rc r
        reqList = list RequestDefinitionList (fmap fn rds) 1
    in  (activeScreen .~ ProjectDetailsScreen c reqList) s

instance ShowDetails RequestDefinition where
  showDetails :: AppState -> RequestDefinitionContext -> AppState
  showDetails s c = (activeScreen .~ RequestDetailsScreen c) s
