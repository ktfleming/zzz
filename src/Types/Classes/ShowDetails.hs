{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Classes.ShowDetails where

import           Types.Models.RequestDefinition ( RequestDefinition
                                                , RequestDefinitionContext
                                                , RequestDefinitionListItem(..)
                                                , RequestDefinitionListItem
                                                )
import           Types.Classes.WithID

import           Brick.Widgets.List             ( list )
import qualified Data.Map.Strict               as Map
import           Data.Vector                    ( fromList )
import           Lens.Micro.Platform            ( (.~) )
import           Types.AppState
import           Types.ContextTransformers
import           Types.Brick.Name
import           Types.Models.Project
import           Types.Models.Screen


-- Types in this typeclass are able to have their details shown to the
-- user, usually by updating the AppState's active Screen to be the
-- details Screen for the given value of the type
class HasID a => ShowDetails a where
  -- here the Context uniquely identifies the model being selected
  showDetails :: AppState -> Context a -> AppState


instance ShowDetails Project where
  showDetails :: AppState -> ProjectContext -> AppState
  showDetails s c =
    let Project { _requestDefinitions }          = model s c
        listItems :: [RequestDefinitionListItem] = foldr
          f
          []
          (Map.toList _requestDefinitions)
           where
            f (rid, r) items =
              let rc = requestDefinitionContext c rid
              in  items ++ [requestDefinitionListItem rc r]
        reqList = list RequestDefinitionList (fromList listItems) 1
    in  (activeScreen .~ ProjectDetailsScreen c reqList) s

instance ShowDetails RequestDefinition where
  showDetails :: AppState -> RequestDefinitionContext -> AppState
  showDetails s c = (activeScreen .~ RequestDetailsScreen c) s
