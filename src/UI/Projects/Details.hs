{-# LANGUAGE NamedFieldPuns #-}
module UI.Projects.Details where

import           Brick.Widgets.List             ( list )
import           Control.Lens
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.WithID           ( model )
import           Types.ContextTransformers      ( requestDefinitionContext
                                                , requestDefinitionListItem
                                                )
import           Types.Models.Project           ( Project(..)
                                                , ProjectContext
                                                )
import           Types.Models.Screen

showProjectDetails :: AppState -> ProjectContext -> AppState
showProjectDetails s c =
  let Project { _requestDefinitions } = model s c
      listItems = foldr f [] (Map.toList _requestDefinitions)
         where
          f (rid, r) items =
            let rc = requestDefinitionContext c rid
            in  items ++ [requestDefinitionListItem rc r]
      reqList = list RequestDefinitionList (V.fromList listItems) 1
  in  (activeScreen .~ ProjectDetailsScreen c reqList) s
