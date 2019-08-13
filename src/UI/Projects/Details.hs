module UI.Projects.Details where

import           Brick.Widgets.List             ( list )
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.HasId            ( model )
import           Types.ContextTransformers      ( requestDefinitionContext
                                                , requestDefinitionListItem
                                                )
import           Types.Models.Id                ( RequestDefinitionId )
import           Types.Models.Project
import           Types.Models.RequestDefinition ( RequestDefinition
                                                , RequestDefinitionListItem
                                                )
import           Types.Models.Screen

showProjectDetails :: AppState -> ProjectContext -> AppState
showProjectDetails s c =
  let p         = model s c
      listItems = foldr f S.empty (Map.toList (p ^. requestDefinitions))
         where
          f
            :: (RequestDefinitionId, RequestDefinition)
            -> Seq RequestDefinitionListItem
            -> Seq RequestDefinitionListItem
          f (rid, r) items =
            let rc = requestDefinitionContext c rid
            in  items |> requestDefinitionListItem rc r
      reqList = list RequestDefinitionList listItems 1
  in  (screen .~ ProjectDetailsScreen c reqList) s
