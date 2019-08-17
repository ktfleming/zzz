{-# LANGUAGE TypeApplications #-}

module UI.Projects.Details where

import           Brick                          ( EventM )
import           Brick.Widgets.List             ( handleListEvent
                                                , list
                                                )
import           Control.Lens
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State      ( StateT
                                                , get
                                                , modify
                                                , put
                                                )
import           Data.Generics.Product.Typed    ( typed )
import qualified Data.HashMap.Strict           as Map
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Graphics.Vty                   ( Event(EvKey)
                                                , Key
                                                )
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
import           UI.List                        ( ZZZList )

showProjectDetails :: Monad m => ProjectContext -> StateT AppState m ()
showProjectDetails c = do
  s <- get
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
  put $ s & screen .~ ProjectDetailsScreen c reqList

updateProjectDetailsList
  :: ZZZList RequestDefinitionListItem
  -> Key
  -> StateT AppState (EventM Name) ()
updateProjectDetailsList l key = do
  updatedList <- lift $ handleListEvent (EvKey key []) l
  modify
    $  screen
    .  _ProjectDetailsScreen
    .  typed @(ZZZList RequestDefinitionListItem)
    .~ updatedList
