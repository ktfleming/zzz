{-# LANGUAGE DataKinds #-}

module UI.Projects.Details
  ( projectDetailsSearchTools,
    showProjectDetails,
  )
where

import Brick.Widgets.List (list)
import Control.Lens
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Types.AppState
import Types.Brick.Name
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.ContextTransformers
  ( requestDefContext,
  )
import Types.Models.Id (RequestDefId)
import Types.Models.Project
import Types.Models.RequestDef
  ( RequestDef,
  )
import Types.Models.Screen
import Types.Search
import UI.List (AppList (..))
import UI.Search.Editor

listItems :: ProjectContext -> Project -> Seq SearchResult
listItems c p =
  foldr f Seq.empty (Map.toList (p ^. requestDefs))
  where
    f :: (RequestDefId, RequestDef) -> Seq SearchResult -> Seq SearchResult
    f (rid, r) items = let rc = requestDefContext c rid in items |> RequestDefResult rc (p ^. name) (r ^. name)

projectDetailsSearchTools :: ProjectContext -> Project -> SearchTools
projectDetailsSearchTools c p =
  let items = listItems c p
      wrappedItems = SelectableResult <$> items
      cands = OneCandidateSet items
   in SearchTools searchEditor (AppList $ list RequestDefList wrappedItems 1) cands

showProjectDetails :: ProjectContext -> AppState a -> AppState 'ProjectDetailsTag
showProjectDetails c s =
  let p = model s c
   in s & screen .~ ProjectDetailsScreen c (projectDetailsSearchTools c p)
