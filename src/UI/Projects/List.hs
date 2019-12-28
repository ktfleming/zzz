{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Projects.List
  ( showProjectListScreen,
    projectListSearchTools,
  )
where

import Brick.Widgets.List (list)
import Control.Lens
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Types.AppState
import Types.Brick.Name
import Types.Classes.Fields
import Types.Models.Id (ProjectId)
import Types.Models.Project
import Types.Models.Screen
import Types.Search
import UI.List (AppList (..))
import UI.Search.Editor

listItems :: HashMap ProjectId Project -> Seq SearchResult
listItems pm =
  foldr
    (\(pid, p) items -> items |> ProjectResult (ProjectContext pid) (p ^. name))
    Seq.empty
    (Map.toList pm)

projectListSearchTools :: HashMap ProjectId Project -> SearchTools
projectListSearchTools pm =
  let items = listItems pm
      cands = OneCandidateSet items
      wrappedItems = SelectableResult <$> items
   in SearchTools searchEditor (AppList $ list ProjectList wrappedItems 1) cands

showProjectListScreen :: AppState a -> AppState 'ProjectListTag
showProjectListScreen s = s & screen .~ ProjectListScreen (projectListSearchTools (s ^. projects))
