{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module UI.Search.SearchScreen
  ( handleEventSearch,
    searchWidget,
    showSearchScreen,
    searchEditor,
  )
where

import Brick.Widgets.List
import Control.Lens
import Data.Coerce (coerce)
import Data.Sequence (Seq)
import Types.AppState
import Types.Classes.Fields
import Types.Models.Environment
import Types.Models.Id (ProjectId)
import Types.Models.Project
import Types.Models.RequestDef
import Types.Models.Screen
import Types.Search
import UI.List
  ( AppList (..),
  )
import UI.Search.Common
import UI.Search.Editor

requestDefResults :: (ProjectId, Project) -> Seq SearchResult
requestDefResults (pid, p) =
  let sortedRds = sortModels (p ^. requestDefs)
   in fmap
        (\(rid, r) -> RequestDefResult (RequestDefContext pid rid) (p ^. name) (r ^. name))
        sortedRds

allSearchResults :: AppState a -> SearchCandidates
allSearchResults s =
  let sortedEnvs = sortModels (s ^. environments)
      envResults =
        NoEnvironmentResult
          <| fmap (\(eid, e) -> AnEnvironmentResult (EnvironmentContext eid) (e ^. name)) sortedEnvs
      sortedProjects = sortModels (s ^. projects)
      projectResults =
        fmap (\(pid, p) -> ProjectResult (ProjectContext pid) (p ^. name)) sortedProjects
      rdResults :: Seq SearchResult
      rdResults = sortedProjects >>= requestDefResults
   in AllCandidateSets (envResults, projectResults, rdResults)

showSearchScreen :: AppState a -> AppState 'SearchTag
showSearchScreen s =
  let -- Note: the sequence of SearchResults (as opposed to the list) is fixed and will not
      -- be narrowed down. It's used for filtering (see note in `handleSearchEvent` for more).
      results = allSearchResults s
      AppList zl = makeResultList results
   in s & screen .~ SearchScreen (SearchTools searchEditor (coerce (listMoveDown zl)) results)
