{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Search
  ( handleEventSearch,
    searchWidget,
    showSearchScreen,
  )
where

import Brick
  ( (<=>),
    Size (Fixed),
    Widget (..),
    txt,
    withAttr,
  )
import Brick.BChan (BChan)
import Brick.Widgets.Edit
  ( editorText,
    getEditContents,
    handleEditorEvent,
    renderEditor,
  )
import Brick.Widgets.List
  ( list,
    listMoveDown,
    listMoveUp,
    listSelectedElement,
  )
import Control.Lens
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Graphics.Vty.Input.Events
  ( Event (..),
    Key (..),
    Modifier,
  )
import Safe (headMay)
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent)
import Types.Brick.Name
import Types.Classes.Fields
import Types.Classes.HasId
import Types.Models.Environment (EnvironmentContext (..))
import Types.Models.Id (ProjectId)
import Types.Models.Project
  ( Project,
    ProjectContext (..),
    requestDefs,
  )
import Types.Models.RequestDef (RequestDefContext (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
  ( listLens,
    updateBrickList,
  )
import Types.Monads
import Types.Search
  ( PartitionedResults,
    SearchListItem (..),
    SearchResult (..),
    filterResults,
  )
import UI.Attr (searchPlaceholderAttr)
import UI.Editor (ZZZEditor (..))
import UI.Events.Environments (selectEnvironment)
import UI.Form (renderText)
import UI.List
  ( AppList (..),
    renderGenericList,
  )
import UI.Projects.Details (showProjectDetails)
import UI.RequestDefs.Details (showRequestDefDetails)
import Utils.Containers (mapToSeq)

makeResultList :: PartitionedResults -> AppList SearchListItem
makeResultList (envs, ps, rds) =
  let makeSection :: T.Text -> Seq SearchResult -> Bool -> Seq SearchListItem
      makeSection header results addBottomPadding
        | Seq.null results =
          Seq.empty
        | otherwise =
          (SearchSection header <| (SelectableResult <$> results))
            <> (if addBottomPadding then Seq.singleton SearchBlankLine else Seq.empty)
      envSection = makeSection "Environments" envs True
      projectSection = makeSection "Projects" ps True
      rdSection = makeSection "Request Definitions" rds False
   in AppList $ list SearchResultsList (envSection <> projectSection <> rdSection) 1

searchWidget :: ZZZEditor -> AppList SearchListItem -> Widget Name
searchWidget (ZZZEditor edt) results =
  let fieldWidget =
        if null (getEditContents edt)
          then withAttr searchPlaceholderAttr $ txt "Enter text to start searching."
          else renderEditor renderText False edt
      allWidgets = fieldWidget <=> renderGenericList True True results
   in Widget Fixed Fixed $ render allWidgets

-- Sort a sequence of named ID/model pairs by the model names
sortModels :: (Ord b, HasName a b) => HashMap (ID a) a -> Seq (ID a, a)
sortModels = Seq.sortOn (view name . snd) . mapToSeq

requestDefResults :: (ProjectId, Project) -> Seq SearchResult
requestDefResults (pid, p) =
  let sortedRds = sortModels (p ^. requestDefs)
   in fmap
        (\(rid, r) -> RequestDefResult (p ^. name) (r ^. name) (RequestDefContext pid rid))
        sortedRds

allSearchResults :: AppState a -> PartitionedResults
allSearchResults s =
  let sortedEnvs = sortModels (s ^. environments)
      envResults =
        fmap (\(eid, e) -> EnvironmentResult (e ^. name) (EnvironmentContext eid)) sortedEnvs
      sortedProjects = sortModels (s ^. projects)
      projectResults =
        fmap (\(pid, p) -> ProjectResult (p ^. name) (ProjectContext pid)) sortedProjects
      rdResults :: Seq SearchResult
      rdResults = sortedProjects >>= requestDefResults
   in (envResults, projectResults, rdResults)

showSearchScreen :: AppState a -> AppState 'SearchTag
showSearchScreen s =
  let edt = ZZZEditor $ editorText SearchField (Just 1) ""
      -- Note: the sequence of SearchResults (as opposed to the list) is fixed and will not
      -- be narrowed down. It's used for filtering (see note in `handleSearchEvent` for more).
      results = allSearchResults s
      AppList zl = makeResultList results
   in s & screen .~ SearchScreen edt (coerce (listMoveDown zl)) results

searchSelect ::
  MonadIO m =>
  SearchResult ->
  BChan CustomEvent ->
  AppState 'SearchTag ->
  m AnyAppState
searchSelect (ProjectResult _ c) _ = pure . wrap . showProjectDetails c
searchSelect (RequestDefResult _ _ c) _ = pure . wrap . showRequestDefDetails c
searchSelect (EnvironmentResult _ c) chan = selectEnvironment (Just c) chan

data Direction = Up | Down -- needed to know which direction to skip past section headers

-- When moving the selection up or down, if the item to be selected is actually a section header (or blank line),
-- then automatically scroll past it to the following item, since section headers can't be actioned
-- and thus there's no reason to allow them to be selected. Note that the first item in the list will
-- always be a section header, so we also want to prevent moving up past the second item.
scrollPastSection :: Direction -> Screen 'SearchTag -> Screen 'SearchTag
scrollPastSection direction scr = do
  let (SearchScreen _ (AppList resultList) _) = scr
      listMoveFunction :: Direction -> AppList a -> AppList a
      listMoveFunction Up (AppList zl) = AppList $ listMoveUp zl
      listMoveFunction Down (AppList zl) = AppList $ listMoveDown zl
      other Up = Down
      other Down = Up
      skipPast = scr & listLens %~ listMoveFunction direction
  case listSelectedElement resultList of
    Just (0, _) -> scr & listLens %~ listMoveFunction (other direction) -- tried to select the first item; have to undo the move
    Just (_, SearchSection _) -> skipPast -- other section headers (besides the first) will be skipped over
    Just (_, SearchBlankLine) -> skipPast
    _ -> scr

-- Up and Down arrows move the selection
-- ENTER selects
-- All other keys are forwarded to the editor
handleEventSearch ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'SearchTag ->
  m AnyAppState
handleEventSearch key mods chan s = do
  let SearchScreen (ZZZEditor edt) (AppList resultList) allResults = s ^. screen
      forwardToList key' direction = do
        updatedScreen <- updateBrickList key' (s ^. screen)
        pure . wrap . (screen .~ scrollPastSection direction updatedScreen) $ s
  case key of
    KUp -> forwardToList key Up
    KDown -> forwardToList key Down
    KEnter -> case listSelectedElement resultList of
      Just (_, SelectableResult selected) -> searchSelect selected chan s
      _ -> pure . wrap $ s
    _ -> do
      updatedEditor <- liftEvent edt $ handleEditorEvent (EvKey key mods) edt
      let editContents = getEditContents updatedEditor
          searchString = fromMaybe "" (headMay editContents)
          -- Right now I'm always filtering on full list of all possible search results;
          -- it's possible to make this more efficient by, for example, only filtering
          -- on the already-filtered list of results if the new search string contains
          -- the previous search string as a prefix. Let's see how the naive version performs
          -- for now, and possibly return to this if it becomes a problem.
          updatedResults = filterResults searchString allResults
          AppList zl = makeResultList updatedResults
      -- Have to use listMoveDown to select the _second_ element in the results, since the first result (if there
      -- are any) will be the section header
      pure . wrap . (screen .~ SearchScreen (ZZZEditor updatedEditor) (AppList (listMoveDown zl)) allResults) $ s
