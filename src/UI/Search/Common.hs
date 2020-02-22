{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Search.Common where

import Brick
  ( Widget (..),
  )
import Brick.BChan (BChan)
import Brick.Types
  ( Padding (Max),
    Size (Fixed),
  )
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Config
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Singletons
import qualified Data.Text as T
import Data.Text (Text)
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
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import Types.Search
import UI.Attr (searchPlaceholderAttr)
import UI.Editor (ZZZEditor (..))
import UI.Environments.List (selectEnvironment)
import UI.Events.Keys (matchKey)
import UI.Form (renderText)
import UI.List
  ( AppList (..),
  )
import UI.Projects.Details (showProjectDetails)
import UI.RequestDefs.Details (showRequestDefDetails)
import Utils.Containers (mapToSeq)

-- Sort a sequence of named ID/model pairs by the model names
sortModels :: (Ord b, HasName a b) => HashMap (ID a) a -> Seq (ID a, a)
sortModels = Seq.sortOn (view name . snd) . mapToSeq

-- Run the appropriate action when a given result is selected. Note that
-- selectEnvironment includes a call to unstashScreen, so it's not necessary
-- to add clearStash to that branch.
searchSelect ::
  (SingI a, MonadIO m) =>
  SearchResult ->
  BChan CustomEvent ->
  AppState a ->
  m AnyAppState
searchSelect (ProjectResult c _) _ = pure . wrap . clearStash . showProjectDetails c
searchSelect (RequestDefResult c _ _) _ = pure . wrap . clearStash . showRequestDefDetails c
searchSelect (AnEnvironmentResult c _) chan = selectEnvironment (Just c) chan
searchSelect NoEnvironmentResult chan = selectEnvironment Nothing chan

data Direction = Up | Down deriving (Eq) -- needed to know which direction to skip past section headers

-- When moving the selection up or down, if the item to be selected is actually a section header (or blank line),
-- then automatically scroll past it to the following item, since section headers can't be actioned
-- and thus there's no reason to allow them to be selected. Note that the first item in the list will
-- always be a section header, so we also want to prevent moving up past the second item.
-- Note that this logic is only necessary for the SearchScreen, as it's the only searchable screen
-- that has different sections with headers and separators.
scrollPastSection :: HasSearchTools (Screen a) => Direction -> Screen a -> Screen a
scrollPastSection direction scr = do
  let st@(SearchTools _ (AppList resultList) _) = scr ^. searchTools
      listMoveFunction :: Int -> Direction -> AppList a -> AppList a
      listMoveFunction moves Up (AppList zl) = AppList $ listMoveBy (-1 * moves) zl
      listMoveFunction moves Down (AppList zl) = AppList $ listMoveBy moves zl
      other Up = Down
      other Down = Up
      skipPast moves = scr & searchTools . appList %~ listMoveFunction moves direction
  if hasHeaders st
    then case listSelectedElement resultList of
      Just (0, _) -> scr & searchTools . appList %~ listMoveFunction 1 (other direction) -- tried to select the first item; have to undo the move
              -- Other section headers (besides the first) will be skipped over. Note that the number of lines to skip depends on which
              -- direction the user is navigating; this is due to the layout of the list (header -> content -> blank lines)
      Just (_, SearchSection _) -> skipPast (if direction == Down then 1 else 2)
      Just (_, SearchBlankLine) -> skipPast (if direction == Down then 2 else 1)
      _ -> scr
    else scr -- no headers means no modification necessary

-- Add section headers and footers and necessary in order to construct an AppList to be placed
-- in the SearchTools of a screen that supports searching
makeResultList :: SearchCandidates -> AppList SearchListItem
makeResultList cands =
  case cands of
    OneCandidateSet cands' -> AppList $ list SearchResultsList (SelectableResult <$> cands') 1
    AllCandidateSets (envs, ps, rds) ->
      let makeSection :: Text -> Seq SearchResult -> Bool -> Seq SearchListItem
          makeSection header results addBottomPadding
            | Seq.null results =
              Seq.empty
            | otherwise =
              let maybeHeader = Seq.singleton (SearchSection header)
                  maybeFooter = if addBottomPadding then Seq.singleton SearchBlankLine else Seq.empty
               in maybeHeader <> (SelectableResult <$> results) <> maybeFooter
          envSection = makeSection "Environments" envs True
          projectSection = makeSection "Projects" ps True
          rdSection = makeSection "Request Definitions" rds False
       in AppList $ list SearchResultsList (envSection <> projectSection <> rdSection) 1

-- Up and Down arrows move the selection
-- ENTER selects
-- All other keys are forwarded to the editor
handleEventSearch ::
  (SingI s, MonadEvent m, MonadReader Config.AppConfig m, HasSearchTools (Screen s)) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState s ->
  m AnyAppState
handleEventSearch key mods chan s = do
  km <- asks (view keymap)
  let st@(SearchTools (ZZZEditor edt) (AppList resultList) allResults) = s ^. screen ^. searchTools
      forwardToList key' direction = do
        updatedList <- AppList <$> liftEvent resultList (handleListEvent (EvKey key' []) resultList)
        let updatedScreen = (s ^. screen) & searchTools . appList .~ updatedList
        pure . wrap . (screen .~ scrollPastSection direction updatedScreen) $ s
  if  | matchKey (km ^. scrollUp) key mods -> forwardToList key Up
      | matchKey (km ^. scrollDown) key mods -> forwardToList key Down
      | matchKey (km ^. submit) key mods -> case listSelectedElement resultList of
        Just (_, SelectableResult selected) -> searchSelect selected chan s
        _ -> pure . wrap $ s
      | matchKey (km ^. back) key mods -> pure . unstashScreen $ s
      | otherwise -> do
        updatedEditor <- liftEvent edt $ handleEditorEvent (EvKey key mods) edt
        let contents = getEditContents updatedEditor
            searchString = fromMaybe "" (headMay contents)
            -- Right now I'm always filtering on full list of all possible search results;
            -- it's possible to make this more efficient by, for example, only filtering
            -- on the already-filtered list of results if the new search string contains
            -- the previous search string as a prefix. Let's see how the naive version performs
            -- for now, and possibly return to this if it becomes a problem.
            updatedResults = filterResults (hasHeaders st) searchString allResults
            AppList zl = makeResultList updatedResults
            -- Have to use listMoveDown to select the _second_ element in the results, since the first result (if there
            -- are any) will be the section header (only if section headers are being shown)
            updatedList = if hasHeaders st then listMoveDown zl else zl
        pure . wrap . (screen . searchTools .~ SearchTools (ZZZEditor updatedEditor) (AppList updatedList) allResults) $ s

searchWidget :: SearchTools -> Text -> Widget Name
searchWidget st@(SearchTools (ZZZEditor edt) (AppList results) _) emptyMessage =
  let contents :: Text = fromMaybe "" (headMay (getEditContents edt))
      fieldWidget =
        withAttr searchPlaceholderAttr (txt "> ")
          <+> if T.null contents
            then withAttr searchPlaceholderAttr $ txt "Enter text to start searching."
            else renderEditor renderText False edt
      renderFunction :: Bool -> SearchListItem -> Widget Name
      renderFunction _ item = padRight Max $ displaySearchListItem (hasHeaders st) item
      emptyNotification = if (Seq.null . listElements) results then txt emptyMessage else emptyWidget
      allWidgets = fieldWidget <=> emptyNotification <=> renderList renderFunction True results
   in Widget Fixed Fixed $ render allWidgets
