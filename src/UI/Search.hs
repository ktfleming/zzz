{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Search
  ( handleEventSearch
  , searchWidget
  , showSearchScreen
  )
where

import           Brick                          ( EventM
                                                , Size(Fixed)
                                                , Widget(..)
                                                , txt
                                                , withAttr
                                                , (<=>)
                                                )
import           Brick.Widgets.Edit             ( Editor
                                                , editorText
                                                , getEditContents
                                                , handleEditorEvent
                                                , renderEditor
                                                )
import           Brick.Widgets.List             ( list
                                                , listSelectedElement
                                                )
import           Control.Lens
import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Graphics.Vty.Input.Events      ( Event(..)
                                                , Key(..)
                                                , Modifier
                                                )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Safe                           ( headMay )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Models.Id                ( ProjectId )
import           Types.Models.Project           ( Project
                                                , ProjectContext(..)
                                                , requestDefs
                                                )
import           Types.Models.RequestDef        ( RequestDefContext(..) )
import           Types.Models.Screen
import           Types.Search                   ( SearchResult(..)
                                                , filterResults
                                                )
import           UI.Attr                        ( searchPlaceholderAttr )
import           UI.Events.BrickUpdates         ( updateBrickList )
import           UI.Form                        ( renderText )
import           UI.List                        ( ZZZList
                                                , renderGenericList
                                                )
import           UI.Projects.Details            ( showProjectDetails )
import           UI.RequestDefs.Details         ( showRequestDefDetails )
import           Utils.IxState                  ( extractScreen
                                                , submerge
                                                , wrapScreen
                                                , (>>>)
                                                )

makeResultList :: Seq SearchResult -> ZZZList SearchResult
makeResultList results = list SearchResultsList results 1

searchWidget :: Editor T.Text Name -> ZZZList SearchResult -> Widget Name
searchWidget edt results =
  -- This ugly case is used since apparently if-then-else doesn't work with RebindableSyntax (?)
  let
    fieldWidget = case getEditContents edt of
      [""] -> withAttr searchPlaceholderAttr $ txt
        "Enter text to start searching. Press ENTER to jump to the selected item's details page."
      _ -> renderEditor renderText False edt
    allWidgets = fieldWidget <=> renderGenericList True results
  in
    Widget Fixed Fixed $ render allWidgets

requestDefResults :: Project -> ProjectId -> Seq SearchResult
requestDefResults p pid =
  let rds = p ^. requestDefs
  in  Map.foldrWithKey
        (\rid r rs -> rs |> RequestDefResult (p ^. name) (r ^. name) (RequestDefContext pid rid))
        S.empty
        rds

allSearchResults :: AppState a -> Seq SearchResult
allSearchResults s = Map.foldrWithKey fn S.empty (s ^. projects)
 where
  fn :: ProjectId -> Project -> Seq SearchResult -> Seq SearchResult
  fn pid p results =
    let projectResult = ProjectResult (p ^. name) (ProjectContext pid)
        rdResults     = requestDefResults p pid
    in  (results |> projectResult) <> rdResults

showSearchScreen :: Monad m => IxStateT m (AppState a) (AppState 'SearchTag) ()
showSearchScreen = iget >>>= \s ->
  let edt     = editorText SearchField (Just 1) ""
      -- Note: the sequence of SearchResults (as opposed to the list) is fixed and will not
      -- be narrowed down. It's used for filtering (see note in `handleSearchEvent` for more).
      results = allSearchResults s
  in  imodify $ screen .~ SearchScreen edt (makeResultList results) results

searchSelect :: SearchResult -> IxStateT (EventM Name) (AppState 'SearchTag) AnyAppState ()
searchSelect (ProjectResult _ c     ) = showProjectDetails c >>> submerge
searchSelect (RequestDefResult _ _ c) = showRequestDefDetails c >>> submerge

-- Up and Down arrows move the selection
-- ENTER selects
-- All other keys are forwarded to the editor
handleEventSearch
  :: Key -> [Modifier] -> IxStateT (EventM Name) (AppState 'SearchTag) AnyAppState ()
handleEventSearch key mods = do
  s <- iget
  let SearchScreen edt resultList allResults = s ^. screen
      forwardToList k = extractScreen >>> updateBrickList k >>> wrapScreen s >>> submerge
  case key of
    KUp    -> forwardToList key
    KDown  -> forwardToList key
    KEnter -> case listSelectedElement resultList of
      Just selected -> searchSelect (snd selected)
      Nothing       -> submerge
    _ -> do
      updatedEditor <- ilift $ handleEditorEvent (EvKey key mods) edt
      let editContents   = getEditContents updatedEditor
          searchString   = fromMaybe "" (headMay editContents)

          -- Right now I'm always filtering on full list of all possible search results;
          -- it's possible to make this more efficient by, for example, only filtering
          -- on the already-filtered list of results if the new search string contains
          -- the previous search string as a prefix. Let's see how the naive version performs
          -- for now, and possibly return to this if it becomes a problem.
          updatedResults = filterResults searchString allResults
      imodify $ screen .~ SearchScreen updatedEditor (makeResultList updatedResults) allResults
      submerge
