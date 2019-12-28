{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Environments.List
  ( selectEnvironment,
    refreshIfNecessary,
    environmentListSearchTools,
    showEnvironmentListScreen,
  )
where

import Brick.BChan (BChan)
import Brick.Widgets.List (list)
import Control.Lens
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Singletons
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Brick.Name
import Types.Classes.Fields
import Types.Models.Environment
import Types.Models.Id (EnvironmentId)
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import Types.Search
import UI.List (AppList (..))
import UI.RequestDefs.Details
import UI.Search.Editor

-- Sets the provided environment (or no environment, in the case of Nothing) as the active one.
-- This necessitates a reset of the currently displayed error, if the active screen happens to be
-- the RequestDef details screen, and the environment is actually changing.
selectEnvironment ::
  (MonadIO m, SingI a) =>
  Maybe EnvironmentContext ->
  BChan CustomEvent ->
  AppState a ->
  m AnyAppState
selectEnvironment c chan s = do
  let currentEnv = s ^. environmentContext
      isChanging = currentEnv /= c
      action = if isChanging then pure . refreshIfNecessary else pure
  saveAfter chan . action . unstashScreen . (environmentContext .~ c) $ s

-- Selecting a new environment necessitates a refresh of the screen if the current screen happens to be the
-- request def details screen
refreshIfNecessary :: AnyAppState -> AnyAppState
refreshIfNecessary outer@(AnyAppState _ s) =
  case s ^. screen of
    RequestDefDetailsScreen {} ->
      wrap $ refreshResponseList (s & screen . lastError .~ Nothing)
    _ -> outer

listItems :: HashMap EnvironmentId Environment -> Seq SearchResult
listItems em =
  foldr f (Seq.singleton NoEnvironmentResult) (Map.toList em)
  where
    f :: (EnvironmentId, Environment) -> Seq SearchResult -> Seq SearchResult
    f (eid, e) items = items |> AnEnvironmentResult (EnvironmentContext eid) (e ^. name)

environmentListSearchTools :: HashMap EnvironmentId Environment -> SearchTools
environmentListSearchTools em =
  let items = listItems em
      wrappedItems = SelectableResult <$> items
      cands = OneCandidateSet items
   in SearchTools searchEditor (AppList $ list EnvironmentList wrappedItems 1) cands

showEnvironmentListScreen :: AppState a -> AppState 'EnvironmentListTag
showEnvironmentListScreen s = s & screen .~ EnvironmentListScreen (environmentListSearchTools (s ^. environments))
