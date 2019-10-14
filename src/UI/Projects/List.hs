{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Projects.List where

import Brick.Widgets.List (list)
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    imodify,
  )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as S
import Types.AppState
import Types.Brick.Name
import Types.Classes.Fields
import Types.Models.Id (ProjectId)
import Types.Models.Project
import Types.Models.Screen
import UI.List (AppList (..))

makeProjectList :: HashMap ProjectId Project -> AppList ProjectListItem
makeProjectList pm =
  let tuples = Map.toList pm
      listItems =
        foldr
          (\(pid, p) items -> items |> ProjectListItem (ProjectContext pid) (p ^. name))
          S.empty
          tuples
   in AppList $ list ProjectList listItems 1

showProjectListScreen :: IxMonadState m => m (AppState a) (AppState 'ProjectListTag) ()
showProjectListScreen =
  imodify $ \s -> s & screen .~ ProjectListScreen (makeProjectList (s ^. projects))
