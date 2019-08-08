{-# LANGUAGE NamedFieldPuns #-}
module UI.Projects.List where

import           Brick.Widgets.List   (list)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.Vector          as V
import           Lens.Micro.Platform  ((&), (.~))
import           Types.AppState
import           Types.Brick.Name
import           Types.Models.ID      (ProjectID)
import           Types.Models.Project
import           Types.Models.Screen
import           UI.List              (ZZZList)

makeProjectList :: Map ProjectID Project -> ZZZList ProjectListItem
makeProjectList pm =
  let tuples    = (V.fromList . Map.toList) pm
      listItems = fmap
        (\(pid, Project { _projectName }) ->
          ProjectListItem (ProjectContext pid) _projectName
        )
        tuples
  in  list ProjectList listItems 1

showProjectListScreen :: AppState -> AppState
showProjectListScreen s@AppState { _projects } = s & activeScreen .~ ProjectListScreen (makeProjectList _projects)

updateProjectList :: AppState -> ZZZList ProjectListItem -> AppState
updateProjectList s l = s & activeScreen .~ ProjectListScreen l
