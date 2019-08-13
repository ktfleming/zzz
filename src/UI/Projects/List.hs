module UI.Projects.List where

import           Brick.Widgets.List             ( list )
import           Control.Lens
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as S
import           Types.AppState
import           Types.Brick.Name
import           Types.Models.Id                ( ProjectId )
import           Types.Models.Project
import           Types.Models.RequestDefinition ( name )
import           Types.Models.Screen
import           UI.List                        ( ZZZList )

makeProjectList :: Map ProjectId Project -> ZZZList ProjectListItem
makeProjectList pm =
  let tuples    = Map.toList pm
      listItems = foldr
        (\(pid, p) items ->
          items |> ProjectListItem (ProjectContext pid) (p ^. name)
        )
        S.empty
        tuples
  in  list ProjectList listItems 1

showProjectListScreen :: AppState -> AppState
showProjectListScreen s =
  s & screen .~ ProjectListScreen (makeProjectList (s ^. projects))

updateProjectList :: AppState -> ZZZList ProjectListItem -> AppState
updateProjectList s l = s & screen .~ ProjectListScreen l
