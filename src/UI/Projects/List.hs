module UI.Projects.List where

import           Brick                          ( EventM )
import           Brick.Widgets.List             ( handleListEvent
                                                , list
                                                )
import           Control.Lens
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as S
import           Graphics.Vty                   ( Event(EvKey)
                                                , Key
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Models.Id                ( ProjectId )
import           Types.Models.Project
import           Types.Models.RequestDefinition ( name )
import           Types.Models.Screen
import           UI.List                        ( ZZZList )

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State      ( StateT
                                                , modify
                                                )

makeProjectList :: HashMap ProjectId Project -> ZZZList ProjectListItem
makeProjectList pm =
  let tuples    = Map.toList pm
      listItems = foldr
        (\(pid, p) items ->
          items |> ProjectListItem (ProjectContext pid) (p ^. name)
        )
        S.empty
        tuples
  in  list ProjectList listItems 1

showProjectListScreen :: Monad m => StateT AppState m ()
showProjectListScreen = modify
  $ \s -> s & screen .~ ProjectListScreen (makeProjectList (s ^. projects))

updateProjectList
  :: ZZZList ProjectListItem -> Key -> StateT AppState (EventM Name) ()
updateProjectList l key = do
  updatedList <- lift $ handleListEvent (EvKey key []) l
  modify $ screen . _ProjectListScreen .~ updatedList
