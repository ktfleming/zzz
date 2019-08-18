{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Projects.List where

import           Brick                          ( EventM )
import           Brick.Widgets.List             ( handleListEvent
                                                , list
                                                )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
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
import           Types.Models.RequestDef        ( name )
import           Types.Models.Screen
import           UI.List                        ( ZZZList )

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

showProjectListScreen
  :: Monad m => IxStateT m (AppState a) (AppState 'ProjectListTag) ()
showProjectListScreen = imodify
  $ \s -> s & screen .~ ProjectListScreen (makeProjectList (s ^. projects))

updateProjectList
  :: Key
  -> IxStateT
       (EventM Name)
       (AppState 'ProjectListTag)
       (AppState 'ProjectListTag)
       ()
updateProjectList key = do
  s :: AppState 'ProjectListTag <- iget
  let (ProjectListScreen l) :: Screen 'ProjectListTag = s ^. screen
  updatedList <- ilift $ handleListEvent (EvKey key []) l
  imodify $ screen .~ ProjectListScreen updatedList
