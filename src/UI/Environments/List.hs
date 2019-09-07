{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module UI.Environments.List where

import           Brick.Widgets.List             ( list )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , imodify
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as S
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Models.Environment
import           Types.Models.Id                ( EnvironmentId )
import           Types.Models.Screen
import           UI.List                        ( ZZZList )

makeEnvironmentList :: HashMap EnvironmentId Environment -> ZZZList EnvironmentListItem
makeEnvironmentList em =
  let tuples    = Map.toList em
      listItems = foldr
        (\(eid, e) items -> items |> EnvironmentListItem (EnvironmentContext eid) (e ^. name))
        S.empty
        tuples
  in  list EnvironmentList listItems 1

showEnvironmentListScreen :: Monad m => IxStateT m (AppState a) (AppState 'EnvironmentListTag) ()
showEnvironmentListScreen =
  imodify $ \s -> s & screen .~ EnvironmentListScreen (makeEnvironmentList (s ^. environments))
