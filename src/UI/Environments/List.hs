{-# LANGUAGE DataKinds #-}

module UI.Environments.List where

import Brick.Widgets.List (list)
import Control.Lens
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Types.AppState
import Types.Brick.Name
import Types.Classes.Fields
import Types.Models.Environment
import Types.Models.Id (EnvironmentId)
import Types.Models.Screen
import UI.List (AppList (..))

makeEnvironmentList :: HashMap EnvironmentId Environment -> AppList EnvironmentListItem
makeEnvironmentList em =
  let tuples = Map.toList em
      listItems =
        foldr
          (\(eid, e) items -> items |> AnEnvironment (EnvironmentContext eid) (e ^. name))
          Seq.empty
          tuples
   in AppList $ list EnvironmentList (NoEnvironment <| listItems) 1

showEnvironmentListScreen :: AppState a -> AppState 'EnvironmentListTag
showEnvironmentListScreen s =
  s & screen .~ EnvironmentListScreen (makeEnvironmentList (s ^. environments))
