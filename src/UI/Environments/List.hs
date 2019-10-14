{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Environments.List where

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
          S.empty
          tuples
   in AppList $ list EnvironmentList (NoEnvironment <| listItems) 1

showEnvironmentListScreen :: IxMonadState m => m (AppState a) (AppState 'EnvironmentListTag) ()
showEnvironmentListScreen =
  imodify $ \s -> s & screen .~ EnvironmentListScreen (makeEnvironmentList (s ^. environments))
