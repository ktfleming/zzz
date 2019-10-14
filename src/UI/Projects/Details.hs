{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Projects.Details where

import Brick.Widgets.List (list)
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Language.Haskell.DoNotation
import Types.AppState
import Types.Brick.Name
import Types.Classes.HasId (model)
import Types.ContextTransformers
  ( requestDefContext,
    requestDefListItem,
  )
import Types.Models.Id (RequestDefId)
import Types.Models.Project
import Types.Models.RequestDef
  ( RequestDef,
    RequestDefListItem,
  )
import Types.Models.Screen
import UI.List (AppList (..))
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

makeRequestDefList :: ProjectContext -> Project -> AppList RequestDefListItem
makeRequestDefList c p =
  let listItems = foldr f S.empty (Map.toList (p ^. requestDefs))
        where
          f :: (RequestDefId, RequestDef) -> Seq RequestDefListItem -> Seq RequestDefListItem
          f (rid, r) items = let rc = requestDefContext c rid in items |> requestDefListItem rc r
   in AppList $ list RequestDefList listItems 1

showProjectDetails ::
  IxMonadState m => ProjectContext -> m (AppState a) (AppState 'ProjectDetailsTag) ()
showProjectDetails c = do
  s <- iget
  let p = model s c
  imodify $ screen .~ ProjectDetailsScreen c (makeRequestDefList c p)
