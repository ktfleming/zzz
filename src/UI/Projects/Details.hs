{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Projects.Details where

import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )

import           Brick                          ( EventM )
import           Brick.Widgets.List             ( handleListEvent
                                                , list
                                                )
import           Control.Lens
import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                , iput
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import qualified Data.HashMap.Strict           as Map
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Graphics.Vty                   ( Event(EvKey)
                                                , Key
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.HasId            ( model )
import           Types.ContextTransformers      ( requestDefContext
                                                , requestDefListItem
                                                )
import           Types.Models.Id                ( RequestDefId )
import           Types.Models.Project
import           Types.Models.RequestDef        ( RequestDef
                                                , RequestDefListItem
                                                )
import           Types.Models.Screen

showProjectDetails
  :: Monad m
  => ProjectContext
  -> IxStateT m (AppState a) (AppState 'ProjectDetailsTag) ()
showProjectDetails c = iget >>>= \s ->
  let p         = model s c
      listItems = foldr f S.empty (Map.toList (p ^. requestDefs))
         where
          f
            :: (RequestDefId, RequestDef)
            -> Seq RequestDefListItem
            -> Seq RequestDefListItem
          f (rid, r) items =
            let rc = requestDefContext c rid in items |> requestDefListItem rc r
      reqList = list RequestDefList listItems 1
      newState :: AppState 'ProjectDetailsTag =
          s & screen .~ ProjectDetailsScreen c reqList
  in  iput newState

updateProjectDetailsList
  :: Key
  -> IxStateT
       (EventM Name)
       (AppState 'ProjectDetailsTag)
       (AppState 'ProjectDetailsTag)
       ()
updateProjectDetailsList key = do
  s <- iget
  let ProjectDetailsScreen c l = s ^. screen
  updatedList <- ilift $ handleListEvent (EvKey key []) l
  imodify $ screen .~ ProjectDetailsScreen c updatedList
