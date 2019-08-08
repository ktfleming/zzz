{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types.Classes.Listable where

import Types.Classes.WithID (HasID)
import           Types.AppState
import Types.Models.Project (ProjectListItem(..), Project(..))
import Types.Classes.Addable (NoContext(..))
import UI.List (ZZZList)
import Lens.Micro.Platform        ((.~), (&))
import           Types.Models.Screen
import UI.Projects.List (makeProjectList)

class HasID a => Listable a where
  type family ListItem a = b | b -> a -- the model that actually gets put in the ZZZList
  type family ListContext a = b | b -> a -- the context required to list items; the "parent" context

  -- Given the required context, construct the ZZZList that user can interact with
  makeList :: AppState -> ListContext a -> ZZZList (ListItem a)

  -- Update the AppState to display the appropriate list screen
  showListScreen :: AppState -> ListContext a -> AppState
  
  -- Use with Brick's `handleListEvent` to update the ZZZList in the AppState
  updateList :: AppState -> ListContext a -> ZZZList (ListItem a) -> AppState
  
instance Listable Project where
  type ListItem Project = ProjectListItem
  type ListContext Project = NoContext
  
  makeList :: AppState -> NoContext -> ZZZList ProjectListItem
  makeList AppState { _projects } _ = makeProjectList _projects
  
  showListScreen :: AppState -> NoContext -> AppState
  showListScreen s _ = s & activeScreen .~ ProjectListScreen (makeList s NoContext)
  
  updateList :: AppState -> NoContext -> ZZZList ProjectListItem -> AppState
  updateList s _ l = s & activeScreen .~ ProjectListScreen l
  