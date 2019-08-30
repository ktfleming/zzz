{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module UI.Events.BrickUpdates
  ( updateBrickForm
  , updateBrickList
  )
where

import           Brick                          ( BrickEvent(..) )
import           Brick.Forms                    ( handleFormEvent )
import           Brick.Types                    ( EventM )
import           Brick.Widgets.List             ( handleListEvent )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Graphics.Vty.Input.Events      ( Event(..)
                                                , Key
                                                )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.Brick.Name               ( Name )
import           Types.Models.Project           ( ProjectFormState
                                                , ProjectListItem
                                                )
import           Types.Models.RequestDef        ( RequestDefFormState
                                                , RequestDefListItem
                                                )
import           Types.Models.Response          ( Response )
import           Types.Models.Screen
import           Types.Search                   ( SearchResult )
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )

class HasBrickForm a where
  type family FormState a
  formLens :: Lens' a (ZZZForm (FormState a))

instance HasBrickForm (Screen 'ProjectAddTag) where
  type FormState (Screen 'ProjectAddTag) = ProjectFormState
  formLens = lens (\(ProjectAddScreen form) -> form) (\_ f -> ProjectAddScreen f)

instance HasBrickForm (Screen 'ProjectEditTag) where
  type FormState (Screen 'ProjectEditTag) = ProjectFormState
  formLens = lens (\(ProjectEditScreen _ form) -> form)
                  (\(ProjectEditScreen c _) form -> ProjectEditScreen c form)

instance HasBrickForm (Screen 'RequestDefAddTag) where
  type FormState (Screen 'RequestDefAddTag) = RequestDefFormState
  formLens = lens (\(RequestDefAddScreen _ form) -> form)
                  (\(RequestDefAddScreen c _) form -> RequestDefAddScreen c form)

instance HasBrickForm (Screen 'RequestDefEditTag)where
  type FormState (Screen 'RequestDefEditTag) = RequestDefFormState
  formLens = lens (\(RequestDefEditScreen _ form) -> form)
                  (\(RequestDefEditScreen c _) form -> RequestDefEditScreen c form)

updateBrickForm :: HasBrickForm (Screen a) => Key -> IxStateT (EventM Name) (Screen a) (Screen a) ()
updateBrickForm key = do
  scr <- iget
  let form = scr ^. formLens
  updatedForm <- ilift $ handleFormEvent (VtyEvent (EvKey key [])) form
  imodify $ formLens .~ updatedForm

class HasBrickList a where
  type family ListItem a
  listLens :: Lens' a (ZZZList (ListItem a))

instance HasBrickList (Screen 'ProjectListTag) where
  type ListItem (Screen 'ProjectListTag) = ProjectListItem
  listLens = lens (\(ProjectListScreen l) -> l) (\_ l -> ProjectListScreen l)

instance HasBrickList (Screen 'ProjectDetailsTag) where
  type ListItem (Screen 'ProjectDetailsTag) = RequestDefListItem
  listLens = lens (\(ProjectDetailsScreen _ l) -> l)
                  (\(ProjectDetailsScreen c _) l -> ProjectDetailsScreen c l)

instance HasBrickList (Screen 'RequestDefDetailsTag) where
  type ListItem (Screen 'RequestDefDetailsTag) = Response
  listLens = lens (\(RequestDefDetailsScreen _ l _) -> l)
                  (\(RequestDefDetailsScreen c _ ring) l -> RequestDefDetailsScreen c l ring)

instance HasBrickList (Screen 'SearchTag) where
  type ListItem (Screen 'SearchTag) = SearchResult
  listLens = lens (\(SearchScreen _ l _) -> l) (\(SearchScreen e _ rs) l -> SearchScreen e l rs)

updateBrickList :: HasBrickList (Screen a) => Key -> IxStateT (EventM Name) (Screen a) (Screen a) ()
updateBrickList key = do
  scr <- iget
  let list = scr ^. listLens
  updatedList <- ilift $ handleListEvent (EvKey key []) list
  imodify $ listLens .~ updatedList
