{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Types.Models.Screen.Optics
  ( updateBrickForm
  , updateBrickList
  , listLens
  , lastError
  )
where

import           Brick                          ( BrickEvent(..) )
import           Brick.Forms                    ( handleFormEvent )
import           Brick.Widgets.List             ( handleListEvent )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxMonadState
                                                , iget
                                                , imodify
                                                )
import           Graphics.Vty.Input.Events      ( Event(..)
                                                , Key
                                                )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(return, (>>), (>>=))
                                                , pure
                                                )
import           Types.Models.Environment       ( EnvironmentFormState
                                                , EnvironmentListItem
                                                )
import           Types.Models.Project           ( ProjectFormState
                                                , ProjectListItem
                                                )
import           Types.Models.RequestDef        ( RequestDefFormState
                                                , RequestDefListItem
                                                , RequestError
                                                )
import           Types.Models.Response          ( Response )
import           Types.Models.Screen
import           Types.Monads                   ( IxMonadEvent
                                                , iliftEvent
                                                )
import           Types.Search                   ( SearchListItem )
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

instance HasBrickForm (Screen 'RequestDefEditTag) where
  type FormState (Screen 'RequestDefEditTag) = RequestDefFormState
  formLens = lens (\(RequestDefEditScreen _ form) -> form)
                  (\(RequestDefEditScreen c _) form -> RequestDefEditScreen c form)

instance HasBrickForm (Screen 'EnvironmentEditTag) where
  type FormState (Screen 'EnvironmentEditTag) = EnvironmentFormState
  formLens = lens (\(EnvironmentEditScreen _ form) -> form)
                  (\(EnvironmentEditScreen c _) form -> EnvironmentEditScreen c form)

instance HasBrickForm (Screen 'EnvironmentAddTag) where
  type FormState (Screen 'EnvironmentAddTag) = EnvironmentFormState
  formLens = lens (\(EnvironmentAddScreen form) -> form) (\_ f -> EnvironmentAddScreen f)

updateBrickForm
  :: (IxMonadState m, IxMonadEvent m, HasBrickForm (Screen a)) => Key -> m (Screen a) (Screen a) ()
updateBrickForm key = do
  scr <- iget
  let form = scr ^. formLens
  updatedForm <- iliftEvent $ handleFormEvent (VtyEvent (EvKey key [])) form
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
  listLens = lens (\(RequestDefDetailsScreen _ l _ _) -> l)
                  (\(RequestDefDetailsScreen c _ ring e) l -> RequestDefDetailsScreen c l ring e)

instance HasBrickList (Screen 'EnvironmentListTag) where
  type ListItem (Screen 'EnvironmentListTag) = EnvironmentListItem
  listLens = lens (\(EnvironmentListScreen l) -> l) (\_ l -> EnvironmentListScreen l)

instance HasBrickList (Screen 'SearchTag) where
  type ListItem (Screen 'SearchTag) = SearchListItem
  listLens = lens (\(SearchScreen _ l _) -> l) (\(SearchScreen e _ rs) l -> SearchScreen e l rs)

updateBrickList
  :: (IxMonadState m, IxMonadEvent m, HasBrickList (Screen a)) => Key -> m (Screen a) (Screen a) ()
updateBrickList key = do
  scr <- iget
  let list = scr ^. listLens
  updatedList <- iliftEvent $ (handleListEvent (EvKey key []) list)
  imodify $ listLens .~ updatedList

-- lens to update the error inside of the RequestDefDetailsScreen
lastError :: Lens' (Screen 'RequestDefDetailsTag) (Maybe RequestError)
lastError = lens (\(RequestDefDetailsScreen _ _ _ e) -> e)
                 (\(RequestDefDetailsScreen c l ring _) e -> RequestDefDetailsScreen c l ring e)
