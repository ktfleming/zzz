{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Models.Screen.Optics
  ( updateBrickForm,
    updateBrickList,
    listLens,
    formLens,
    lastError,
    FormMode (..),
    ifValid,
  )
where

import Brick (BrickEvent (..))
import Brick.Forms (allFieldsValid, handleFormEvent)
import Brick.Widgets.List (handleListEvent)
import Control.Lens hiding (imap)
import Control.Monad.Indexed (imap)
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import Data.Singletons (SingI)
import Graphics.Vty.Input.Events
  ( Event (..),
    Key,
  )
import Language.Haskell.DoNotation
import Types.AppState (AnyAppState (..), AppState, screen)
import Types.Forms (FormMode (..))
import Types.Models.Environment
  ( EnvironmentFormState,
    EnvironmentListItem,
  )
import Types.Models.Project
  ( ProjectFormState,
    ProjectListItem,
  )
import Types.Models.RequestDef
  ( RequestDefFormState,
    RequestDefListItem,
    RequestError,
  )
import Types.Models.Response (Response)
import Types.Models.Screen
import Types.Monads
  ( IxMonadEvent,
    iliftEvent,
    submerge,
  )
import Types.Search (SearchListItem)
import UI.Form (AppForm (..))
import UI.List (AppList (..))
import Utils.IfThenElse
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

class HasBrickForm a where

  type FormState a

  formLens :: Lens' a (AppForm (FormState a))

instance HasBrickForm (Screen 'ProjectAddTag) where

  type FormState (Screen 'ProjectAddTag) = ProjectFormState 'Adding

  formLens = lens (\(ProjectAddScreen form) -> form) (\_ f -> ProjectAddScreen f)

instance HasBrickForm (Screen 'ProjectEditTag) where

  type FormState (Screen 'ProjectEditTag) = ProjectFormState 'Editing

  formLens =
    lens
      (\(ProjectEditScreen _ form) -> form)
      (\(ProjectEditScreen c _) form -> ProjectEditScreen c form)

instance HasBrickForm (Screen 'RequestDefAddTag) where

  type FormState (Screen 'RequestDefAddTag) = RequestDefFormState 'Adding

  formLens =
    lens
      (\(RequestDefAddScreen _ form) -> form)
      (\(RequestDefAddScreen c _) form -> RequestDefAddScreen c form)

instance HasBrickForm (Screen 'RequestDefEditTag) where

  type FormState (Screen 'RequestDefEditTag) = RequestDefFormState 'Editing

  formLens =
    lens
      (\(RequestDefEditScreen _ form) -> form)
      (\(RequestDefEditScreen c _) form -> RequestDefEditScreen c form)

instance HasBrickForm (Screen 'EnvironmentEditTag) where

  type FormState (Screen 'EnvironmentEditTag) = EnvironmentFormState 'Editing

  formLens =
    lens
      (\(EnvironmentEditScreen _ form) -> form)
      (\(EnvironmentEditScreen c _) form -> EnvironmentEditScreen c form)

instance HasBrickForm (Screen 'EnvironmentAddTag) where

  type FormState (Screen 'EnvironmentAddTag) = EnvironmentFormState 'Adding

  formLens = lens (\(EnvironmentAddScreen form) -> form) (\_ f -> EnvironmentAddScreen f)

-- For getting a form out of an AppState that contains a Screen that contains a form
instance HasBrickForm (Screen a) => HasBrickForm (AppState a) where

  type FormState (AppState a) = FormState (Screen a)

  formLens =
    lens
      (view (screen . formLens))
      (\s form -> s & screen . formLens .~ form)

updateBrickForm ::
  (IxMonadState m, IxMonadEvent m, HasBrickForm (Screen a)) => Key -> m (Screen a) (Screen a) ()
updateBrickForm key = do
  scr <- iget
  let AppForm form = scr ^. formLens
  updatedForm <- imap AppForm $ iliftEvent form $ handleFormEvent (VtyEvent (EvKey key [])) form
  imodify $ formLens .~ updatedForm

class HasBrickList a where

  type ListItem a

  listLens :: Lens' a (AppList (ListItem a))

instance HasBrickList (Screen 'ProjectListTag) where

  type ListItem (Screen 'ProjectListTag) = ProjectListItem

  listLens = lens (\(ProjectListScreen l) -> l) (\_ l -> ProjectListScreen l)

instance HasBrickList (Screen 'ProjectDetailsTag) where

  type ListItem (Screen 'ProjectDetailsTag) = RequestDefListItem

  listLens =
    lens
      (\(ProjectDetailsScreen _ l) -> l)
      (\(ProjectDetailsScreen c _) l -> ProjectDetailsScreen c l)

instance HasBrickList (Screen 'RequestDefDetailsTag) where

  type ListItem (Screen 'RequestDefDetailsTag) = Response

  listLens =
    lens
      (\(RequestDefDetailsScreen _ l _ _) -> l)
      (\(RequestDefDetailsScreen c _ ring e) l -> RequestDefDetailsScreen c l ring e)

instance HasBrickList (Screen 'EnvironmentListTag) where

  type ListItem (Screen 'EnvironmentListTag) = EnvironmentListItem

  listLens = lens (\(EnvironmentListScreen l) -> l) (\_ l -> EnvironmentListScreen l)

instance HasBrickList (Screen 'SearchTag) where

  type ListItem (Screen 'SearchTag) = SearchListItem

  listLens = lens (\(SearchScreen _ l _) -> l) (\(SearchScreen e _ rs) l -> SearchScreen e l rs)

-- For getting a list out of an AppState that contains a Screen that contains a list
instance HasBrickList (Screen a) => HasBrickList (AppState a) where

  type ListItem (AppState a) = ListItem (Screen a)

  listLens =
    lens
      (view (screen . listLens))
      (\s list -> s & screen . listLens .~ list)

updateBrickList ::
  (IxMonadState m, IxMonadEvent m, HasBrickList (Screen a)) => Key -> m (Screen a) (Screen a) ()
updateBrickList key = do
  scr <- iget
  let AppList l = scr ^. listLens
  updatedList <- imap AppList $ iliftEvent l (handleListEvent (EvKey key []) l)
  imodify $ listLens .~ updatedList

-- lens to update the error inside of the RequestDefDetailsScreen
lastError :: Lens' (Screen 'RequestDefDetailsTag) (Maybe RequestError)
lastError =
  lens
    (\(RequestDefDetailsScreen _ _ _ e) -> e)
    (\(RequestDefDetailsScreen c l ring _) e -> RequestDefDetailsScreen c l ring e)

-- Starting with an AppState that has a form, check if the form is valid. If so, run the provided action; if not, just submerge the state.
ifValid :: (IxMonadState m, SingI i, HasBrickForm (AppState i)) => m (AppState i) AnyAppState () -> m (AppState i) AnyAppState ()
ifValid onValid = do
  model <- iget
  let AppForm form = model ^. formLens
  if allFieldsValid form then onValid else submerge
