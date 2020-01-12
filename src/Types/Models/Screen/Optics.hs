{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Models.Screen.Optics where

import Brick (BrickEvent (..))
import Brick.Forms (allFieldsValid, handleFormEvent)
import Brick.Widgets.List (handleListEvent)
import Control.Lens
import Data.Singletons (SingI)
import Graphics.Vty.Input.Events
  ( Event (..),
    Key,
  )
import Types.AppState (AnyAppState (..), AppState, screen)
import Types.Models.Environment
import Types.Models.Project
import Types.Models.RequestDef
import Types.Models.Response (Response)
import Types.Models.Screen
import Types.Monads
import Types.Search
import UI.Form (AppForm (..))
import UI.List (AppList (..))

class HasBrickForm a where
  type FormState a

  formLens :: Lens' a (AppForm (FormState a))

instance HasBrickForm (Screen 'ProjectAddTag) where
  type FormState (Screen 'ProjectAddTag) = ProjectFormState

  formLens = lens (\(ProjectAddScreen form) -> form) (\_ f -> ProjectAddScreen f)

instance HasBrickForm (Screen 'ProjectEditTag) where
  type FormState (Screen 'ProjectEditTag) = ProjectFormState

  formLens =
    lens
      (\(ProjectEditScreen _ form) -> form)
      (\(ProjectEditScreen c _) form -> ProjectEditScreen c form)

instance HasBrickForm (Screen 'RequestDefAddTag) where
  type FormState (Screen 'RequestDefAddTag) = RequestDefFormState

  formLens =
    lens
      (\(RequestDefAddScreen _ form) -> form)
      (\(RequestDefAddScreen c _) form -> RequestDefAddScreen c form)

instance HasBrickForm (Screen 'RequestDefEditTag) where
  type FormState (Screen 'RequestDefEditTag) = RequestDefFormState

  formLens =
    lens
      (\(RequestDefEditScreen _ form) -> form)
      (\(RequestDefEditScreen c _) form -> RequestDefEditScreen c form)

instance HasBrickForm (Screen 'EnvironmentEditTag) where
  type FormState (Screen 'EnvironmentEditTag) = EnvironmentFormState

  formLens =
    lens
      (\(EnvironmentEditScreen _ form) -> form)
      (\(EnvironmentEditScreen c _) form -> EnvironmentEditScreen c form)

instance HasBrickForm (Screen 'EnvironmentAddTag) where
  type FormState (Screen 'EnvironmentAddTag) = EnvironmentFormState

  formLens = lens (\(EnvironmentAddScreen form) -> form) (\_ f -> EnvironmentAddScreen f)

-- For getting a form out of an AppState that contains a Screen that contains a form
instance HasBrickForm (Screen a) => HasBrickForm (AppState a) where
  type FormState (AppState a) = FormState (Screen a)

  formLens =
    lens
      (view (screen . formLens))
      (\s form -> s & screen . formLens .~ form)

updateBrickForm ::
  (MonadEvent m, HasBrickForm a) => Key -> a -> m a
updateBrickForm key model = do
  let AppForm form = model ^. formLens
  updatedForm <- AppForm <$> liftEvent form (handleFormEvent (VtyEvent (EvKey key [])) form)
  pure $ model & formLens .~ updatedForm

class HasBrickList a where
  type ListItem a

  listLens :: Lens' a (AppList (ListItem a))

instance HasBrickList (Screen 'RequestDefDetailsTag) where
  type ListItem (Screen 'RequestDefDetailsTag) = Response

  listLens =
    lens
      (\(RequestDefDetailsScreen _ l _ _) -> l)
      (\(RequestDefDetailsScreen c _ ring e) l -> RequestDefDetailsScreen c l ring e)

-- For getting a list out of an AppState that contains a Screen that contains a list
instance HasBrickList (Screen a) => HasBrickList (AppState a) where
  type ListItem (AppState a) = ListItem (Screen a)

  listLens =
    lens
      (view (screen . listLens))
      (\s list -> s & screen . listLens .~ list)

-- Use Brick's `handleListEvent` to handle a key input and update the list
-- contained in the given model (Screen or AppState)
updateBrickList ::
  (MonadEvent m, HasBrickList a) => Key -> a -> m a
updateBrickList key model = do
  let AppList l = model ^. listLens
  updatedList <- AppList <$> liftEvent l (handleListEvent (EvKey key []) l)
  pure $ model & listLens .~ updatedList

-- lens to update the error inside of the RequestDefDetailsScreen
lastError :: Lens' (Screen 'RequestDefDetailsTag) (Maybe RequestError)
lastError =
  lens
    (\(RequestDefDetailsScreen _ _ _ e) -> e)
    (\(RequestDefDetailsScreen c l ring _) e -> RequestDefDetailsScreen c l ring e)

-- Starting with an AppState that has a form, check if the form is valid. If so, run the provided action; if not, just wrap the state.
ifValid :: (Monad m, SingI i, HasBrickForm (AppState i)) => (AppState i -> m AnyAppState) -> AppState i -> m AnyAppState
ifValid onValid s =
  let AppForm form = s ^. formLens
   in if allFieldsValid form then onValid s else pure $ wrap s

class HasContext s a | s -> a where
  context :: Getter s a

instance HasContext (Screen 'ProjectEditTag) ProjectContext where
  context = to $ \(ProjectEditScreen c _) -> c

instance HasContext (Screen 'ProjectDetailsTag) ProjectContext where
  context = to $ \(ProjectDetailsScreen c _) -> c

instance HasContext (Screen 'RequestDefAddTag) ProjectContext where
  context = to $ \(RequestDefAddScreen c _) -> c

instance HasContext (Screen 'RequestDefEditTag) RequestDefContext where
  context = to $ \(RequestDefEditScreen c _) -> c

instance HasContext (Screen 'RequestDefDetailsTag) RequestDefContext where
  context = to $ \(RequestDefDetailsScreen c _ _ _) -> c

class HasSearchTools a where
  searchTools :: Lens' a SearchTools

instance HasSearchTools (Screen 'SearchTag) where
  searchTools = lens (\(SearchScreen t) -> t) (\_ t -> SearchScreen t)

instance HasSearchTools (Screen 'ProjectListTag) where
  searchTools = lens (\(ProjectListScreen t) -> t) (\_ t -> ProjectListScreen t)

instance HasSearchTools (Screen 'ProjectDetailsTag) where
  searchTools = lens (\(ProjectDetailsScreen _ t) -> t) (\(ProjectDetailsScreen c _) t -> ProjectDetailsScreen c t)

instance HasSearchTools (Screen 'EnvironmentListTag) where
  searchTools = lens (\(EnvironmentListScreen t) -> t) (\_ t -> EnvironmentListScreen t)
