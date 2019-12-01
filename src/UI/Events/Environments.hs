{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Events.Environments where

import Brick.BChan (BChan)
import Brick.Widgets.List (listSelectedElement)
import Control.Lens
import Control.Monad.Indexed (ireturn)
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
    iput,
  )
import Data.Singletons (SingI)
import Data.UUID.V4 (nextRandom)
import Graphics.Vty.Input.Events
import Language.Haskell.DoNotation
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Classes.Fields
import Types.Modal
import Types.Models.Environment
  ( EnvironmentContext,
    EnvironmentListItem (..),
  )
import Types.Models.Id (EnvironmentId (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
  ( ifValid,
    lastError,
    updateBrickForm,
    updateBrickList,
  )
import Types.Monads
import UI.Environments.Add
  ( finishAddingEnvironment,
    showEnvironmentAddScreen,
  )
import UI.Environments.Edit
  ( finishEditingEnvironment,
    showEnvironmentEditScreen,
  )
import UI.Environments.List (showEnvironmentListScreen)
import UI.List (AppList (..))
import UI.RequestDefs.Details (refreshResponseList)
import Utils.IfThenElse (ifThenElse)
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

handleEventEnvironmentAdd ::
  (IxMonadState m, IxMonadEvent m, IxMonadIO m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'EnvironmentAddTag) AnyAppState ()
handleEventEnvironmentAdd key mods chan = do
  s <- iget
  case (key, mods) of
    (KChar 's', [MCtrl]) -> ifValid $ sm $ do
      eid <- iliftIO $ EnvironmentId <$> nextRandom
      finishAddingEnvironment eid
      sendEvent Save chan
      showEnvironmentListScreen
    (KEsc, []) -> showEnvironmentListScreen >>> submerge
    _ -> sm $ do
      extractScreen
      updateBrickForm key
      wrapScreen s

-- Sets the provided environment (or no environment, in the case of Nothing) as the active one.
-- This necessitates a reset of the currently displayed error, if the active screen happens to be
-- the RequestDef details screen, and the environment is actually changing.
selectEnvironment ::
  (IxMonadState m, IxMonadIO m, SingI a) =>
  Maybe EnvironmentContext ->
  BChan CustomEvent ->
  m (AppState a) AnyAppState ()
selectEnvironment c chan = do
  s <- iget
  let currentEnv = s ^. environmentContext
      isChanging = currentEnv /= c
  imodify (environmentContext .~ c)
  sendEvent Save chan
  unstashScreen
  if isChanging then refreshIfNecessary else ireturn ()

-- Selecting a new environment necessitates a refresh of the screen if the current screen happens to be the
-- request def details screen
refreshIfNecessary :: IxMonadState m => m AnyAppState AnyAppState ()
refreshIfNecessary = do
  (AnyAppState _ s) <- iget
  case s ^. screen of
    RequestDefDetailsScreen {} -> sm $ do
      iput s
      refreshResponseList
      imodify $ screen . lastError .~ Nothing
    _ -> ireturn ()

handleEventEnvironmentList ::
  (IxMonadState m, IxMonadIO m, IxMonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'EnvironmentListTag) AnyAppState ()
handleEventEnvironmentList key mods chan = do
  s <- iget
  let EnvironmentListScreen (AppList list) = s ^. screen
      selectedEnv = snd <$> listSelectedElement list
  case (key, mods) of
    (KEnter, []) -> case selectedEnv of
      Just (AnEnvironment c _) -> selectEnvironment (Just c) chan
      Just NoEnvironment -> selectEnvironment Nothing chan
      Nothing -> submerge
    (KChar 'd', []) -> case selectedEnv of
      Just (AnEnvironment c _) -> sm $ imodify (modal ?~ DeleteEnvironmentModal c)
      _ -> submerge
    (KChar 'e', []) -> case selectedEnv of
      Just (AnEnvironment c _) -> sm $ showEnvironmentEditScreen c
      _ -> submerge
    (KChar 'a', []) -> sm showEnvironmentAddScreen
    _ -> sm $ do
      extractScreen
      updateBrickList key
      wrapScreen s

handleEventEnvironmentEdit ::
  (IxMonadState m, IxMonadIO m, IxMonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'EnvironmentEditTag) AnyAppState ()
handleEventEnvironmentEdit key mods chan = do
  s <- iget
  case (key, mods) of
    (KChar 's', [MCtrl]) -> ifValid $ sm $ do
      finishEditingEnvironment
      sendEvent Save chan
      showEnvironmentListScreen
    (KEsc, []) -> sm showEnvironmentListScreen
    _ -> sm $ do
      extractScreen
      updateBrickForm key
      wrapScreen s
