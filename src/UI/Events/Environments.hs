{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module UI.Events.Environments where

import Brick.BChan (BChan)
import Brick.Widgets.List (listSelectedElement)
import qualified Config
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.UUID.V4 (nextRandom)
import Graphics.Vty.Input.Events
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Classes.Fields
import Types.Modal
import Types.Models.Id (EnvironmentId (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import Types.Search
import UI.Environments.Add
import UI.Environments.Edit
import UI.Environments.List (showEnvironmentListScreen)
import UI.Events.Keys (matchKey)
import UI.List
import UI.Search.Common

handleEventEnvironmentAdd ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'EnvironmentAddTag ->
  m AnyAppState
handleEventEnvironmentAdd key mods chan s = do
  km <- asks (view keymap)
  let doAdd s' = do
        eid <- liftIO $ EnvironmentId <$> nextRandom
        saveAfter chan $ pure . wrap . showEnvironmentListScreen . finishAddingEnvironment eid $ s'
  if  | matchKey (km ^. save) key mods -> ifValid doAdd s
      | matchKey (km ^. back) key mods -> pure . wrap . showEnvironmentListScreen $ s
      | otherwise -> pure . wrap <=< updateBrickForm key $ s

handleEventEnvironmentList ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'EnvironmentListTag ->
  m AnyAppState
handleEventEnvironmentList key mods chan s = do
  km <- asks (view keymap)
  let AppList list = s ^. screen ^. searchTools ^. appList
      selectedEnv = snd <$> listSelectedElement list
  if  | matchKey (km ^. delete) key mods -> case selectedEnv of
        Just (SelectableResult (AnEnvironmentResult c _)) -> pure . wrap . (modal ?~ DeleteEnvironmentModal c) $ s
        _ -> pure . wrap $ s
      | matchKey (km ^. edit) key mods -> case selectedEnv of
        Just (SelectableResult (AnEnvironmentResult c _)) -> pure . wrap . showEnvironmentEditScreen c $ s
        _ -> pure . wrap $ s
      | matchKey (km ^. add) key mods -> pure . wrap . showEnvironmentAddScreen $ s
      | otherwise -> handleEventSearch key mods chan s

handleEventEnvironmentEdit ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'EnvironmentEditTag ->
  m AnyAppState
handleEventEnvironmentEdit key mods chan s = do
  km <- asks (view keymap)
  let doEdit = saveAfter chan . pure . wrap . showEnvironmentListScreen . finishEditingEnvironment
  if  | matchKey (km ^. save) key mods -> ifValid doEdit s
      | matchKey (km ^. back) key mods -> pure . wrap . showEnvironmentListScreen $ s
      | otherwise -> pure . wrap <=< updateBrickForm key $ s
