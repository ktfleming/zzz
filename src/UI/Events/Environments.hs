{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Events.Environments where

import           Brick.BChan                    ( BChan )
import           Brick.Types                    ( EventM )
import           Brick.Widgets.List             ( listSelectedElement )
import           Control.Lens
import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Graphics.Vty.Input.Events
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name               ( Name )
import           Types.Modal
import           Types.Models.Environment       ( EnvironmentListItem(..) )
import           Types.Models.Screen
import           UI.Environments.Add            ( finishAddingEnvironment
                                                , showEnvironmentAddScreen
                                                )
import           UI.Environments.Edit           ( finishEditingEnvironment
                                                , showEnvironmentEditScreen
                                                )
import           UI.Environments.List           ( showEnvironmentListScreen )
import           UI.Events.BrickUpdates         ( updateBrickForm
                                                , updateBrickList
                                                )
import           UI.Projects.List               ( showProjectListScreen )
import           Utils.IxState                  ( extractScreen
                                                , save
                                                , submerge
                                                , wrapScreen
                                                , (>>>)
                                                )

handleEventEnvironmentAdd
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'EnvironmentAddTag) AnyAppState ()
handleEventEnvironmentAdd key mods chan = iget >>>= \s -> case (key, mods) of
  (KChar 's', [MCtrl]) ->
    finishAddingEnvironment >>> save chan >>> showEnvironmentListScreen >>> submerge
  (KEsc, []) -> showEnvironmentListScreen >>> submerge
  _          -> extractScreen >>> updateBrickForm key >>> wrapScreen s >>> submerge

handleEventEnvironmentList
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'EnvironmentListTag) AnyAppState ()
handleEventEnvironmentList key mods chan = iget >>>= \s ->
  let EnvironmentListScreen list = s ^. screen
      selectedEnv                = snd <$> listSelectedElement list
  in  case (key, mods) of
        (KEnter, []) -> case selectedEnv of
          Just (AnEnvironment c _) ->
            imodify (environmentContext ?~ c) >>> save chan >>> showProjectListScreen >>> submerge
          Just NoEnvironment ->
            imodify (environmentContext .~ Nothing)
              >>> save chan
              >>> showProjectListScreen
              >>> submerge
          Nothing -> submerge
        (KChar 'd', []) -> case selectedEnv of
          Just (AnEnvironment c _) -> imodify (modal ?~ DeleteEnvironmentModal c) >>> submerge
          _                        -> submerge
        (KChar 'e', []) -> case selectedEnv of
          Just (AnEnvironment c _) -> showEnvironmentEditScreen c >>> submerge
          _                        -> submerge
        (KChar 'a', []) -> showEnvironmentAddScreen >>> submerge
        _               -> extractScreen >>> updateBrickList key >>> wrapScreen s >>> submerge

handleEventEnvironmentEdit
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'EnvironmentEditTag) AnyAppState ()
handleEventEnvironmentEdit key mods chan = iget >>>= \s -> case (key, mods) of
  (KChar 's', [MCtrl]) ->
    finishEditingEnvironment >>> save chan >>> showEnvironmentListScreen >>> submerge
  (KEsc, []) -> showEnvironmentListScreen >>> submerge
  _          -> extractScreen >>> updateBrickForm key >>> wrapScreen s >>> submerge
