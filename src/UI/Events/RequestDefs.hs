{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module UI.Events.RequestDefs where

import Brick
  ( vScrollBy,
    vScrollToBeginning,
    viewportScroll,
  )
import Brick.BChan (BChan)
import Brick.Focus
  ( focusGetCurrent,
    focusNext,
    focusPrev,
  )
import Brick.Widgets.List (listSelected)
import qualified Config
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as Map
import Data.Maybe (isNothing)
import Data.UUID.V4 (nextRandom)
import Graphics.Vty.Input.Events
import Request.Request
  ( cancelRequest,
    sendRequest,
  )
import Types.AppState
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Brick.Name
import Types.Classes.Fields
import Types.Modal (Modal (..))
import Types.Models.Id (RequestDefId (..))
import Types.Models.Project (ProjectContext (..))
import Types.Models.RequestDef (RequestDefContext (..))
import Types.Models.Response (ResponseIndex (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import UI.Events.Keys (matchKey)
import UI.FocusRing (AppFocusRing (..))
import UI.List (AppList (..))
import UI.Projects.Details (showProjectDetails)
import UI.RequestDefs.Add (finishAddingRequestDef)
import UI.RequestDefs.Details (showRequestDefDetails)
import UI.RequestDefs.Edit
  ( finishEditingRequestDef,
    showEditRequestDefScreen,
  )

handleEventRequestAdd ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'RequestDefAddTag ->
  m AnyAppState
handleEventRequestAdd key mods chan s = do
  km <- asks (view keymap)
  let c = s ^. screen ^. context
      doAdd s' = do
        rid <- liftIO $ RequestDefId <$> nextRandom
        saveAfter chan $ pure . wrap . showProjectDetails c . finishAddingRequestDef rid $ s'
  if  | matchKey (km ^. save) key mods -> ifValid doAdd s
      | matchKey (km ^. back) key mods -> pure . wrap . showProjectDetails c $ s
      | otherwise -> pure . wrap <=< updateBrickForm key $ s

handleEventRequestEdit ::
  (MonadEvent m, MonadReader Config.AppConfig m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'RequestDefEditTag ->
  m AnyAppState
handleEventRequestEdit key mods chan s = do
  km <- asks (view keymap)
  let c = s ^. screen ^. context
      doEdit = saveAfter chan . pure . wrap . showRequestDefDetails c . finishEditingRequestDef
  if  | matchKey (km ^. save) key mods -> ifValid doEdit s
      | matchKey (km ^. back) key mods -> pure . wrap . showRequestDefDetails c $ s
      | otherwise -> pure . wrap <=< updateBrickForm key $ s

handleEventRequestDetails ::
  (MonadReader Config.AppConfig m, MonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'RequestDefDetailsTag ->
  m AnyAppState
handleEventRequestDetails key mods chan s = do
  km <- asks (view keymap)
  let RequestDefDetailsScreen c@(RequestDefContext _ rid) (AppList list) (AppFocusRing ring) _ = s ^. screen
      focused = focusGetCurrent ring
      activeRequest = Map.lookup rid (s ^. activeRequests . coerced)
      ringLens :: Lens' (Screen 'RequestDefDetailsTag) (AppFocusRing Name)
      ringLens =
        lens
          (\(RequestDefDetailsScreen _ _ target _) -> target)
          (\(RequestDefDetailsScreen c' l' _ e') r' -> RequestDefDetailsScreen c' l' r' e')
      selectedResponse = ResponseIndex <$> listSelected list
      modifyFocus f s' = do
        liftEvent () $ vScrollToBeginning (viewportScroll ResponseBodyViewport)
        pure $ s' & screen . ringLens %~ (\(AppFocusRing r) -> AppFocusRing (f r))
  if  | matchKey (km ^. back) key mods ->
        let (RequestDefContext pid _) = c
         in pure . wrap . showProjectDetails (ProjectContext pid) $ s
      | matchKey (km ^. cancel) key mods ->
        case activeRequest of
          Just toCancel -> fmap wrap . cancelRequest c toCancel $ s
          Nothing -> pure . wrap $ s
      | matchKey (km ^. edit) key mods -> pure . wrap . showEditRequestDefScreen c $ s
      | matchKey (km ^. delete) key mods -> case (focused, selectedResponse) of
        (Just ResponseList, Just i) -> pure . wrap . (modal ?~ DeleteResponseModal c i) $ s
        (Just ResponseBodyDetails, Just i) -> pure . wrap . (modal ?~ DeleteResponseModal c i) $ s
        _ -> pure . wrap . (modal ?~ DeleteRequestDefModal c) $ s
      | matchKey (km ^. submit) key mods ->
        if focused == Just RequestDetails && isNothing activeRequest
          then fmap wrap . sendRequest c chan $ s
          else pure . wrap $ s
      | matchKey (km ^. Types.Classes.Fields.focusNext) key mods ->
        fmap wrap . modifyFocus Brick.Focus.focusNext $ s
      | matchKey (km ^. Types.Classes.Fields.focusPrev) key mods ->
        fmap wrap . modifyFocus Brick.Focus.focusPrev $ s
      | otherwise -> case focused of
        Just ResponseList -> pure . wrap <=< updateBrickList key $ s
        Just ResponseBodyDetails ->
          let vp = viewportScroll ResponseBodyViewport
           in if  | matchKey (km ^. scrollUp) key mods ->
                    liftEvent () (vScrollBy vp (-5)) >> pure (wrap s)
                  | matchKey (km ^. scrollDown) key mods ->
                    liftEvent () (vScrollBy vp 5) >> pure (wrap s)
                  | otherwise -> pure . wrap $ s
        _ -> pure . wrap $ s
