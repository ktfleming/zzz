{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

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
import Types.Config.Config
import Types.Modal (Modal (..))
import Types.Models.Id (RequestDefId (..))
import Types.Models.Project (ProjectContext (..))
import Types.Models.RequestDef (RequestDefContext (..))
import Types.Models.Response (ResponseIndex (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
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
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'RequestDefAddTag ->
  m AnyAppState
handleEventRequestAdd key mods chan s =
  let c = s ^. screen ^. context
      doAdd s' = do
        rid <- liftIO $ RequestDefId <$> nextRandom
        saveAfter chan $ pure . wrap . showProjectDetails c . finishAddingRequestDef rid $ s'
   in case (key, mods) of
        (KChar 's', [MCtrl]) -> ifValid doAdd
        (KEsc, []) -> pure . wrap . showProjectDetails c
        _ -> pure . wrap <=< updateBrickForm key
        $ s

handleEventRequestEdit ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'RequestDefEditTag ->
  m AnyAppState
handleEventRequestEdit key mods chan s =
  let c = s ^. screen ^. context
      doEdit = saveAfter chan . pure . wrap . showRequestDefDetails c . finishEditingRequestDef
   in case (key, mods) of
        (KChar 's', [MCtrl]) -> ifValid doEdit
        (KEsc, []) -> pure . wrap . showRequestDefDetails c
        _ -> pure . wrap <=< updateBrickForm key
        $ s

handleEventRequestDetails ::
  (MonadReader Config m, MonadEvent m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  AppState 'RequestDefDetailsTag ->
  m AnyAppState
handleEventRequestDetails key mods chan s =
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
   in case (key, mods) of
        (KLeft, []) ->
          let (RequestDefContext pid _) = c
           in pure . wrap . showProjectDetails (ProjectContext pid)
        (KChar 'x', []) ->
          case activeRequest of
            Just toCancel -> fmap wrap . cancelRequest c toCancel
            Nothing -> pure . wrap
        (KChar 'e', []) -> pure . wrap . showEditRequestDefScreen c
        (KChar 'd', []) -> case (focused, selectedResponse) of
          (Just ResponseList, Just i) -> pure . wrap . (modal ?~ DeleteResponseModal c i)
          (Just ResponseBodyDetails, Just i) -> pure . wrap . (modal ?~ DeleteResponseModal c i)
          _ -> pure . wrap . (modal ?~ DeleteRequestDefModal c)
        (KEnter, []) ->
          if focused == Just RequestDetails && isNothing activeRequest
            then fmap wrap . sendRequest c chan
            else pure . wrap
        (KChar '\t', []) -> fmap wrap . modifyFocus focusNext
        (KBackTab, []) -> fmap wrap . modifyFocus focusPrev
        _ -> case focused of
          Just ResponseList -> pure . wrap <=< updateBrickList key
          Just ResponseBodyDetails ->
            let vp = viewportScroll ResponseBodyViewport
             in case key of
                  KUp -> \s' -> liftEvent () (vScrollBy vp (-5)) >> pure (wrap s')
                  KDown -> \s' -> liftEvent () (vScrollBy vp 5) >> pure (wrap s')
                  _ -> pure . wrap
          _ -> pure . wrap
        $ s
