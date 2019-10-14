{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Monad.Indexed (ireturn)
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import qualified Data.HashMap.Strict as Map
import Data.Maybe (isNothing)
import Data.UUID.V4 (nextRandom)
import Graphics.Vty.Input.Events
import Language.Haskell.DoNotation
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
  ( updateBrickForm,
    updateBrickList,
  )
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
import Utils.IfThenElse (ifThenElse)
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

handleEventRequestAdd ::
  (IxMonadState m, IxMonadEvent m, IxMonadIO m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'RequestDefAddTag) AnyAppState ()
handleEventRequestAdd key mods chan = do
  s <- iget
  let RequestDefAddScreen c _ = s ^. screen
  case (key, mods) of
    (KChar 's', [MCtrl]) -> sm $ do
      rid <- iliftIO $ RequestDefId <$> nextRandom
      finishAddingRequestDef rid
      sendEvent Save chan
      showProjectDetails c
    (KEsc, []) -> sm $ showProjectDetails c
    _ -> sm $ do
      extractScreen
      updateBrickForm key
      wrapScreen s

handleEventRequestEdit ::
  (IxMonadState m, IxMonadEvent m, IxMonadIO m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'RequestDefEditTag) AnyAppState ()
handleEventRequestEdit key mods chan = do
  s <- iget
  let RequestDefEditScreen c _ = s ^. screen
  case (key, mods) of
    (KChar 's', [MCtrl]) -> sm $ do
      finishEditingRequestDef
      sendEvent Save chan
      showRequestDefDetails c
    (KEsc, []) -> sm $ showRequestDefDetails c
    _ -> sm $ do
      extractScreen
      updateBrickForm key
      wrapScreen s

handleEventRequestDetails ::
  (IxMonadState m, IxMonadEvent m, IxMonadIO m) =>
  Key ->
  [Modifier] ->
  BChan CustomEvent ->
  m (AppState 'RequestDefDetailsTag) AnyAppState ()
handleEventRequestDetails key mods chan = do
  s <- iget
  let RequestDefDetailsScreen c@(RequestDefContext _ rid) (AppList list) (AppFocusRing ring) _ =
        s ^. screen
      focused = focusGetCurrent ring
      activeRequest = Map.lookup rid (s ^. activeRequests . coerced)
      ringLens :: Lens' (Screen 'RequestDefDetailsTag) (AppFocusRing Name)
      ringLens =
        lens
          (\(RequestDefDetailsScreen _ _ target _) -> target)
          (\(RequestDefDetailsScreen x y _ z) toSet -> RequestDefDetailsScreen x y toSet z)
      selectedResponse = ResponseIndex <$> listSelected list
      modifyFocus f = sm $ do
        imodify $ screen . ringLens %~ (\(AppFocusRing r) -> AppFocusRing (f r))
        iliftEvent () $ vScrollToBeginning (viewportScroll ResponseBodyViewport)
  case (key, mods) of
    (KLeft, []) ->
      sm $ let (RequestDefContext pid _) = c in showProjectDetails (ProjectContext pid)
    (KChar 'x', []) -> sm $ maybe (ireturn ()) (cancelRequest c) activeRequest
    (KChar 'e', []) -> sm $ showEditRequestDefScreen c
    (KChar 'd', []) -> case (focused, selectedResponse) of
      (Just ResponseList, Just i) -> sm $ imodify (modal ?~ DeleteResponseModal c i)
      (Just ResponseBodyDetails, Just i) -> sm $ imodify (modal ?~ DeleteResponseModal c i)
      _ -> sm $ imodify (modal ?~ DeleteRequestDefModal c)
    (KEnter, []) ->
      if focused == Just RequestDetails && isNothing activeRequest
        then sm $ sendRequest c chan
        else submerge
    (KChar '\t', []) -> modifyFocus focusNext
    (KBackTab, []) -> modifyFocus focusPrev
    _ -> case focused of
      Just ResponseList -> sm $ do
        extractScreen
        updateBrickList key
        wrapScreen s
      Just ResponseBodyDetails ->
        let vp = viewportScroll ResponseBodyViewport
         in sm $ case key of
              KUp -> iliftEvent () (vScrollBy vp (-5))
              KDown -> iliftEvent () (vScrollBy vp 5)
              _ -> ireturn ()
      _ -> submerge
