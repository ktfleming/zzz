{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Events.RequestDefs where

import           Brick                          ( EventM
                                                , vScrollBy
                                                , viewportScroll
                                                )
import           Brick.BChan                    ( BChan )
import           Brick.Focus                    ( focusGetCurrent
                                                , focusNext
                                                , focusPrev
                                                )
import           Control.Lens
import           Control.Monad.Indexed          ( ireturn
                                                , (>>>=)
                                                )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                     ( isNothing )
import           Graphics.Vty.Input.Events
import           Request.Request                ( cancelRequest
                                                , sendRequest
                                                )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent(..) )
import           Types.Brick.Name
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectContext(..) )
import           Types.Models.RequestDef        ( RequestDefContext(..) )
import           Types.Models.Screen
import           UI.Events.BrickUpdates         ( updateBrickForm
                                                , updateBrickList
                                                )
import           UI.Projects.Details            ( showProjectDetails )
import           UI.RequestDefs.Add             ( finishAddingRequestDef )
import           UI.RequestDefs.Details         ( showRequestDefDetails )
import           UI.RequestDefs.Edit            ( finishEditingRequestDef
                                                , showEditRequestDefScreen
                                                )
import           Utils.IxState                  ( extractScreen
                                                , save
                                                , submerge
                                                , wrapScreen
                                                , (>>>)
                                                )

handleEventRequestAdd
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'RequestDefAddTag) AnyAppState ()
handleEventRequestAdd key mods chan = iget >>>= \s ->
  let RequestDefAddScreen c _ = s ^. screen
  in  case (key, mods) of
        (KChar 's', [MCtrl]) ->
          finishAddingRequestDef >>> save chan >>> showProjectDetails c >>> submerge
        (KEsc, []) -> showProjectDetails c >>> submerge
        _          -> extractScreen >>> updateBrickForm key >>> wrapScreen s >>> submerge

handleEventRequestEdit
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'RequestDefEditTag) AnyAppState ()
handleEventRequestEdit key mods chan = iget >>>= \s ->
  let RequestDefEditScreen c _ = s ^. screen
  in  case (key, mods) of
        (KChar 's', [MCtrl]) ->
          finishEditingRequestDef >>> save chan >>> showRequestDefDetails c >>> submerge
        (KEsc, []) -> showRequestDefDetails c >>> submerge
        _          -> extractScreen >>> updateBrickForm key >>> wrapScreen s >>> submerge

handleEventRequestDetails
  :: Key
  -> [Modifier]
  -> BChan CustomEvent
  -> IxStateT (EventM Name) (AppState 'RequestDefDetailsTag) AnyAppState ()
handleEventRequestDetails key mods chan = iget >>>= \s ->
  let
    RequestDefDetailsScreen c@(RequestDefContext _ rid) list ring = s ^. screen
    focused       = focusGetCurrent ring
    activeRequest = Map.lookup rid (s ^. activeRequests)
  in
    case (key, mods) of
      (KLeft, []) ->
        let (RequestDefContext pid _) = c in showProjectDetails (ProjectContext pid) >>> submerge
      (KChar 'x', []) -> maybe (ireturn ()) (cancelRequest c) activeRequest >>> submerge
      (KChar 'e', []) -> showEditRequestDefScreen c >>> submerge
      (KChar 'd', []) -> imodify (modal ?~ DeleteRequestDefModal c) >>> submerge
      (KEnter   , []) -> if focused == Just RequestDetails && isNothing activeRequest
        then sendRequest c chan >>> submerge
        else ireturn () >>> submerge
      (KChar '\t', []) ->
        imodify (screen .~ RequestDefDetailsScreen c list (focusNext ring)) >>> submerge -- TODO: HasFocusRing typeclass w/ modify method, similar to HasBrickForm
      (KBackTab, []) ->
        imodify (screen .~ RequestDefDetailsScreen c list (focusPrev ring)) >>> submerge
      _ -> case focused of
        Just ResponseList -> extractScreen >>> updateBrickList key >>> wrapScreen s >>> submerge
        Just ResponseBodyDetails ->
          let vp = viewportScroll ResponseBodyViewport
          in  case key of
                KUp   -> ilift (vScrollBy vp (-5)) >>> submerge
                KDown -> ilift (vScrollBy vp 5) >>> submerge
                _     -> ireturn () >>> submerge
        _ -> ireturn () >>> submerge
