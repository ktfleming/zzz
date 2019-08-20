{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Events.RequestDefs where

import           Brick                          ( EventM
                                                , vScrollBy
                                                , viewportScroll
                                                )
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
import           Graphics.Vty.Input.Events
import           Request.Request                ( sendRequest )
import           Types.AppState
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
import           Utils.IxState                  ( runOnScreen
                                                , submerge
                                                , (>>>)
                                                )

handleEventRequestAdd :: Key -> IxStateT (EventM Name) (AppState 'RequestDefAddTag) AnyAppState ()
handleEventRequestAdd key = iget >>>= \s ->
  let RequestDefAddScreen c _ = s ^. screen
  in  case key of
        KEnter -> submerge finishAddingRequestDef
        KEsc   -> submerge $ showProjectDetails c
        _      -> runOnScreen $ updateBrickForm key

handleEventRequestEdit
  :: Key -> IxStateT (EventM Name) (AppState 'RequestDefEditTag) AnyAppState ()
handleEventRequestEdit key = iget >>>= \s ->
  let RequestDefEditScreen c _ = s ^. screen
  in  case key of
        KEnter -> finishEditingRequestDef >>> submerge (showRequestDefDetails c)
        KEsc   -> submerge $ showRequestDefDetails c
        _      -> runOnScreen $ updateBrickForm key

handleEventRequestDetails
  :: Key -> IxStateT (EventM Name) (AppState 'RequestDefDetailsTag) AnyAppState ()
handleEventRequestDetails key = iget >>>= \s ->
  let RequestDefDetailsScreen c list ring = s ^. screen
  in
    case key of
      KLeft ->
        let (RequestDefContext pid _) = c in submerge $ showProjectDetails (ProjectContext pid)
      KChar 'e'  -> submerge $ showEditRequestDefScreen c
      KChar 'd'  -> submerge $ imodify $ modal ?~ DeleteRequestDefModal c
      KEnter     -> submerge $ sendRequest c
      KChar '\t' -> submerge $ imodify $ screen .~ RequestDefDetailsScreen c list (focusNext ring) -- TODO: better way of doing this, without setting the whole screen?
      KBackTab   -> submerge $ imodify $ screen .~ RequestDefDetailsScreen c list (focusPrev ring)
      _          -> case focusGetCurrent ring of
        Just ResponseList -> runOnScreen $ updateBrickList key
        Just ResponseBody ->
          let vp = viewportScroll ResponseBodyViewport
          in  case key of
                KUp   -> submerge $ ilift $ vScrollBy vp (-1)
                KDown -> submerge $ ilift $ vScrollBy vp 1
                _     -> submerge $ ireturn ()
        _ -> submerge $ ireturn ()
