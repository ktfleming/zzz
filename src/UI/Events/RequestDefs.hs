{-# LANGUAGE TypeApplications #-}
module UI.Events.RequestDefs where

import           Brick                          ( BrickEvent(VtyEvent)
                                                , EventM
                                                , vScrollBy
                                                , viewportScroll
                                                )
import           Brick.Focus                    ( FocusRing
                                                , focusGetCurrent
                                                , focusNext
                                                , focusPrev
                                                )
import           Brick.Forms                    ( formState )
import           Control.Lens
import           Data.Generics.Product.Typed    ( typed )
import           Graphics.Vty.Input.Events
import           Request.Request                ( sendRequest )
import           Types.AppState
import           Types.Brick.Name
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectContext(..) )
import           Types.Models.RequestDef        ( RequestDefContext(..)
                                                , RequestDefFormState
                                                )
import           Types.Models.Screen
import           UI.Projects.Details            ( showProjectDetails )
import           UI.RequestDefs.Add             ( finishAddingRequestDef
                                                , updateAddRequestDefForm
                                                )
import           UI.RequestDefs.Details         ( showRequestDefDetails
                                                , updateResponseList
                                                )
import           UI.RequestDefs.Edit            ( finishEditingRequestDef
                                                , showEditRequestDefScreen
                                                , updateEditRequestDefForm
                                                )

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State      ( modify )
import           Control.Monad.Trans.State.Lazy ( StateT )
import           Types.Models.Response          ( Response )
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )

handleEventRequestAdd
  :: ProjectContext
  -> ZZZForm RequestDefFormState
  -> Key
  -> StateT AppState (EventM Name) ()
handleEventRequestAdd c form key = case key of
  KEnter -> finishAddingRequestDef c (formState form)
  KEsc   -> showProjectDetails c
  _      -> updateAddRequestDefForm form (VtyEvent (EvKey key []))

handleEventRequestEdit
  :: RequestDefContext
  -> ZZZForm RequestDefFormState
  -> Key
  -> StateT AppState (EventM Name) ()
handleEventRequestEdit c form key = case key of
  KEnter -> finishEditingRequestDef c form >> showRequestDefDetails c
  KEsc   -> showRequestDefDetails c
  _      -> updateEditRequestDefForm form (VtyEvent (EvKey key []))

handleEventRequestDetails
  :: RequestDefContext
  -> ZZZList Response
  -> FocusRing Name
  -> Key
  -> StateT AppState (EventM Name) ()
handleEventRequestDetails c list ring key = case key of
  KLeft ->
    let (RequestDefContext pid _) = c
    in  showProjectDetails (ProjectContext pid)
  KChar 'e' -> showEditRequestDefScreen c
  KChar 'd' -> modify $ modal ?~ DeleteRequestDefModal c
  KEnter    -> sendRequest c
  KChar '\t' ->
    modify
      $  screen
      .  _RequestDefDetailsScreen
      .  typed @(FocusRing Name)
      %~ focusNext
  KBackTab ->
    modify
      $  screen
      .  _RequestDefDetailsScreen
      .  typed @(FocusRing Name)
      %~ focusPrev
  _ -> case focusGetCurrent ring of
    Just ResponseList -> updateResponseList list key
    Just ResponseBody ->
      let vp = viewportScroll ResponseBodyViewport
      in  case key of
            KUp   -> lift $ vScrollBy vp (-1)
            KDown -> lift $ vScrollBy vp 1
            _     -> return ()
    _ -> return ()
