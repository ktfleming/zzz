{-# LANGUAGE TypeApplications #-}
module UI.Events.RequestDefs where

import           Brick                          ( BrickEvent(VtyEvent)
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
import           Types.Aliases                  ( EventHandlerFunction )
import           Types.AppState
import           Types.Brick.Name
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectContext(..) )
import           Types.Models.RequestDef        ( RequestDefContext(..) )
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
import           Control.Monad.Trans.State      ( get
                                                , modify
                                                )
import           Types.Classes.HasId            ( model )

handleEventRequestAdd :: EventHandlerFunction
handleEventRequestAdd ev@(VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    RequestDefAddScreen c form -> case key of
      KEnter -> finishAddingRequestDef c (formState form)
      KEsc   -> showProjectDetails c
      _      -> updateAddRequestDefForm form ev
    _ -> return ()

handleEventRequestAdd _ = return ()

handleEventRequestEdit :: EventHandlerFunction
handleEventRequestEdit ev@(VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    RequestDefEditScreen c form -> case key of
      KEnter ->
        finishEditingRequestDef c (model s c) form >> showRequestDefDetails c
      KEsc -> showRequestDefDetails c
      _    -> updateEditRequestDefForm form ev
    _ -> return ()

handleEventRequestEdit _ = return ()

handleEventRequestDetails :: EventHandlerFunction
handleEventRequestDetails (VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    RequestDefDetailsScreen c list ring -> case key of
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
    _ -> return ()

handleEventRequestDetails _ = return ()
