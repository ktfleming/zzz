{-# LANGUAGE TypeApplications #-}
module UI.Events.RequestDefinitions where

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
import           Types.Models.RequestDefinition ( RequestDefinitionContext(..) )
import           Types.Models.Screen
import           UI.Projects.Details            ( showProjectDetails )
import           UI.RequestDefinitions.Add      ( finishAddingRequestDefinition
                                                , updateAddRequestDefinitionForm
                                                )
import           UI.RequestDefinitions.Details  ( showRequestDefinitionDetails
                                                , updateResponseList
                                                )
import           UI.RequestDefinitions.Edit     ( finishEditingRequestDefinition
                                                , showEditRequestDefinitionScreen
                                                , updateEditRequestDefinitionForm
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
    RequestAddScreen c form -> case key of
      KEnter -> finishAddingRequestDefinition c (formState form)
      KEsc   -> showProjectDetails c
      _      -> updateAddRequestDefinitionForm form ev
    _ -> return ()

handleEventRequestAdd _ = return ()

handleEventRequestEdit :: EventHandlerFunction
handleEventRequestEdit ev@(VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    RequestEditScreen c form -> case key of
      KEnter -> finishEditingRequestDefinition c (model s c) form
        >> showRequestDefinitionDetails c
      KEsc -> showRequestDefinitionDetails c
      _    -> updateEditRequestDefinitionForm form ev
    _ -> return ()

handleEventRequestEdit _ = return ()

handleEventRequestDetails :: EventHandlerFunction
handleEventRequestDetails (VtyEvent (EvKey key [])) = do
  s <- get
  case s ^. screen of
    RequestDetailsScreen c list ring -> case key of
      KLeft ->
        let (RequestDefinitionContext pid _) = c
        in  showProjectDetails (ProjectContext pid)
      KChar 'e' -> showEditRequestDefinitionScreen c
      KChar 'd' -> modify $ modal ?~ DeleteRequestDefinitionModal c
      KEnter    -> sendRequest c
      KChar '\t' ->
        modify
          $  screen
          .  _RequestDetailsScreen
          .  typed @(FocusRing Name)
          %~ focusNext
      KBackTab ->
        modify
          $  screen
          .  _RequestDetailsScreen
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
