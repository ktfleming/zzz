{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module UI.Events.RequestDefinitions where

import           Brick                          ( BrickEvent(VtyEvent)
                                                , continue
                                                , vScrollBy
                                                , viewportScroll
                                                )
import           Brick.Focus                    ( FocusRing
                                                , focusGetCurrent
                                                , focusNext
                                                , focusPrev
                                                )
import           Brick.Forms                    ( formState
                                                , handleFormEvent
                                                )
import           Brick.Widgets.List             ( handleListEvent )
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Generics.Product.Typed    ( typed )
import qualified Data.Text                     as T
import           Graphics.Vty.Input.Events
import           Messages.Messages              ( logMessage )
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

handleEventRequestAdd :: EventHandlerFunction
handleEventRequestAdd s@AppState { appStateScreen = RequestAddScreen c form } ev@(VtyEvent (EvKey key []))
  = case key of
    KEnter ->
      liftIO (finishAddingRequestDefinition s c (formState form)) >>= continue
    KEsc -> continue $ showProjectDetails s c
    _ ->
      updateAddRequestDefinitionForm s <$> handleFormEvent ev form >>= continue
handleEventRequestAdd s _ = continue s

handleEventRequestEdit :: EventHandlerFunction
handleEventRequestEdit s@AppState { appStateScreen = RequestEditScreen c form } ev@(VtyEvent (EvKey key []))
  = case key of
    KEnter ->
      let updatedState = finishEditingRequestDefinition s c (formState form)
      in  continue $ showRequestDefinitionDetails updatedState c
    KEsc -> continue $ showRequestDefinitionDetails s c
    _ ->
      updateEditRequestDefinitionForm s <$> handleFormEvent ev form >>= continue
handleEventRequestEdit s _ = continue s

handleEventRequestDetails :: EventHandlerFunction
handleEventRequestDetails s@AppState { appStateScreen = RequestDetailsScreen c list ring } (VtyEvent (EvKey key []))
  = case key of
    KLeft ->
      let (RequestDefinitionContext pid _) = c
      in  continue $ showProjectDetails s (ProjectContext pid)
    KChar 'e' -> continue $ showEditRequestDefinitionScreen s c
    KChar 'd' -> continue $ (modal ?~ DeleteRequestDefinitionModal c) s
    KEnter    -> do
      result <- liftIO $ runExceptT $ sendRequest s c
      case result of
        Left e ->
          -- TODO: display error
          let errorMessage = "ERROR: " <> T.pack e
          in  liftIO (logMessage s errorMessage) >>= continue
        Right newState -> continue newState
    KChar '\t' ->
      continue
        $  s
        &  screen
        .  _RequestDetailsScreen
        .  typed @(FocusRing Name)
        %~ focusNext
    KBackTab ->
      continue
        $  s
        &  screen
        .  _RequestDetailsScreen
        .  typed @(FocusRing Name)
        %~ focusPrev
    _ -> case focusGetCurrent ring of
      Just ResponseList ->
        updateResponseList s
          <$> handleListEvent (EvKey key []) list
          >>= continue
      Just ResponseBody ->
        let vp = viewportScroll ResponseBodyViewport
        in  case key of
              KUp   -> vScrollBy vp (-1) >> continue s
              KDown -> vScrollBy vp 1 >> continue s
              _     -> continue s
      _ -> continue s
handleEventRequestDetails s _ = continue s
