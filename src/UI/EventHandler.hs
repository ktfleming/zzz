{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module UI.EventHandler
  ( handleEvent
  )
where

import           Brick                          ( BrickEvent(..)
                                                , EventM
                                                , Next
                                                , continue
                                                , halt
                                                , vScrollBy
                                                , viewportScroll
                                                )
import           Prelude                 hiding ( writeFile )

import           Types.Brick.CustomEvent        ( CustomEvent )

import           Types.Brick.Name               ( Name(..) )

import           Brick.Focus                    ( FocusRing
                                                , focusGetCurrent
                                                , focusNext
                                                , focusPrev
                                                )
import           Brick.Forms                    ( formState
                                                , handleFormEvent
                                                )
import           Brick.Widgets.List             ( handleListEvent
                                                , listSelectedElement
                                                )
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import           Data.Generics.Product.Typed    ( typed )
import qualified Data.Text                     as T
import           Graphics.Vty.Input.Events
import           Messages.Messages              ( logMessage )
import           Request.Request                ( sendRequest )
import           Types.AppState
import           Types.Constants                ( mainSettingsFile
                                                , responseHistoryFile
                                                )
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectContext(..)
                                                , ProjectListItem(..)
                                                )
import           Types.Models.RequestDefinition ( RequestDefinitionContext(..)
                                                , RequestDefinitionListItem(..)
                                                )
import           Types.Models.Screen
import           UI.Console                     ( toggleConsole )
import           UI.Modal                       ( dismissModal
                                                , handleConfirm
                                                )
import           UI.Projects.Add                ( finishAddingProject
                                                , showProjectAddScreen
                                                , updateProjectAddForm
                                                )
import           UI.Projects.Details            ( showProjectDetails
                                                , updateProjectDetailsList
                                                )
import           UI.Projects.Edit               ( finishEditingProject
                                                , showEditProjectScreen
                                                , updateEditProjectForm
                                                )
import           UI.Projects.List               ( showProjectListScreen
                                                , updateProjectList
                                                )
import           UI.RequestDefinitions.Add      ( finishAddingRequestDefinition
                                                , showAddRequestDefinitionScreen
                                                , updateAddRequestDefinitionForm
                                                )
import           UI.RequestDefinitions.Details  ( showRequestDefinitionDetails
                                                , updateResponseList
                                                )
import           UI.RequestDefinitions.Edit     ( finishEditingRequestDefinition
                                                , showEditRequestDefinitionScreen
                                                , updateEditRequestDefinitionForm
                                                )

handleEvent
  :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s
handleEvent s (VtyEvent (EvKey (KChar 'e') [MCtrl])) =
  continue $ toggleConsole s
handleEvent s (VtyEvent (EvKey (KChar 'p') [MCtrl])) =
  continue $ s & helpPanelVisible . coerced %~ not

handleEvent s@AppState { appStateModal = Just m } (VtyEvent (EvKey key [])) =
  case key of
    KChar 'n' -> continue $ dismissModal s
    KChar 'y' -> continue $ dismissModal $ handleConfirm s m
    _         -> continue s

handleEvent s ev@(VtyEvent (EvKey key [])) = case s ^. screen of
  RequestDetailsScreen c@(RequestDefinitionContext pid _) list ring ->
    case key of
      KLeft     -> continue $ showProjectDetails s (ProjectContext pid)
      KChar 'e' -> continue $ showEditRequestDefinitionScreen s c
      KChar 'd' -> continue $ (modal ?~ DeleteRequestDefinitionModal c) s
      KEnter    -> do
        result :: Either String AppState <- liftIO $ runExceptT $ sendRequest
          s
          c
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

  RequestEditScreen c form -> case key of
    KEnter ->
      let updatedState = finishEditingRequestDefinition s c (formState form)
      in  continue $ showRequestDefinitionDetails updatedState c
    KEsc -> continue $ showRequestDefinitionDetails s c
    _ ->
      updateEditRequestDefinitionForm s <$> handleFormEvent ev form >>= continue

  ProjectListScreen list -> case key of
    KEnter -> case listSelectedElement list of
      Just (_, ProjectListItem context _) ->
        continue $ showProjectDetails s context
      Nothing -> continue s
    KChar 'a' -> continue $ showProjectAddScreen s
    _ ->
      updateProjectList s <$> handleListEvent (EvKey key []) list >>= continue

  ProjectDetailsScreen c list -> case key of
    KEnter -> case listSelectedElement list of
      Just (_, RequestDefinitionListItem reqContext _) ->
        continue $ showRequestDefinitionDetails s reqContext
      Nothing -> continue s
    KChar 'e' -> continue $ showEditProjectScreen s c
    KChar 'a' -> continue $ showAddRequestDefinitionScreen s c
    KChar 'd' -> continue $ (modal ?~ DeleteProjectModal c) s
    KLeft     -> continue $ showProjectListScreen s
    _ ->
      updateProjectDetailsList s
        <$> handleListEvent (EvKey key []) list
        >>= continue

  ProjectEditScreen c form -> case key of
    KEnter ->
      let updatedState = finishEditingProject s c (formState form)
      in  continue $ showProjectDetails updatedState c
    KEsc -> continue $ showProjectDetails s c
    _    -> updateEditProjectForm s <$> handleFormEvent ev form >>= continue

  ProjectAddScreen form -> case key of
    KEnter -> liftIO (finishAddingProject s (formState form)) >>= continue
    KEsc   -> continue $ showProjectListScreen s
    _      -> updateProjectAddForm s <$> handleFormEvent ev form >>= continue

  RequestAddScreen c form -> case key of
    KEnter ->
      liftIO (finishAddingRequestDefinition s c (formState form)) >>= continue
    KEsc -> continue $ showProjectDetails s c
    _ ->
      updateAddRequestDefinitionForm s <$> handleFormEvent ev form >>= continue

  _ -> continue s

handleEvent s _ = continue s

saveState :: AppState -> IO ()
saveState s = do
  _ <- writeFile mainSettingsFile (encodePretty s)
  _ <- writeFile responseHistoryFile (encodePretty (s ^. responses))
  return ()
