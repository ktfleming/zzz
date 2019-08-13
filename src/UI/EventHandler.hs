{-# LANGUAGE ScopedTypeVariables #-}

module UI.EventHandler
  ( handleEvent
  )
where

import           Brick                          ( BrickEvent(..)
                                                , EventM
                                                , Next
                                                , continue
                                                , halt
                                                )
import           Prelude                 hiding ( writeFile )

import           Types.Brick.CustomEvent        ( CustomEvent )

import           Types.Brick.Name               ( Name )

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
import           Graphics.Vty.Input.Events
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
import           UI.Projects.Details            ( showProjectDetails )
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
import           UI.RequestDefinitions.Details  ( showRequestDefinitionDetails )
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

handleEvent s@AppState { appStateModal = Just m } (VtyEvent (EvKey key [])) =
  case key of
    KChar 'n' -> continue $ dismissModal s
    KChar 'y' -> continue $ dismissModal $ handleConfirm s m
    _         -> continue s

handleEvent s ev@(VtyEvent (EvKey key [])) = case s ^. screen of
  RequestDetailsScreen c@(RequestDefinitionContext pid _) -> case key of
    KLeft     -> continue $ showProjectDetails s (ProjectContext pid)
    KChar 'e' -> continue $ showEditRequestDefinitionScreen s c
    KChar 'd' -> continue $ (modal ?~ DeleteRequestDefinitionModal c) s
    KEnter    -> do
      result :: Either String AppState <- liftIO $ runExceptT $ sendRequest s c
      case result of
        Left _ ->
          -- TODO: display error
          continue s
        Right newState -> continue newState
    _ -> continue s

  RequestEditScreen c form -> case key of
    KEnter ->
      let updatedState = finishEditingRequestDefinition s c (formState form)
      in  continue $ showRequestDefinitionDetails updatedState c
    KEsc -> continue $ showRequestDefinitionDetails s c
    _    -> handleFormEvent ev form
      >>= \f -> continue $ updateEditRequestDefinitionForm s c f

  ProjectListScreen list -> case key of
    KEnter -> case listSelectedElement list of
      Just (_, ProjectListItem context _) ->
        continue $ showProjectDetails s context
      Nothing -> continue s
    KChar 'a' -> continue $ showProjectAddScreen s
    _         -> handleListEvent (EvKey key []) list
      >>= \l -> continue $ updateProjectList s l

  ProjectDetailsScreen c list -> case key of
    KEnter -> case listSelectedElement list of
      Just (_, RequestDefinitionListItem reqContext _) ->
        continue $ showRequestDefinitionDetails s reqContext
      Nothing -> continue s
    KChar 'e' -> continue $ showEditProjectScreen s c
    KChar 'a' -> continue $ showAddRequestDefinitionScreen s c
    KChar 'd' -> continue $ (modal ?~ DeleteProjectModal c) s
    KLeft     -> continue $ showProjectListScreen s
    _         -> handleListEvent (EvKey key []) list
      >>= \l -> continue $ (screen .~ ProjectDetailsScreen c l) s

  ProjectEditScreen c form -> case key of
    KEnter ->
      let updatedState = finishEditingProject s c (formState form)
      in  continue $ showProjectDetails updatedState c
    KEsc -> continue $ showProjectDetails s c
    _ ->
      handleFormEvent ev form >>= \f -> continue $ updateEditProjectForm s c f

  ProjectAddScreen form -> case key of
    KEnter -> liftIO (finishAddingProject s (formState form)) >>= continue
    KEsc -> continue $ showProjectListScreen s
    _ -> handleFormEvent ev form >>= \f -> continue $ updateProjectAddForm s f

  RequestAddScreen c form -> case key of
    KEnter ->
      liftIO (finishAddingRequestDefinition s c (formState form)) >>= continue
    KEsc -> continue $ showProjectDetails s c
    _    -> handleFormEvent ev form
      >>= \f -> continue $ updateAddRequestDefinitionForm s c f

  _ -> continue s

handleEvent s _ = continue s

saveState :: AppState -> IO ()
saveState s = do
  _ <- writeFile mainSettingsFile (encodePretty s)
  _ <- writeFile responseHistoryFile (encodePretty (s ^. responses))
  return ()
