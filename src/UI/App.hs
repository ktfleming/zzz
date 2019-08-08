{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.App where

import           Prelude                 hiding ( writeFile )

import           Brick                          ( App(..)
                                                , AttrMap
                                                , BrickEvent(VtyEvent)
                                                , BrickEvent
                                                , CursorLocation
                                                , EventM
                                                , Next
                                                , Widget
                                                , attrMap
                                                , continue
                                                , halt
                                                , joinBorders
                                                , padBottom
                                                , txt
                                                , withBorderStyle
                                                , (<=>)
                                                )
import           Brick.Forms                    ( formState
                                                , handleFormEvent
                                                , renderForm
                                                )
import           Brick.Types                    ( Padding(Max) )
import           Brick.Util
import           Brick.Widgets.Border           ( border
                                                , hBorder
                                                )
import           Brick.Widgets.Border.Style     ( unicodeRounded )
import           Brick.Widgets.List             ( handleListEvent
                                                , listSelectedElement
                                                , listSelectedFocusedAttr
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import           Data.Maybe                     ( maybeToList )
import           Graphics.Vty                   ( withForeColor )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events
import           Lens.Micro.Platform
import           Types.AppState
import           Types.Brick.CustomEvent
import           Types.Brick.Name
import           Types.Classes.Displayable      ( display )
import           Types.Constants                ( mainSettingsFile )
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           UI.Attr
import           UI.List                        ( renderGenericList )
import           UI.Modal                       ( dismissModal
                                                , handleConfirm
                                                , renderModal
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

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw         = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent  = handleEvent
            , appStartEvent   = startEvent
            , appAttrMap      = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI s@AppState { _projects, _activeScreen, _modal } =
  let
    mainWidget = case _activeScreen of
      HelpScreen                  -> txt "Todo"
      ProjectAddScreen  form      -> renderForm form
      ProjectListScreen list      -> renderGenericList list
      ProjectEditScreen    _ form -> renderForm form
      ProjectDetailsScreen _ list -> renderGenericList list
      RequestAddScreen     _ form -> renderForm form
      RequestDetailsScreen c ->
        let r = lookupRequestDefinition s c in txt $ display r
      RequestEditScreen _ form -> renderForm form
    helpText = case _activeScreen of
      ProjectAddScreen{}  -> "Enter: Finish adding | ESC: Return without adding"
      ProjectListScreen{} -> "Enter: View project | a: Add Project"
      ProjectDetailsScreen{}
        -> "Enter: View request definition | Left: back | e: Edit Project | a: Add request definition | d: Delete"
      RequestDetailsScreen{} -> "Left: back | e: Edit | d: Delete"
      ProjectEditScreen{}    -> "Enter: Save | ESC: Return without saving"
      RequestAddScreen{} -> "Enter: Finsh adding | ESC: Return without adding"
      RequestEditScreen{}    -> "Enter: Save | ESC: Return without saving"
      HelpScreen             -> "todo"
    titleLine = txt $ title s _activeScreen
    helpLine  = txt helpText
    everything =
      titleLine
        <=> hBorder
        <=> padBottom Max mainWidget
        <=> hBorder
        <=> helpLine
    borderedEverything =
      withBorderStyle unicodeRounded $ (joinBorders . border) everything
    modalWidget = maybeToList $ renderModal s <$> _modal
  in
    modalWidget ++ [borderedEverything]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent
  :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s

handleEvent s@AppState { _modal = Just m } (VtyEvent (EvKey key [])) =
  case key of
    KChar 'n' -> continue $ dismissModal s
    KChar 'y' -> continue $ dismissModal $ handleConfirm s m
    _         -> continue s

handleEvent s@AppState { _activeScreen, _projects } ev@(VtyEvent (EvKey key []))
  = case _activeScreen of
    RequestDetailsScreen c@(RequestDefinitionContext pid _) -> case key of
      KLeft     -> continue $ showProjectDetails s (ProjectContext pid)
      KChar 'e' -> continue $ showEditRequestDefinitionScreen s c
      KChar 'd' -> continue $ (modal ?~ DeleteRequestDefinitionModal c) s
      _         -> continue s

    RequestEditScreen c form -> case key of
      KEnter -> continue $ finishEditingRequestDefinition s c (formState form)
      KEsc   -> continue $ showRequestDefinitionDetails s c
      _      -> handleFormEvent ev form
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
        >>= \l -> continue $ (activeScreen .~ ProjectDetailsScreen c l) s

    ProjectEditScreen c form -> case key of
      KEnter -> continue $ finishEditingProject s c (formState form)
      KEsc   -> continue $ showProjectDetails s c
      _ ->
        handleFormEvent ev form >>= \f -> continue $ updateEditProjectForm s c f

    ProjectAddScreen form -> case key of
      KEnter -> liftIO (finishAddingProject s (formState form)) >>= continue
      KEsc   -> continue $ showProjectListScreen s
      _ ->
        handleFormEvent ev form >>= \f -> continue $ updateProjectAddForm s f

    RequestAddScreen c form -> case key of
      KEnter ->
        liftIO (finishAddingRequestDefinition s c (formState form)) >>= continue
      KEsc -> continue $ showProjectDetails s c
      _    -> handleFormEvent ev form
        >>= \f -> continue $ updateAddRequestDefinitionForm s c f

    HelpScreen -> continue s

handleEvent s _ = continue s

startEvent :: AppState -> EventM Name AppState
startEvent = return

myMap :: AttrMap
myMap = attrMap
  V.defAttr
  [ (listSelectedFocusedAttr, withForeColor (Brick.Util.bg V.green) V.black)
  , (highlighted            , Brick.Util.bg V.blue)
  ]

saveState :: AppState -> IO ()
saveState s = writeFile mainSettingsFile (encodePretty s)
