{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}

module UI.App where

import           Prelude                    hiding (writeFile)

import           Brick                      (App (..), AttrMap,
                                             BrickEvent (VtyEvent), BrickEvent,
                                             BrickEvent, CursorLocation, EventM,
                                             Next, Widget, attrMap, continue,
                                             halt, joinBorders, padBottom, txt,
                                             withBorderStyle, (<=>))
import           Brick.Forms                (formState, handleFormEvent,
                                             renderForm)
import           Brick.Types                (Padding (Max))
import           Brick.Util
import           Brick.Widgets.Border       (border, hBorder)
import           Brick.Widgets.Border.Style (unicodeRounded)
import           Brick.Widgets.List         (handleListEvent,
                                             listSelectedElement,
                                             listSelectedFocusedAttr)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString.Lazy       (writeFile)
import qualified Data.Map.Strict            as Map
import           Graphics.Vty               (withForeColor)
import qualified Graphics.Vty               as V
import           Graphics.Vty.Input.Events
import           Lens.Micro.Platform
import           Types.Classes.Addable              (finishAdding, makeAddForm,
                                             updateAddForm, NoContext(..))
import           Types.AppState
import           Types.Constants            (mainSettingsFile)
import           Types.Brick.CustomEvent
import           Types.Classes.Displayable          (display)
import           Types.Classes.Editable             (finishEditing, showEditScreen,
                                             updateEditForm)
import           Types.Brick.Name
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           UI.Attr
import           UI.List                    (renderGenericList)
import           UI.Projects.List           (makeProjectList)
import           Types.Classes.ShowDetails             (showDetails)

uiApp :: App AppState CustomEvent Name
uiApp = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = startEvent
  , appAttrMap      = const myMap
  }

drawUI :: AppState -> [Widget Name]
drawUI s@AppState { _projects, _activeScreen } =
  let
    mainWidget = case _activeScreen of
      HelpScreen                  -> txt "Todo"
      ProjectAddScreen  form      -> renderForm form
      ProjectListScreen list      -> renderGenericList list
      ProjectEditScreen    _ form -> renderForm form
      ProjectDetailsScreen _ list -> renderGenericList list
      RequestAddScreen _ form -> renderForm form
      RequestDetailsScreen c ->
        let r = lookupRequestDefinition s c in txt $ display r
      RequestEditScreen _ form -> renderForm form
    helpText = case _activeScreen of
      ProjectAddScreen{} -> "Enter: Finish adding | ESC: Return without adding"
      ProjectListScreen{} -> "Enter: View project | a: Add Project"
      ProjectDetailsScreen{} ->
        "Enter: View request definition | Left: back | e: Edit Project | a: Add request definition"
      RequestDetailsScreen{} -> "Left: back | e: Edit request definition"
      ProjectEditScreen{}    -> "Enter: Save | ESC: Return without saving"
      RequestAddScreen{}     -> "Enter: Finsh adding | ESC: Return without adding"
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
  in [withBorderStyle unicodeRounded $ (joinBorders . border) everything]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent
  :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s

handleEvent s@AppState { _activeScreen, _projects } ev@(VtyEvent (EvKey key []))
  = case _activeScreen of
    RequestDetailsScreen context@(RequestDefinitionContext pid _) ->
      case key of
        KLeft     -> continue $ showDetails s (ProjectContext pid)
        KChar 'e' -> continue $ showEditScreen s context
        _         -> continue s

    RequestEditScreen c form -> case key of
      KEnter -> continue $ finishEditing s c (formState form)
      KEsc -> continue $ showDetails s c
      _ -> handleFormEvent ev form >>= \f -> continue $ updateEditForm s c f

    ProjectListScreen list -> case key of
      KEnter -> case listSelectedElement list of
        Just (_, ProjectListItem context _) -> continue $ showDetails s context
        Nothing                             -> continue s
      KChar 'a' -> continue $ (activeScreen .~ ProjectAddScreen makeAddForm) s
      _         -> handleListEvent (EvKey key []) list
        >>= \l -> continue $ (activeScreen .~ ProjectListScreen l) s

    ProjectDetailsScreen c list -> case key of
      KEnter -> case listSelectedElement list of
        Just (_, RequestDefinitionListItem reqContext _) ->
          continue $ showDetails s reqContext
        Nothing -> continue s
      KChar 'e' -> continue $ showEditScreen s c
      KChar 'a' -> continue $ (activeScreen .~ RequestAddScreen c makeAddForm) s
      KLeft ->
        let projectList = makeProjectList (Map.toList _projects)
        in continue $ (activeScreen .~ ProjectListScreen projectList) s
      _ -> handleListEvent (EvKey key []) list
        >>= \l -> continue $ (activeScreen .~ ProjectDetailsScreen c l) s

    ProjectEditScreen c form -> case key of
      KEnter -> continue $ finishEditing s c (formState form)
      KEsc -> continue $ showDetails s c
      _ -> handleFormEvent ev form >>= \f -> continue $ updateEditForm s c f

    ProjectAddScreen form -> case key of
      KEnter -> liftIO (finishAdding s NoContext (formState form)) >>= continue
      KEsc -> continue $ (activeScreen .~ ProjectListScreen (makeProjectList (Map.toList _projects))) s
      _      -> handleFormEvent ev form >>= \f -> continue $ updateAddForm s NoContext f

    RequestAddScreen c form -> case key of
      KEnter -> liftIO (finishAdding s c (formState form)) >>= continue
      KEsc -> continue $ showDetails s c
      _ -> handleFormEvent ev form >>= \f -> continue $ updateAddForm s c f

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