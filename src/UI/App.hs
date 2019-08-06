{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.App where

import           Prelude                 hiding ( writeFile )

import           Brick                          ( BrickEvent(VtyEvent)
                                                , attrMap
                                                , AttrMap
                                                , Next
                                                , EventM
                                                , BrickEvent
                                                , CursorLocation
                                                , Widget
                                                , App(..)
                                                , continue
                                                , halt
                                                , txt
                                                , (<=>)
                                                , withBorderStyle
                                                , joinBorders
                                                , padBottom
                                                , BrickEvent
                                                )
import           Brick.Forms                    ( renderForm
                                                , handleFormEvent
                                                , formState
                                                )
import           Brick.Util
import           Brick.Widgets.List             ( handleListEvent
                                                , listSelectedElement
                                                , listSelectedFocusedAttr
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( writeFile )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events
import           Lens.Micro.Platform
import           Types.AppState
import           Types.Constants                ( mainSettingsFile )
import           Types.CustomEvent
import           Types.Name
import           Types.Screen
import           UI.Attr
import           UI.EventHandlers.ActiveForm    ( finishEditing
                                                , showEditScreen
                                                , Editable
                                                , EditState
                                                , updateForm
                                                )
import           UI.ShowDetails                 ( showDetails
                                                , ShowDetails
                                                )
import           UI.HelpScreen
import           UI.List                        ( renderGenericList
                                                , ZZZList
                                                )
import           Types.Displayable              ( display )

--import           Debug.Trace
import           Brick.Widgets.Border           ( border
                                                , hBorder
                                                )
import           Brick.Widgets.Border.Style     ( unicodeRounded )
import           Brick.Types                    ( Padding(Max) )
import           Types.Project
import           Types.RequestDefinition
import           Graphics.Vty                   ( withForeColor )
import qualified Data.Map.Strict               as Map
import           UI.Projects.List               ( makeProjectList )
import           UI.Form                        ( ZZZForm )
import           Types.WithID
import           UI.EventHandlers.ActiveList    ( Listable
                                                , ListItem
                                                )

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw         = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent  = handleEvent
            , appStartEvent   = startEvent
            , appAttrMap      = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI s@AppState { _projects, _activeScreen } =
  let
    mainWidget = case _activeScreen of
      HelpScreen                  -> helpWidget
      ProjectAddScreen  form      -> renderForm form
      ProjectListScreen list      -> renderGenericList list
      ProjectEditScreen    _ form -> renderForm form
      ProjectDetailsScreen _ list -> renderGenericList list
      RequestDetailsScreen c ->
        let r = lookupRequestDefinition s c in txt $ display r
      RequestEditScreen _ form -> renderForm form
    helpText = case _activeScreen of
      ProjectListScreen{} -> "Enter: View project"
      ProjectDetailsScreen{} ->
        "Enter: View request definition | Left: back | e: Edit Project"
      RequestDetailsScreen{} -> "Left: back | e: Edit request definition"
      ProjectEditScreen{}    -> "Enter: Save | ESC: Return without saving"
      RequestEditScreen{}    -> "Enter: Save | ESC: Return without saving"
      _                      -> "todo"
    titleLine = txt $ title s _activeScreen
    helpLine  = txt helpText
    everything =
      titleLine
        <=> hBorder
        <=> padBottom Max mainWidget
        <=> hBorder
        <=> helpLine
  in
    [withBorderStyle unicodeRounded $ (joinBorders . border) everything]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

interceptFormEvent
  :: Editable a
  => AppState
  -> Context a
  -> BrickEvent Name CustomEvent
  -> ZZZForm (EditState a)
  -> EventM Name (Next AppState)
interceptFormEvent s c ev form =
  handleFormEvent ev form >>= \f -> continue $ updateForm s c f

interceptListEvent
  :: (ShowDetails a, Listable a)
  => AppState
  -> Key
  -> ZZZList (ListItem a)
  -> (ZZZList (ListItem a) -> Screen)
  -> EventM Name (Next AppState)
interceptListEvent s key list screenMaker = handleListEvent (EvKey key []) list
  >>= \l -> continue $ (activeScreen .~ screenMaker l) s

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

    RequestEditScreen context form -> case key of
      KEnter -> continue $ finishEditing s context (formState form)
      KEsc   -> continue $ showDetails s context
      _      -> interceptFormEvent s context ev form

    ProjectListScreen list -> case key of
      KEnter -> case listSelectedElement list of
        Just (_, ProjectListItem context _) -> continue $ showDetails s context
        Nothing                             -> continue s
      _ -> interceptListEvent s key list ProjectListScreen

    ProjectDetailsScreen c list -> case key of
      KEnter -> case listSelectedElement list of
        Just (_, RequestDefinitionListItem reqContext _) ->
          continue $ showDetails s reqContext
        Nothing -> continue s
      KChar 'e' -> continue $ showEditScreen s c
      KLeft ->
        let projectList = makeProjectList (Map.elems _projects)
        in  continue $ (activeScreen .~ ProjectListScreen projectList) s
      _ -> interceptListEvent s key list (ProjectDetailsScreen c)

    ProjectEditScreen context form -> case key of
      KEnter -> continue $ finishEditing s context (formState form)
      KEsc   -> continue $ showDetails s context
      _      -> interceptFormEvent s context ev form

    _ -> continue s

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
