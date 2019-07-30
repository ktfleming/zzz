{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

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
                                                )
import           Brick.Forms                    ( renderForm
                                                , handleFormEvent
                                                , formState
                                                , Form
                                                )
import           Brick.Util
import           Brick.Widgets.List             ( handleListEvent
                                                , listSelectedElement
                                                , listSelectedFocusedAttr
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( ByteString
                                                , writeFile
                                                )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Input.Events
import           Lens.Micro.Platform
import           Types.AppState
import           Types.Constants                ( mainSettingsFile )
import           Types.CustomEvent
import           Types.EventHandler             ( EventHandler(..)
                                                , ActiveForm(..)
                                                , ActiveList(..)
                                                , handleSubmit
                                                , projectListSelectHandler
                                                )
import           Types.Name
import           Types.Screen
import           UI.Attr
import           UI.HelpScreen
import           UI.List                        ( renderGenericList )
import           UI.Projects.Add                ( mkForm
                                                , ProjectAddState
                                                )
import           UI.Projects.Details            ( projectDetailsWidget )
import           UI.Projects.List               ( makeProjectList )

import           Debug.Trace

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw         = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent  = handleEvent
            , appStartEvent   = startEvent
            , appAttrMap      = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI AppState { _allProjects, _activeScreen } = case _activeScreen of
  ProjectDetailsScreen p                   -> [projectDetailsWidget p]
  HelpScreen                               -> [helpWidget]
  ProjectListScreen (AddingProject   form) -> [renderForm form]
  ProjectListScreen (ListingProjects list) -> [renderGenericList $ list]


chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent
  :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey KEsc [])) = trace (show s) $ continue s -- For debugging
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s

handleEvent s@AppState { _activeScreen } ev = case _activeScreen of

  -- "Add project" form is showing: delegate to handleFormEvent, then put the updated form back in the state
  ProjectListScreen (AddingProject form) -> handleFormEvent ev form >>= \f ->
    continue $ (activeScreen .~ ProjectListScreen (AddingProject f)) s

  -- Project list is showing: delegate to handleListEvent, then put the updated list back in the state
  -- ...unless the ENTER key is pressed, which case run the appropriate select handler
  ProjectListScreen (ListingProjects list) -> case ev of
    VtyEvent (EvKey KEnter []) -> case listSelectedElement list of
      Just (_, selected) -> continue $ projectListSelectHandler s selected
      Nothing            -> continue s
    VtyEvent vtyEvent -> handleListEvent vtyEvent list >>= \l ->
      continue $ (activeScreen .~ ProjectListScreen (ListingProjects l)) s
    _ -> continue s -- non-vty events won't affect the list
  _ -> continue s

startEvent :: AppState -> EventM Name AppState
startEvent = return

myMap :: AttrMap
myMap = attrMap
  V.defAttr
  [ (listSelectedFocusedAttr, Brick.Util.bg V.green)
  , (highlighted            , Brick.Util.bg V.blue)
  ]

saveState :: AppState -> IO ()
saveState s =
  let jsonified :: ByteString = encodePretty s
  in  writeFile mainSettingsFile jsonified
