{-# LANGUAGE ScopedTypeVariables #-}

module UI.App where

import           Prelude                 hiding ( writeFile )

import           Brick                          ( BrickEvent(VtyEvent)
                                                , attrMap
                                                , AttrMap
                                                , Next
                                                , EventM
                                                , BrickEvent
                                                , CursorLocation
                                                , (<+>)
                                                , str
                                                , withBorderStyle
                                                , Widget
                                                , App(..)
                                                , continue
                                                , halt
                                                )
import           Brick.Forms
import           Brick.Types                    ( Next )
import           Brick.Util
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center           ( center )
import           Brick.Widgets.List             ( handleListEvent
                                                , renderList
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
import           Types.Name
import           Types.Screen
import           UI.Attr
import           UI.HelpScreen
import           UI.List                        ( renderGenericList )
import           UI.Projects.Add

import           Debug.Trace

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw         = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent  = handleEvent
            , appStartEvent   = startEvent
            , appAttrMap      = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI AppState { _eventHandler = FormHandler (ActiveForm form) } =
  [renderForm form]
drawUI AppState { _eventHandler = ListHandler (ActiveList list) } =
  [renderGenericList list]
drawUI s = case _activeScreen s of
  -- ProjectListScreen -> [renderProjectList $ _allProjects s]
  HelpScreen    -> [helpWidget]
  ProjectScreen -> [helpWidget] -- TODO: this is just temporary
  _             -> [str "something went wrong!"]
-- drawUI s = [withBorderStyle unicode $ borderWithLabel (str "Hello!") $ (center (str "Left") <+> vBorder <+> center (str "Right"))]

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent
  :: forall a
   . AppState
  -> BrickEvent Name CustomEvent
  -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey KEsc [])) = trace (show s) $ continue s -- For debugging
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s

-- When a form is active, use `handleFormEvent` to send events to the form, unless the Enter key is pressed, in which case
-- we activate the form's submit handler
handleEvent s@AppState { _eventHandler = FormHandler (ActiveForm form) } ev =
  case ev of
    VtyEvent (EvKey KEnter []) -> continue $ handleSubmit s form
    _ ->
      -- Use handleFormEvent to update the form (reflect text entered, control focused, etc)
      -- Then we need to bind over the new form, using a lens to update the `_activeForm` field
      -- in our AppState in order to get the new AppState (so it will be rendered properly in `drawUI`),
      -- then finally `continue` with the updated AppState
      let newForm :: EventM Name EventHandler =
              fmap (FormHandler . ActiveForm) (handleFormEvent ev form)
          mapper :: EventHandler -> EventM Name (Next AppState)
          mapper form = continue $ eventHandler .~ form $ s
      in  newForm >>= mapper

-- If a list is active, delete events with `handleListEvent`
handleEvent s@AppState { _eventHandler = ListHandler (ActiveList list) } ev =
  case ev of
    VtyEvent (EvKey KEnter []) -> continue s -- TODO: select item
    VtyEvent vtyEvent ->
      let newList :: EventM Name EventHandler =
              fmap (ListHandler . ActiveList) (handleListEvent vtyEvent list)
          mapper :: EventHandler -> EventM Name (Next AppState)
          mapper list = continue $ eventHandler .~ list $ s
      in  newList >>= mapper
    _ -> continue s -- non-vty events won't affect the list
handleEvent s (VtyEvent (EvKey (KChar 'p') [])) =
  continue $ activeScreen .~ ProjectListScreen $ s
handleEvent s (VtyEvent (EvKey (KChar 'h') [])) =
  continue $ activeScreen .~ HelpScreen $ s
handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s  -- 'q' to quit
handleEvent s _ = continue s

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
