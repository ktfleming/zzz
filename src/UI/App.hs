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
                                                , getEventHandler
                                                )
import           Types.Name
import           Types.Screen
import           UI.Attr
import           UI.HelpScreen
import           UI.List                        ( renderGenericList )
import           UI.Projects.Details            ( projectDetailsWidget )

import           Debug.Trace

uiApp :: App AppState CustomEvent Name
uiApp = App { appDraw         = drawUI
            , appChooseCursor = chooseCursor
            , appHandleEvent  = handleEvent
            , appStartEvent   = startEvent
            , appAttrMap      = const myMap
            }

drawUI :: AppState -> [Widget Name]
drawUI s@AppState { _activeScreen } = case getEventHandler s of
  FormHandler (ActiveForm form  ) -> [renderForm form]
  ListHandler (ActiveList _ list) -> [renderGenericList list]
  NoHandler                       -> case _activeScreen of
    ProjectListScreen      -> []
    HelpScreen             -> [helpWidget]
    ProjectDetailsScreen p -> [projectDetailsWidget p]


chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent
  :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- Ctrl-C always exits immediately
handleEvent s (VtyEvent (EvKey KEsc [])) = trace (show s) $ continue s -- For debugging
handleEvent s (VtyEvent (EvKey (KChar 's') [MCtrl])) =
  liftIO (saveState s) >> continue s

-- When a form is active, use `handleFormEvent` to send events to the form, unless the Enter key is pressed, in which case
-- we activate the form's submit handler
-- handleEvent s@AppState { _eventHandler = FormHandler (ActiveForm form) } ev =
--   case ev of
--     VtyEvent (EvKey KEnter []) -> continue $ handleSubmit s form
--     _ ->
--       -- Use handleFormEvent to update the form (reflect text entered, control focused, etc)
--       -- Then we need to bind over the new form, using a lens to update the `_activeForm` field
--       -- in our AppState in order to get the new AppState (so it will be rendered properly in `drawUI`),
--       -- then finally `continue` with the updated AppState
--       let newForm :: EventM Name EventHandler =
--               fmap (FormHandler . ActiveForm) (handleFormEvent ev form)
--           mapper :: EventHandler -> EventM Name (Next AppState)
--           mapper _form = continue $ eventHandler .~ _form $ s
--       in  newForm >>= mapper

-- -- If a list is active, delete events with `handleListEvent`
-- handleEvent s@AppState { _eventHandler = ListHandler (ActiveList selectHandler list) } ev
--   = case ev of
--     VtyEvent (EvKey KEnter []) -> case listSelectedElement list of
--       Just (_, selected) -> continue $ selectHandler s selected
--       Nothing            -> continue s
--     VtyEvent vtyEvent ->
--       let newList :: EventM Name EventHandler = fmap
--             (ListHandler . ActiveList selectHandler)
--             (handleListEvent vtyEvent list)
--           mapper :: EventHandler -> EventM Name (Next AppState)
--           mapper _list = continue $ eventHandler .~ _list $ s
--       in  newList >>= mapper
--     _ -> continue s -- non-vty events won't affect the list
-- handleEvent s (VtyEvent (EvKey (KChar 'p') [])) =
--   continue $ activeScreen .~ ProjectListScreen $ s
-- handleEvent s (VtyEvent (EvKey (KChar 'h') [])) =
--   continue $ activeScreen .~ HelpScreen $ s
-- handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s  -- 'q' to quit
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
