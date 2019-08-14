{-# LANGUAGE OverloadedStrings #-}

module UI.MainWidget
  ( mainWidget
  )
where

import           Brick                          ( Widget
                                                , padLeft
                                                , padTop
                                                , txt
                                                , txtWrap
                                                , vLimit
                                                , (<=>)
                                                )
import           Brick.Focus                    ( focusGetCurrent )
import           Brick.Forms                    ( renderForm )
import           Brick.Types                    ( Padding(Pad) )
import           Brick.Widgets.Border           ( hBorder )
import           Brick.Widgets.List             ( listSelectedElement )
import           Control.Lens
import           Types.AppState
import           Types.Brick.Name               ( Name(..) )
import           Types.Models.Screen
import           UI.Console                     ( console )
import           UI.List                        ( renderGenericList )
import           UI.RequestDefinitions.Details  ( requestDefinitionDetailsWidget
                                                )
import           UI.Responses.Details           ( responseBodyWidget )

formHelpText :: Widget Name
formHelpText =
  txtWrap
    "Use Tab and Shift-Tab to navigate between fields. Press Enter to save your changes."

-- shortcut for padding a form widget one on top and 2 on the left
padForm :: Widget Name -> Widget Name
padForm = padTop (Pad 1) . padLeft (Pad 2)

mainWidget :: AppState -> Widget Name
mainWidget s = case s ^. screen of
  HelpScreen            -> txt "Todo"
  ProjectAddScreen form -> renderForm form
  ProjectListScreen list ->
    txtWrap "Select a project to view its details and request definitions."
      <=> padTop (Pad 1) (renderGenericList True list)
  ProjectEditScreen _ form -> formHelpText <=> padForm (renderForm form)
  ProjectDetailsScreen _ list ->
    txtWrap
        "Select a request definition to view its details and send a request."
      <=> padTop (Pad 1) (renderGenericList True list)
  RequestAddScreen _ form -> renderForm form
  RequestDetailsScreen c list ring ->
    let historyListFocused = focusGetCurrent ring == Just ResponseList
        responseWidget     = case listSelectedElement list of
          Just (_, r) -> responseBodyWidget r (not historyListFocused)
          Nothing     -> txt "No response selected."
    in 
      -- TODO: depending on how wide the screen is, place response widget on right side?
        padLeft (Pad 2) (requestDefinitionDetailsWidget s c)
          <=> hBorder
          <=> vLimit 10 (renderGenericList historyListFocused list)
          <=> hBorder
          <=> responseWidget
  RequestEditScreen _ form -> formHelpText <=> padForm (renderForm form)
  ConsoleScreen            -> console (s ^. messages)
