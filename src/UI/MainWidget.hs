{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
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
import           UI.List                        ( renderGenericList )
import           UI.RequestDefs.Details         ( requestDefDetailsWidget )
import           UI.Responses.Details           ( responseDetails )

formHelpText :: Widget Name
formHelpText =
  txtWrap "Use Tab and Shift-Tab to navigate between fields. Press Enter to save your changes."

-- shortcut for padding a form widget one on top and 2 on the left
padForm :: Widget Name -> Widget Name
padForm = padTop (Pad 1) . padLeft (Pad 2)

mainWidget :: AnyAppState -> Widget Name
mainWidget (AnyAppState s) = case s ^. screen of
  HelpScreen            -> txt "Todo"
  ProjectAddScreen form -> renderForm form
  ProjectListScreen list ->
    txtWrap "Select a project to view its details and request definitions."
      <=> padTop (Pad 1) (renderGenericList True list)
  ProjectEditScreen _ form -> formHelpText <=> padForm (renderForm form)
  ProjectDetailsScreen _ list ->
    txtWrap "Select a request definition to view its details and send a request."
      <=> padTop (Pad 1) (renderGenericList True list)
  RequestDefAddScreen _ form -> renderForm form
  RequestDefDetailsScreen c list ring ->
    let historyListFocused = focusGetCurrent ring == Just ResponseList
        responseWidget     = case listSelectedElement list of
          Just (_, r) -> responseDetails r (not historyListFocused)
          Nothing     -> txt "No response selected."
    in 
      -- TODO: depending on how wide the screen is, place response widget on right side?
        padLeft (Pad 2) (requestDefDetailsWidget s c)
          <=> hBorder
          <=> vLimit 10 (renderGenericList historyListFocused list)
          <=> hBorder
          <=> responseWidget
  RequestDefEditScreen _ form -> formHelpText <=> padForm (renderForm form)
